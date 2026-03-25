import argparse
import json
import logging
import multiprocessing as mp
import os
import time
from pathlib import Path

from flask import Flask, abort, jsonify, make_response, send_from_directory
from openslide import OpenSlide
from openslide.deepzoom import DeepZoomGenerator

logging.getLogger("werkzeug").disabled = True

# Speed-oriented defaults
TILE_SIZE = 512
OVERLAP = 0
LIMIT_BOUNDS = False
JPEG_QUALITY = 75

APP = Flask(__name__)

STATUS_FILE = "status_state.json"
FAILED_FILE = "failed_tiles.json"

HEARTBEAT_INTERVAL_SEC = 1.0
DEFAULT_STALL_SEC = 90
DEFAULT_RESTART_LIMIT = 4
DEFAULT_BATCH_ROWS = 8

# Per-process globals for pool workers
_SLIDE = None
_DZ = None


def now_ts():
    return time.time()


def atomic_write_text(path: Path, text: str):
    tmp = path.with_suffix(path.suffix + ".tmp")
    tmp.write_text(text, encoding="utf-8")
    os.replace(tmp, path)


def atomic_write_json(path: Path, payload: dict):
    atomic_write_text(path, json.dumps(payload, indent=2, default=str))


def read_json(path: Path, default=None):
    if default is None:
        default = {}
    try:
        if path.exists():
            return json.loads(path.read_text(encoding="utf-8"))
    except Exception:
        pass
    return default


def update_json(path: Path, **kwargs):
    data = read_json(path, {})
    data.update(kwargs)
    atomic_write_json(path, data)
    return data


def append_failed_tile(path: Path, entry: dict):
    data = read_json(path, {"failed_tiles": []})
    if "failed_tiles" not in data or not isinstance(data["failed_tiles"], list):
        data["failed_tiles"] = []
    data["failed_tiles"].append(entry)
    atomic_write_json(path, data)


def add_cors(resp):
    resp.headers["Access-Control-Allow-Origin"] = "*"
    resp.headers["Access-Control-Allow-Methods"] = "GET, OPTIONS"
    resp.headers["Access-Control-Allow-Headers"] = "Content-Type"
    return resp


def parse_args():
    p = argparse.ArgumentParser()
    p.add_argument("--slide", required=True)
    p.add_argument("--output-dir", required=True)
    p.add_argument("--host", default="127.0.0.1")
    p.add_argument("--port", type=int, default=5001)
    p.add_argument("--workers", type=int, default=1)
    p.add_argument("--stall-sec", type=int, default=DEFAULT_STALL_SEC)
    p.add_argument("--restart-limit", type=int, default=DEFAULT_RESTART_LIMIT)
    p.add_argument("--max-tile-retries", type=int, default=2)  # kept for compatibility
    p.add_argument("--skip-failed-tiles", action="store_true")
    p.add_argument("--batch-rows", type=int, default=DEFAULT_BATCH_ROWS)
    return p.parse_args()


def initial_status(output_dir: str, slide_path: str, workers: int, stall_sec: int, restart_limit: int):
    return {
        "phase": "booting",
        "ready": False,
        "error": None,
        "traceback": None,
        "output_dir": output_dir,
        "slide_path": slide_path,
        "workers": int(workers),
        "processed_tiles": 0,
        "total_tiles": 0,
        "failed_tiles": 0,
        "skipped_tiles": 0,
        "restart_count": 0,
        "stall_sec": int(stall_sec),
        "restart_limit": int(restart_limit),
        "started_ts": now_ts(),
        "finished_ts": None,
        "heartbeat_ts": now_ts(),
        "last_progress_ts": None,
        "current_level": None,
        "current_col": None,
        "current_row": None,
        "last_tile_path": None,
        "last_action": "booting",
        "worker_alive": False,
        "worker_pid": None,
        "dzi_written": False,
    }


def build_metadata(slide_path: str):
    slide = OpenSlide(slide_path)
    try:
        dz = DeepZoomGenerator(
            slide,
            tile_size=TILE_SIZE,
            overlap=OVERLAP,
            limit_bounds=LIMIT_BOUNDS,
        )
        total = 0
        levels = []
        for level in range(dz.level_count):
            cols, rows = dz.level_tiles[level]
            total += cols * rows
            levels.append({"level": level, "cols": cols, "rows": rows})
        dzi_text = dz.get_dzi("jpeg")
        return {"total_tiles": total, "levels": levels, "dzi_text": dzi_text}
    finally:
        slide.close()


def count_existing_tiles(output_dir: Path):
    slide_files = output_dir / "slide_files"
    if not slide_files.exists():
        return 0
    return sum(1 for _ in slide_files.rglob("*.jpeg"))


def build_jobs(levels, batch_rows):
    jobs = []
    for level_info in levels:
        level = int(level_info["level"])
        cols = int(level_info["cols"])
        rows = int(level_info["rows"])
        for row_start in range(0, rows, batch_rows):
            row_end = min(rows, row_start + batch_rows)
            jobs.append({
                "level": level,
                "cols": cols,
                "row_start": row_start,
                "row_end": row_end,
            })
    return jobs


def init_pool_worker(slide_path: str):
    global _SLIDE, _DZ
    _SLIDE = OpenSlide(slide_path)
    _DZ = DeepZoomGenerator(
        _SLIDE,
        tile_size=TILE_SIZE,
        overlap=OVERLAP,
        limit_bounds=LIMIT_BOUNDS,
    )


def close_pool_worker():
    global _SLIDE
    if _SLIDE is not None:
        try:
            _SLIDE.close()
        except Exception:
            pass


def process_job(args):
    global _DZ
    job, output_dir_str, skip_failed_tiles = args
    output_dir = Path(output_dir_str)

    level = int(job["level"])
    cols = int(job["cols"])
    row_start = int(job["row_start"])
    row_end = int(job["row_end"])

    level_dir = output_dir / "slide_files" / str(level)
    level_dir.mkdir(parents=True, exist_ok=True)

    processed = 0
    failed = 0
    skipped = 0
    last_col = 0
    last_row = row_start
    failures = []

    for row in range(row_start, row_end):
        for col in range(cols):
            last_col = col
            last_row = row

            tile_path = level_dir / f"{col}_{row}.jpeg"
            if tile_path.exists():
                processed += 1
                continue

            try:
                tile = _DZ.get_tile(level, (col, row))
                tmp_path = tile_path.with_suffix(tile_path.suffix + ".tmp")
                tile.save(tmp_path, "JPEG", quality=JPEG_QUALITY)
                os.replace(tmp_path, tile_path)
                processed += 1
            except Exception as e:
                failed += 1
                failures.append({
                    "ts": now_ts(),
                    "level": level,
                    "col": col,
                    "row": row,
                    "tile_path": str(tile_path),
                    "error": f"{type(e).__name__}: {e}",
                })
                if skip_failed_tiles:
                    skipped += 1
                    processed += 1
                else:
                    raise

    return {
        "level": level,
        "row_start": row_start,
        "row_end": row_end,
        "processed": processed,
        "failed": failed,
        "skipped": skipped,
        "last_col": last_col,
        "last_row": last_row,
        "failures": failures,
    }


def coordinator_preprocess(
    slide_path: str,
    output_dir: str,
    status_path: str,
    failed_path: str,
    requested_workers: int,
    batch_rows: int,
    skip_failed_tiles: bool,
):
    output_dir = Path(output_dir)
    status_path = Path(status_path)
    failed_path = Path(failed_path)

    try:
        update_json(
            status_path,
            phase="opening_slide",
            worker_alive=True,
            worker_pid=os.getpid(),
            heartbeat_ts=now_ts(),
            last_progress_ts=now_ts(),
            last_action="building metadata",
            error=None,
            traceback=None,
        )

        meta = build_metadata(slide_path)
        levels = meta["levels"]
        total_tiles = int(meta["total_tiles"])

        (output_dir / "slide_files").mkdir(parents=True, exist_ok=True)
        if not (output_dir / "slide.dzi").exists():
            (output_dir / "slide.dzi").write_text(meta["dzi_text"], encoding="utf-8")

        processed_tiles = count_existing_tiles(output_dir)
        failed_tiles = int(read_json(status_path, {}).get("failed_tiles", 0))
        skipped_tiles = int(read_json(status_path, {}).get("skipped_tiles", 0))

        update_json(
            status_path,
            phase="preprocessing",
            total_tiles=total_tiles,
            processed_tiles=processed_tiles,
            failed_tiles=failed_tiles,
            skipped_tiles=skipped_tiles,
            dzi_written=True,
            heartbeat_ts=now_ts(),
            last_progress_ts=now_ts(),
            last_action="creating jobs",
        )

        jobs = build_jobs(levels, max(1, int(batch_rows)))

        num_workers = max(1, min(int(requested_workers), 10))
        update_json(
            status_path,
            workers=num_workers,
            last_action=f"starting pool with {num_workers} workers",
        )

        pool = mp.Pool(
            processes=num_workers,
            initializer=init_pool_worker,
            initargs=(slide_path,),
            maxtasksperchild=16,
        )

        try:
            pending = []
            for job in jobs:
                res = pool.apply_async(process_job, args=((job, str(output_dir), bool(skip_failed_tiles)),))
                pending.append((job, res))

            last_heartbeat = 0.0

            while pending:
                t_now = now_ts()
                if t_now - last_heartbeat >= HEARTBEAT_INTERVAL_SEC:
                    update_json(
                        status_path,
                        heartbeat_ts=t_now,
                        worker_alive=True,
                        worker_pid=os.getpid(),
                        last_action="waiting for batch results",
                    )
                    last_heartbeat = t_now

                next_pending = []
                progress_made = False

                for job, res in pending:
                    if not res.ready():
                        next_pending.append((job, res))
                        continue

                    progress_made = True
                    out = res.get()

                    processed_tiles += int(out["processed"])
                    failed_tiles += int(out["failed"])
                    skipped_tiles += int(out["skipped"])

                    for entry in out["failures"]:
                        append_failed_tile(failed_path, entry)

                    update_json(
                        status_path,
                        processed_tiles=processed_tiles,
                        failed_tiles=failed_tiles,
                        skipped_tiles=skipped_tiles,
                        heartbeat_ts=now_ts(),
                        last_progress_ts=now_ts(),
                        current_level=int(out["level"]),
                        current_col=int(out["last_col"]),
                        current_row=int(out["last_row"]),
                        last_action=f"batch_done rows {out['row_start']}:{out['row_end']}",
                    )

                pending = next_pending

                if not progress_made:
                    time.sleep(0.2)

        finally:
            try:
                pool.close()
            except Exception:
                pass
            try:
                pool.terminate()
            except Exception:
                pass
            try:
                pool.join()
            except Exception:
                pass

        # Recount once at end to avoid drift after restarts/skips
        processed_tiles = count_existing_tiles(output_dir) + skipped_tiles

        update_json(
            status_path,
            phase="ready",
            ready=True,
            worker_alive=False,
            worker_pid=None,
            finished_ts=now_ts(),
            heartbeat_ts=now_ts(),
            last_progress_ts=now_ts(),
            processed_tiles=processed_tiles,
            failed_tiles=failed_tiles,
            skipped_tiles=skipped_tiles,
            last_action="done",
        )

    except Exception as e:
        update_json(
            status_path,
            phase="worker_error",
            ready=False,
            worker_alive=False,
            worker_pid=None,
            error=f"{type(e).__name__}: {e}",
            traceback=str(e),
            heartbeat_ts=now_ts(),
            last_action="worker_exception",
        )
        raise


def monitor_thread(
    slide_path: str,
    output_dir: str,
    status_path: str,
    failed_path: str,
    workers: int,
    stall_sec: int,
    restart_limit: int,
    batch_rows: int,
    skip_failed_tiles: bool,
):
    status_path = Path(status_path)
    restart_count = 0
    coordinator = None

    def start_coordinator():
        proc = mp.Process(
            target=coordinator_preprocess,
            args=(
                slide_path,
                output_dir,
                str(status_path),
                str(failed_path),
                workers,
                batch_rows,
                skip_failed_tiles,
            ),
            daemon=False,
        )
        proc.start()
        update_json(
            status_path,
            worker_alive=True,
            worker_pid=proc.pid,
            restart_count=restart_count,
            heartbeat_ts=now_ts(),
            last_action="coordinator_started",
        )
        return proc

    coordinator = start_coordinator()

    while True:
        time.sleep(1.0)
        state = read_json(status_path, {})
        phase = state.get("phase", "unknown")
        ready = bool(state.get("ready", False))
        last_progress_ts = state.get("last_progress_ts")
        heartbeat_ts = state.get("heartbeat_ts")

        if ready and phase == "ready":
            if coordinator is not None and coordinator.is_alive():
                coordinator.join(timeout=0.1)
            break

        if coordinator is not None and not coordinator.is_alive():
            exitcode = coordinator.exitcode
            state = read_json(status_path, {})
            if bool(state.get("ready", False)):
                break

            if restart_count >= restart_limit:
                update_json(
                    status_path,
                    phase="error",
                    ready=False,
                    worker_alive=False,
                    worker_pid=None,
                    error=f"Coordinator stopped too many times. exitcode={exitcode}",
                    finished_ts=now_ts(),
                    last_action="restart_limit_reached",
                )
                break

            restart_count += 1
            update_json(
                status_path,
                phase="restarting_worker",
                ready=False,
                worker_alive=False,
                worker_pid=None,
                restart_count=restart_count,
                error=None,
                traceback=None,
                last_action=f"restart_after_exitcode_{exitcode}",
            )
            coordinator = start_coordinator()
            continue

        now = now_ts()
        if last_progress_ts is not None and (now - float(last_progress_ts)) > stall_sec:
            if coordinator is not None and coordinator.is_alive():
                try:
                    coordinator.kill()
                except Exception:
                    try:
                        coordinator.terminate()
                    except Exception:
                        pass
                coordinator.join(timeout=5)

            if restart_count >= restart_limit:
                update_json(
                    status_path,
                    phase="error",
                    ready=False,
                    worker_alive=False,
                    worker_pid=None,
                    error="Coordinator stalled too many times without progress.",
                    finished_ts=now_ts(),
                    last_action="restart_limit_reached_after_stall",
                )
                break

            restart_count += 1
            update_json(
                status_path,
                phase="restarting_worker",
                ready=False,
                worker_alive=False,
                worker_pid=None,
                restart_count=restart_count,
                heartbeat_ts=now,
                last_action="restart_after_no_progress",
            )
            coordinator = start_coordinator()
            continue

        if heartbeat_ts is not None:
            update_json(
                status_path,
                heartbeat_ts=now,
                worker_alive=(coordinator is not None and coordinator.is_alive()),
                worker_pid=(coordinator.pid if coordinator is not None and coordinator.is_alive() else None),
            )

    update_json(
        status_path,
        worker_alive=False,
        worker_pid=None,
        finished_ts=read_json(status_path, {}).get("finished_ts", now_ts()),
    )


@APP.route("/status")
def status():
    return add_cors(jsonify(read_json(Path(APP.config["STATUS_PATH"]), {"phase": "missing_status"})))


@APP.route("/failed_tiles")
def failed_tiles():
    return add_cors(jsonify(read_json(Path(APP.config["FAILED_PATH"]), {"failed_tiles": []})))


@APP.route("/slide.dzi")
def dzi():
    output_dir = Path(APP.config["OUTPUT_DIR"])
    path = output_dir / "slide.dzi"
    if not path.exists():
        abort(404)
    resp = make_response(
        send_from_directory(str(output_dir), "slide.dzi", mimetype="application/xml")
    )
    return add_cors(resp)


@APP.route("/slide_files/<int:level>/<path:filename>")
def tile(level, filename):
    output_dir = Path(APP.config["OUTPUT_DIR"])
    tile_dir = output_dir / "slide_files" / str(level)
    path = tile_dir / filename
    if not path.exists():
        abort(404)
    resp = make_response(send_from_directory(str(tile_dir), filename))
    return add_cors(resp)


def main():
    mp.set_start_method("spawn", force=True)

    args = parse_args()
    slide_path = os.path.abspath(args.slide)
    output_dir = os.path.abspath(args.output_dir)

    output_dir_p = Path(output_dir)
    output_dir_p.mkdir(parents=True, exist_ok=True)

    status_path = output_dir_p / STATUS_FILE
    failed_path = output_dir_p / FAILED_FILE

    requested_workers = max(1, min(int(args.workers), 10))

    atomic_write_json(
        status_path,
        initial_status(
            output_dir=output_dir,
            slide_path=slide_path,
            workers=requested_workers,
            stall_sec=max(20, int(args.stall_sec)),
            restart_limit=max(1, int(args.restart_limit)),
        ),
    )
    atomic_write_json(failed_path, {"failed_tiles": []})

    meta = build_metadata(slide_path)
    (output_dir_p / "slide.dzi").write_text(meta["dzi_text"], encoding="utf-8")
    update_json(
        status_path,
        total_tiles=int(meta["total_tiles"]),
        dzi_written=True,
        phase="starting_monitor",
        last_action="metadata_ready",
    )

    APP.config["OUTPUT_DIR"] = output_dir
    APP.config["STATUS_PATH"] = str(status_path)
    APP.config["FAILED_PATH"] = str(failed_path)

    import threading
    mon = threading.Thread(
        target=monitor_thread,
        args=(
            slide_path,
            output_dir,
            str(status_path),
            str(failed_path),
            requested_workers,
            max(20, int(args.stall_sec)),
            max(1, int(args.restart_limit)),
            max(1, int(args.batch_rows)),
            bool(args.skip_failed_tiles),
        ),
        daemon=True,
    )
    mon.start()

    APP.run(
        host=args.host,
        port=args.port,
        debug=False,
        threaded=True,
        use_reloader=False,
    )


if __name__ == "__main__":
    main()
