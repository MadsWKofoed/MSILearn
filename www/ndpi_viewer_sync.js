window.ndpiSyncViewer = (() => {
  const states = new Map();
  let activeContainerId = null;

  function getState(containerId = activeContainerId) {
    if (!containerId || !states.has(containerId)) return null;
    return states.get(containerId);
  }

  function setInput(state, id, value) {
    if (!window.Shiny || !state) return;
    Shiny.setInputValue(state.inputPrefix + id, value, { priority: "event" });
  }

  function makeSvgEl(tag, attrs = {}) {
    const el = document.createElementNS("http://www.w3.org/2000/svg", tag);
    Object.entries(attrs).forEach(([k, v]) => el.setAttribute(k, v));
    return el;
  }

  function imageCoordsFromClient(state, clientX, clientY) {
    const rect = state.viewer.container.getBoundingClientRect();
    const pixel = new OpenSeadragon.Point(clientX - rect.left, clientY - rect.top);
    const vpPoint = state.viewer.viewport.pointFromPixel(pixel);
    const imgPoint = state.viewer.viewport.viewportToImageCoordinates(vpPoint);
    return { x: imgPoint.x, y: imgPoint.y };
  }

    function scheduleRedraw(state) {
    if (!state) return;

    if (!state.isHovered && !state.drawing) return;

    if (state.rafPending) return;
    state.rafPending = true;

    requestAnimationFrame(() => {
        state.rafPending = false;
        redrawAll(state);
    });
    }

  function imageToScreen(state, point) {
    const vp = state.viewer.viewport.imageToViewportCoordinates(point.x, point.y);
    const px = state.viewer.viewport.pixelFromPoint(vp, true);
    return { x: px.x, y: px.y };
  }

  function pointsToScreenString(state, points) {
    return points
      .map((p) => {
        const s = imageToScreen(state, p);
        return `${s.x},${s.y}`;
      })
      .join(" ");
  }

  function clearSvg(state) {
    if (!state || !state.drawGroup) return;
    while (state.drawGroup.firstChild) {
      state.drawGroup.removeChild(state.drawGroup.firstChild);
    }
  }

  function getDisplaySizes(state) {
    const zoom = state?.viewer ? state.viewer.viewport.getZoom(true) : 1;
    return {
      strokeWidth: Math.max(1.2, 2 / zoom),
      pointRadius: Math.max(1.5, 3 / zoom),
      pointStrokeWidth: Math.max(0.8, 1 / zoom)
    };
  }

  function drawCurrentPolyline(state) {
    if (!state || !state.drawGroup || state.currentPoints.length === 0) return;

    const sizes = getDisplaySizes(state);

    const pl = makeSvgEl("polyline", {
      points: pointsToScreenString(state, state.currentPoints),
      fill: "none",
      stroke: "#ffff00",
      "stroke-width": sizes.strokeWidth,
      "stroke-linejoin": "round",
      "stroke-linecap": "round"
    });
    state.drawGroup.appendChild(pl);

    state.currentPoints.forEach((p) => {
      const s = imageToScreen(state, p);
      const c = makeSvgEl("circle", {
        cx: s.x,
        cy: s.y,
        r: sizes.pointRadius,
        fill: "#00ffff",
        stroke: "#000000",
        "stroke-width": sizes.pointStrokeWidth
      });
      state.drawGroup.appendChild(c);
    });
  }

  function drawLastPolygon(state) {
    if (!state || !state.drawGroup || !state.lastPolygon || state.lastPolygon.length < 3) return;

    const sizes = getDisplaySizes(state);

    const poly = makeSvgEl("polygon", {
      points: pointsToScreenString(state, state.lastPolygon),
      fill: "rgba(255, 255, 0, 0.20)",
      stroke: "#ffcc00",
      "stroke-width": sizes.strokeWidth,
      "stroke-linejoin": "round"
    });
    state.drawGroup.appendChild(poly);
  }

  function redrawAll(state) {
    if (!state || !state.viewer || !state.svg) return;

    const w = state.viewer.container.clientWidth;
    const h = state.viewer.container.clientHeight;

    state.svg.setAttribute("width", w);
    state.svg.setAttribute("height", h);
    state.svg.setAttribute("viewBox", `0 0 ${w} ${h}`);

    clearSvg(state);
    drawLastPolygon(state);
    drawCurrentPolyline(state);
  }

  function setDrawMode(state, enabled) {
    state.drawing = !!enabled;

    if (!state.drawLayer) return;

    if (state.drawing) {
      state.drawLayer.style.pointerEvents = "auto";
      state.drawLayer.style.cursor = "crosshair";
      state.viewer.setMouseNavEnabled(false);
    } else {
      state.drawLayer.style.pointerEvents = "none";
      state.drawLayer.style.cursor = "default";
      state.viewer.setMouseNavEnabled(true);
      state.pointerDown = false;
      state.currentPoints = [];
      redrawAll(state);
    }
  }

  function finishPolygon(state) {
    if (!state) return;

    if (state.currentPoints.length < 3) {
      state.currentPoints = [];
      state.pointerDown = false;
      setDrawMode(state, false);
      redrawAll(state);
      return;
    }

    const poly = state.currentPoints.map((p) => ({ x: p.x, y: p.y }));

    state.lastPolygon = poly;
    state.currentPoints = [];
    state.pointerDown = false;
    setDrawMode(state, false);
    redrawAll(state);

    setInput(state, "ndpi_polygon_finished", {
      points: poly,
      ts: Date.now()
    });
  }

  function cancelPolygon(state) {
    if (!state) return;
    state.pointerDown = false;
    state.currentPoints = [];
    setDrawMode(state, false);
    redrawAll(state);
  }

  function removeOverlay(state) {
    if (state?.drawLayer && state.drawLayer.parentNode) {
      state.drawLayer.parentNode.removeChild(state.drawLayer);
    }
    state.drawLayer = null;
    state.svg = null;
    state.drawGroup = null;
  }

  function ensureOverlay(state) {
    const container = state.viewer.container;
    container.style.position = "relative";

    removeOverlay(state);

    const drawLayer = document.createElement("div");
    drawLayer.style.position = "absolute";
    drawLayer.style.inset = "0";
    drawLayer.style.zIndex = "999";
    drawLayer.style.pointerEvents = "none";
    drawLayer.style.background = "transparent";

    const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.style.position = "absolute";
    svg.style.inset = "0";
    svg.style.width = "100%";
    svg.style.height = "100%";
    svg.style.overflow = "visible";

    const drawGroup = document.createElementNS("http://www.w3.org/2000/svg", "g");
    svg.appendChild(drawGroup);

    drawLayer.appendChild(svg);
    container.appendChild(drawLayer);

    state.drawLayer = drawLayer;
    state.svg = svg;
    state.drawGroup = drawGroup;

    drawLayer.addEventListener("mousedown", (e) => {
      if (!state.drawing) return;
      e.preventDefault();
      e.stopPropagation();

      state.pointerDown = true;
      state.currentPoints = [imageCoordsFromClient(state, e.clientX, e.clientY)];
      redrawAll(state);
    });

    drawLayer.addEventListener("mousemove", (e) => {
      if (!state.drawing || !state.pointerDown) return;
      e.preventDefault();
      e.stopPropagation();

      const pt = imageCoordsFromClient(state, e.clientX, e.clientY);
      const last = state.currentPoints[state.currentPoints.length - 1];
      const dx = pt.x - last.x;
      const dy = pt.y - last.y;

      if ((dx * dx + dy * dy) > 100) {
        state.currentPoints.push(pt);
        redrawAll(state);
      }
    });

    drawLayer.addEventListener("mouseup", (e) => {
      if (!state.drawing || !state.pointerDown) return;
      e.preventDefault();
      e.stopPropagation();

      state.currentPoints.push(imageCoordsFromClient(state, e.clientX, e.clientY));
      finishPolygon(state);
    });

    drawLayer.addEventListener("mouseleave", () => {
      if (state.drawing && state.pointerDown) {
        finishPolygon(state);
      }
    });

    window.addEventListener("keydown", (e) => {
      if (e.key === "Escape") {
        const s = getState();
        if (s && s.drawing) cancelPolygon(s);
      }
    });

    redrawAll(state);
  }

  function destroy(containerId = activeContainerId) {
    const state = getState(containerId);
    if (!state) return;

    try {
      if (state.viewer) state.viewer.destroy();
    } catch (_) {}

    removeOverlay(state);

    states.delete(containerId);
    if (activeContainerId === containerId) activeContainerId = null;
  }

    function init(opts) {
    if (!window.OpenSeadragon) throw new Error("OpenSeadragon is not loaded.");

    const containerId = opts.containerId;
    if (!containerId) throw new Error("containerId is required.");

    destroy(containerId);

    const container = document.getElementById(containerId);
    if (!container) throw new Error(`Container not found: ${containerId}`);

    const state = {
        containerId,
        container,
        inputPrefix: opts.inputPrefix || "",
        viewer: null,
        drawLayer: null,
        svg: null,
        drawGroup: null,
        drawing: false,
        pointerDown: false,
        currentPoints: [],
        lastPolygon: null,
        rafPending: false,
        isHovered: false,
    };

    state.container.addEventListener("mouseenter", () => {
        state.isHovered = true;
        scheduleRedraw(state);
    });

    state.container.addEventListener("mouseleave", () => {
        state.isHovered = false;
    });

    state.viewer = OpenSeadragon({
      id: containerId,
      prefixUrl: "https://openseadragon.github.io/openseadragon/images/",
      tileSources: opts.dziUrl,
      showNavigator: true,
      zoomPerScroll: 2,
      animationTime: 0.15,
      springStiffness: 7,
      imageLoaderLimit: 6,
      timeout: 120000,
      maxImageCacheCount: 400,
      maxZoomPixelRatio: 6,
      minZoomImageRatio: 1,
      visibilityRatio: 1,
      constrainDuringPan: true,
      immediateRender: true,
      blendTime: 0,
      alwaysBlend: false,
      gestureSettingsMouse: {
        clickToZoom: false,
        dblClickToZoom: false,
        dragToPan: true,
        scrollToZoom: true
      }
    });

    state.viewer.addHandler("open", () => {
        ensureOverlay(state);
        redrawAll(state);
    });

    state.viewer.addHandler("animation", () => scheduleRedraw(state));
    state.viewer.addHandler("resize", () => scheduleRedraw(state));
    state.viewer.addHandler("pan", () => scheduleRedraw(state));
    state.viewer.addHandler("zoom", () => scheduleRedraw(state));

    states.set(containerId, state);
    activeContainerId = containerId;
    }

    function startPolygon(containerId = activeContainerId) {
    const state = getState(containerId);
    if (!state || !state.viewer) return;
    state.lastPolygon = null;
    state.currentPoints = [];
    redrawAll(state);
    setDrawMode(state, true);
    scheduleRedraw(state);
    }

    function stopPolygon(containerId = activeContainerId) {
    const state = getState(containerId);
    if (!state || !state.viewer) return;
    cancelPolygon(state);
    scheduleRedraw(state);
    }

  return {
    init,
    destroy,
    startPolygon,
    stopPolygon
  };
})();