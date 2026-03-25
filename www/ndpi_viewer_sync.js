
window.ndpiSyncViewer = (() => {
  const states = new Map();
  const registeredHandlers = new Set();
  let activeContainerId = null;

  function getState(containerId = activeContainerId) {
    if (!containerId || !states.has(containerId)) return null;
    return states.get(containerId);
  }

  function setInput(state, id, value) {
    if (!window.Shiny || !state) return;
    Shiny.setInputValue(state.inputPrefix + id, value, { priority: "event" });
  }

  function toImagePoint(state, clientX, clientY) {
    const rect = state.viewer.container.getBoundingClientRect();
    const px = new OpenSeadragon.Point(clientX - rect.left, clientY - rect.top);
    const vp = state.viewer.viewport.pointFromPixel(px);
    const im = state.viewer.viewport.viewportToImageCoordinates(vp);
    return { x: im.x, y: im.y };
  }

  function clearTempPolyline(state) {
    if (!state || !state.polyline) return;
    state.polyline.setAttribute("points", "");
  }

  function redrawTempPolyline(state) {
    if (!state || !state.polyline) return;
    const pts = state.points.map((p) => `${p.x},${p.y}`).join(" ");
    state.polyline.setAttribute("points", pts);
  }

  function updatePointerMode(state) {
    if (!state || !state.drawLayer) return;
    const interactive = !!state.regMode || !!state.drawing;
    state.drawLayer.style.pointerEvents = interactive ? "auto" : "none";
  }

  function finishPolygon(state) {
    if (!state) return;
    state.pointerDown = false;

    if (state.points.length >= 3) {
      setInput(state, "ndpi_polygon_finished", {
        points: state.points,
        ts: Date.now()
      });
    }

    state.points = [];
    clearTempPolyline(state);
    state.drawing = false;
    state.viewer.setMouseNavEnabled(true);
    updatePointerMode(state);
  }

  function cancelPolygon(state) {
    if (!state) return;
    state.pointerDown = false;
    state.points = [];
    clearTempPolyline(state);
    state.drawing = false;
    state.viewer.setMouseNavEnabled(true);
    updatePointerMode(state);
  }

  function ensureOverlay(state) {
    const c = state.viewer.container;
    c.style.position = "relative";

    const drawLayer = document.createElement("div");
    drawLayer.style.position = "absolute";
    drawLayer.style.inset = "0";
    drawLayer.style.zIndex = "999";
    drawLayer.style.pointerEvents = "none";
    c.appendChild(drawLayer);

    const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.style.position = "absolute";
    svg.style.inset = "0";
    svg.style.width = "100%";
    svg.style.height = "100%";
    drawLayer.appendChild(svg);

    const g = document.createElementNS("http://www.w3.org/2000/svg", "g");
    svg.appendChild(g);

    const polyline = document.createElementNS("http://www.w3.org/2000/svg", "polyline");
    polyline.setAttribute("fill", "rgba(0, 255, 255, 0.08)");
    polyline.setAttribute("stroke", "#00ffff");
    polyline.setAttribute("stroke-width", "2");
    polyline.setAttribute("points", "");
    g.appendChild(polyline);

    state.drawLayer = drawLayer;
    state.svg = svg;
    state.g = g;
    state.polyline = polyline;

    drawLayer.addEventListener("mousedown", (e) => {
      if (!state.drawing) return;
      e.preventDefault();
      e.stopPropagation();

      state.pointerDown = true;
      state.points = [toImagePoint(state, e.clientX, e.clientY)];
      redrawTempPolyline(state);
    });

    drawLayer.addEventListener("mousemove", (e) => {
      if (!state.drawing || !state.pointerDown) return;

      const p = toImagePoint(state, e.clientX, e.clientY);
      const last = state.points[state.points.length - 1];
      const dx = p.x - last.x;
      const dy = p.y - last.y;

      if ((dx * dx + dy * dy) >= 9) {
        state.points.push(p);
        redrawTempPolyline(state);
      }
    });

    drawLayer.addEventListener("mouseup", (e) => {
      if (!state.drawing || !state.pointerDown) return;
      state.points.push(toImagePoint(state, e.clientX, e.clientY));
      redrawTempPolyline(state);
      finishPolygon(state);
    });

    drawLayer.addEventListener("mouseleave", () => {
      if (state.drawing && state.pointerDown) {
        finishPolygon(state);
      }
    });

    drawLayer.addEventListener("click", (e) => {
      if (!state.regMode || state.drawing) return;
      const p = toImagePoint(state, e.clientX, e.clientY);
      setInput(state, "ndpi_landmark_click", { x: p.x, y: p.y, ts: Date.now() });
      e.preventDefault();
      e.stopPropagation();
    });

    window.addEventListener("keydown", (e) => {
      if (e.key === "Escape") {
        const s = getState();
        if (s && s.drawing) cancelPolygon(s);
      }
    });

    updatePointerMode(state);
  }

  function destroy(containerId = activeContainerId) {
    const state = getState(containerId);
    if (!state) return;

    try {
      if (state.viewer) state.viewer.destroy();
    } catch (_) {}

    if (state.drawLayer && state.drawLayer.parentNode) {
      state.drawLayer.parentNode.removeChild(state.drawLayer);
    }

    states.delete(containerId);
    if (activeContainerId === containerId) activeContainerId = null;
  }

  function init(opts) {
    if (!window.OpenSeadragon) {
      throw new Error("OpenSeadragon is not loaded.");
    }

    const containerId = opts.containerId;
    if (!containerId) throw new Error("containerId is required.");
    destroy(containerId);

    const state = {
      containerId,
      inputPrefix: opts.inputPrefix || "",
      viewer: null,
      drawLayer: null,
      svg: null,
      g: null,
      polyline: null,
      regMode: false,
      drawing: false,
      pointerDown: false,
      points: []
    };

    state.viewer = OpenSeadragon({
      id: containerId,
      prefixUrl: "https://openseadragon.github.io/openseadragon/images/",
      tileSources: opts.dziUrl,
      showNavigator: true
    });

    state.viewer.addHandler("open", () => {
      ensureOverlay(state);
    });

    states.set(containerId, state);
    activeContainerId = containerId;
  }

  function setRegistrationMode(on, containerId = activeContainerId) {
    const state = getState(containerId);
    if (!state) return;
    state.regMode = !!on;
    if (state.regMode) {
      state.drawing = false;
      state.pointerDown = false;
      state.viewer.setMouseNavEnabled(true);
      clearTempPolyline(state);
    }
    updatePointerMode(state);
  }

  function startPolygon(containerId = activeContainerId) {
    const state = getState(containerId);
    if (!state) return;
    state.regMode = false;
    state.drawing = true;
    state.pointerDown = false;
    state.points = [];
    clearTempPolyline(state);
    state.viewer.setMouseNavEnabled(false);
    updatePointerMode(state);
  }

  function registerShinyHandlers({ loadHandler, regHandler, polyHandler }) {
    if (!window.Shiny) return;

    const pairs = [
      [loadHandler, (msg) => init(msg)],
      [regHandler, (msg) => setRegistrationMode(!!msg.enabled, msg.containerId || activeContainerId)],
      [polyHandler, (msg) => startPolygon(msg?.containerId || activeContainerId)]
    ];

    for (const [name, fn] of pairs) {
      if (!name || registeredHandlers.has(name)) continue;
      Shiny.addCustomMessageHandler(name, fn);
      registeredHandlers.add(name);
    }
  }

  // Convenience helper: pass ns("") from R, e.g. "clustering-"
  function bindNamespacedHandlers(nsPrefix) {
    registerShinyHandlers({
      loadHandler: `${nsPrefix}ndpiLoadSlide`,
      regHandler: `${nsPrefix}ndpiSetRegistrationMode`,
      polyHandler: `${nsPrefix}ndpiStartPolygon`
    });
  }

  return {
    init,
    destroy,
    setRegistrationMode,
    startPolygon,
    registerShinyHandlers,
    bindNamespacedHandlers
  };
})();