/* ICL Browser JavaScript
 * SPDX-License-Identifier: MIT
 * Copyright (C) 2025 Anthony Green <green@moxielogic.com>
 *
 * Configuration is read from body data attributes (CSP-compliant).
 */

// Read configuration from body data attributes (no inline scripts needed)
const ICL_CONFIG = {
  wsToken: document.body.dataset.wsToken,
  version: document.body.dataset.version,
  unsafeVisualizations: document.body.dataset.unsafeVisualizations === 'true'
};

// Monaco editor for syntax highlighting (colorizer only, not full editor)
let monacoReady = false;
let monacoReadyPromise = null;

function initMonaco() {
  if (monacoReadyPromise) return monacoReadyPromise;

  monacoReadyPromise = new Promise((resolve) => {
    if (typeof require !== 'undefined' && require.config) {
      require.config({ paths: { 'vs': '/assets/monaco/vs' }});
      require(['vs/editor/editor.main'], function() {
        monacoReady = true;
        resolve();
      });
    } else {
      // Monaco loader not available yet, wait a bit
      setTimeout(() => {
        initMonaco().then(resolve);
      }, 100);
    }
  });
  return monacoReadyPromise;
}

// Start loading Monaco immediately
initMonaco();

// Helper to setup modal close buttons (CSP-compliant - no inline handlers)
function setupModalCloseButtons(container) {
  container.querySelectorAll('.modal-close-btn').forEach(btn => {
    btn.addEventListener('click', () => {
      const backdrop = btn.closest('.modal-backdrop');
      if (backdrop) backdrop.remove();
    });
  });
}

// Check for updates
async function checkForUpdates() {
  const currentVersion = ICL_CONFIG.version.replace(/^v/, '').split('-')[0]; // Strip 'v' prefix and git suffix

  // Show checking dialog
  const backdrop = document.createElement('div');
  backdrop.id = 'update-backdrop';
  backdrop.className = 'modal-backdrop';
  backdrop.onclick = (e) => { if (e.target === backdrop) backdrop.remove(); };

  const modal = document.createElement('div');
  modal.className = 'modal-dialog update-dialog';
  modal.innerHTML = `
    <h3>Checking for Updates...</h3>
    <p class="update-status">Contacting GitHub...</p>
    <button class="about-close modal-close-btn">Cancel</button>
  `;
  backdrop.appendChild(modal);
  document.body.appendChild(backdrop);
  setupModalCloseButtons(modal);

  try {
    const response = await fetch('https://api.github.com/repos/atgreen/icl/releases/latest');
    if (!response.ok) throw new Error('Failed to fetch release info');

    const release = await response.json();
    const latestVersion = release.tag_name.replace(/^v/, '');

    // Simple version comparison (works for semver)
    const isNewer = compareVersions(latestVersion, currentVersion) > 0;

    if (isNewer) {
      modal.innerHTML = `
        <h3>Update Available</h3>
        <p class="update-status">A new version of ICL is available!</p>
        <div class="update-versions">
          <div><span class="update-label">Current:</span> ${currentVersion}</div>
          <div><span class="update-label">Latest:</span> ${latestVersion}</div>
        </div>
        <p class="update-note">If you installed via a package manager (apt, dnf, choco, brew), update through that instead.</p>
        <div class="update-actions">
          <a href="${release.html_url}" target="_blank" class="update-link">View Release</a>
          <button class="about-close modal-close-btn">Close</button>
        </div>
      `;
      setupModalCloseButtons(modal);
    } else {
      modal.innerHTML = `
        <h3>You're Up to Date</h3>
        <p class="update-status">ICL ${currentVersion} is the latest version.</p>
        <button class="about-close modal-close-btn">Close</button>
      `;
      setupModalCloseButtons(modal);
    }
  } catch (err) {
    modal.innerHTML = `
      <h3>Update Check Failed</h3>
      <p class="update-status">Could not check for updates: ${err.message}</p>
      <p class="update-note">You can check manually at:</p>
      <a href="https://github.com/atgreen/icl/releases" target="_blank" class="update-link">github.com/atgreen/icl/releases</a>
      <button class="about-close modal-close-btn" style="margin-top:16px;">Close</button>
    `;
    setupModalCloseButtons(modal);
  }
}

// Compare semver versions, returns: 1 if a > b, -1 if a < b, 0 if equal
function compareVersions(a, b) {
  const partsA = a.split('.').map(n => parseInt(n, 10) || 0);
  const partsB = b.split('.').map(n => parseInt(n, 10) || 0);
  for (let i = 0; i < Math.max(partsA.length, partsB.length); i++) {
    const numA = partsA[i] || 0;
    const numB = partsB[i] || 0;
    if (numA > numB) return 1;
    if (numA < numB) return -1;
  }
  return 0;
}

// About dialog
function showAboutDialog() {
  // Remove existing dialog if any
  const existing = document.getElementById('about-backdrop');
  if (existing) existing.remove();

  const backdrop = document.createElement('div');
  backdrop.id = 'about-backdrop';
  backdrop.className = 'modal-backdrop';
  backdrop.onclick = (e) => { if (e.target === backdrop) backdrop.remove(); };

  const modal = document.createElement('div');
  modal.className = 'modal-dialog';
  modal.innerHTML = `
    <div class="about-header">
      <img src="/assets/favicon-192.png" alt="ICL" class="about-logo">
      <div>
        <h2 class="about-title">Interactive Common Lisp</h2>
        <div class="about-version">Version ${ICL_CONFIG.version}</div>
      </div>
    </div>
    <p class="about-description">An enhanced REPL for Common Lisp with multi-line editing, system browser, visualization, and profiling.</p>
    <div class="about-section">
      <div class="about-label">Connected Lisp</div>
      <div id="about-lisp-info">Loading...</div>
    </div>
    <div class="about-section">
      <div class="about-label">Author</div>
      <div>Anthony Green
        <span class="about-sep">|</span>
        <a href="https://github.com/atgreen" target="_blank" class="about-link">GitHub</a>
        <span class="about-sep">|</span>
        <a href="https://www.linkedin.com/in/green/" target="_blank" class="about-link">LinkedIn</a>
      </div>
    </div>
    <div class="about-section">
      <div class="about-label">License</div>
      <div>MIT</div>
    </div>
    <div class="about-links">
      <a href="https://github.com/atgreen/icl" target="_blank" class="about-link">Project on GitHub</a>
      <span class="about-sep">|</span>
      <a href="/assets/OPEN-SOURCE-NOTICES.txt" target="_blank" class="about-link">Open Source Notices</a>
    </div>
    <button class="about-close modal-close-btn">Close</button>
  `;

  backdrop.appendChild(modal);
  document.body.appendChild(backdrop);
  setupModalCloseButtons(modal);

  // Request Lisp info from server
  if (ws && ws.readyState === WebSocket.OPEN) {
    ws.send(JSON.stringify({ type: 'get-lisp-info' }));
  }
}

// WebSocket connection
const ws = new WebSocket('ws://' + location.host + '/ws/' + ICL_CONFIG.wsToken);
let terminal, fitAddon;
let pendingTheme = null;  // Store theme if received before terminal is ready
let selectedPackage = null;
let selectedSymbol = null;
let packages = [];
let symbols = [];

// Dockview API and inspector management
let dockviewApi = null;
let inspectorCounter = 0;
const inspectorStates = new Map();  // panelId -> {depth, element, header}
const pendingInspections = new Map();  // panelId -> last inspection msg

// Graphviz class graph state management
let classGraphCounter = 0;
const graphvizStates = new Map();  // panelId -> GraphvizPanel instance
const pendingClassGraphs = new Map();  // panelId -> pending graph messages

// Connection status indicator
function updateConnectionStatus(connected) {
  const indicator = document.getElementById('connection-status');
  if (indicator) {
    indicator.className = 'connection-status ' + (connected ? 'connected' : 'disconnected');
    indicator.title = connected ? 'Connected' : 'Disconnected';
  }
}

ws.onopen = () => {
  console.log('Connected');
  updateConnectionStatus(true);
  // Send dark mode preference for theme auto-selection
  sendDarkModePreference();
  ws.send(JSON.stringify({type: 'get-packages'}));
  // Request Lisp info to update tab title
  ws.send(JSON.stringify({ type: 'get-lisp-info' }));
};

ws.onclose = () => {
  console.log('Connection closed - ICL process terminated');
  updateConnectionStatus(false);
  document.title = 'ICL - Disconnected';
  // Close the browser window/tab when ICL dies
  window.close();
  // If window.close() doesn't work (e.g., not opened by script), show message
  document.body.innerHTML = '<div style="display:flex;align-items:center;justify-content:center;height:100vh;background:#1a1a2e;color:#eee;font-family:monospace;font-size:1.2em;">ICL process terminated. You may close this tab.</div>';
};

ws.onerror = (err) => {
  console.error('WebSocket error:', err);
  updateConnectionStatus(false);
};

ws.onmessage = (e) => {
  const msg = JSON.parse(e.data);
  switch(msg.type) {
    case 'output':
      if (terminal) terminal.write(msg.data.replace(/\n/g, '\r\n'));
      break;
    case 'packages':
      packages = msg.data || [];
      // Preserve current filter when re-rendering
      renderPackages(document.getElementById('package-filter')?.value || '');
      // Re-fetch symbols for selected package (may have new definitions)
      if (selectedPackage) {
        ws.send(JSON.stringify({type: 'get-symbols', package: selectedPackage}));
      }
      break;
    case 'symbols':
      symbols = msg.data || [];
      // Preserve current filter when re-rendering
      renderSymbols(document.getElementById('symbol-filter')?.value || '');
      break;
    case 'symbol-info':
      renderSymbolInfo(msg.data);
      break;
    case 'inspection':
      console.log('inspection received:', msg, 'panelId:', msg['panel-id']);
      renderInspection(msg, msg['panel-id']);
      break;
    case 'inspector-eval-result':
      handleInspectorEvalResult(msg);
      break;
    case 'inspector-expand-result':
      handleInspectorExpandResult(msg);
      break;
    case 'symbol-clicked':
      handleSymbolClicked(msg);
      break;
    case 'theme':
      applyTheme(msg.data);
      break;
    case 'lisp-info':
      // Update browser tab title
      if (msg['lisp-type'] && msg['lisp-version']) {
        document.title = 'ICL - ' + msg['lisp-type'] + ' ' + msg['lisp-version'];
      }
      // Update About dialog if open
      const lispInfoEl = document.getElementById('about-lisp-info');
      if (lispInfoEl) {
        if (msg['lisp-type'] && msg['lisp-version']) {
          lispInfoEl.textContent = msg['lisp-type'] + ' ' + msg['lisp-version'];
        } else {
          lispInfoEl.textContent = 'Not connected';
        }
      }
      break;
    case 'open-speedscope':
      openSpeedscopePanel(msg.profileId, msg.title);
      break;
    case 'open-coverage':
      openCoveragePanel(msg.title);
      break;
    case 'open-monaco-coverage':
      openMonacoCoveragePanel(msg.title, msg.data);
      break;
    case 'class-graph':
      handleClassGraph(msg);
      break;
    case 'class-graph-expand':
      handleClassGraphExpand(msg);
      break;
    case 'class-children-list':
      handleClassChildrenList(msg);
      break;
    case 'open-class-graph':
      openClassGraphPanel(msg.className, msg.packageName);
      restoreTerminalFocus();
      break;
    case 'open-hash-table':
      openHashTablePanel(msg.title, msg.count, msg.entries, msg.sourceExpr);
      restoreTerminalFocus();
      break;
    case 'open-svg':
      openSvgPanel(msg.title, msg.content, msg.sourceExpr);
      restoreTerminalFocus();
      break;
    case 'open-html':
      openHtmlPanel(msg.title, msg.content, msg.sourceExpr);
      restoreTerminalFocus();
      break;
    case 'open-json':
      openJsonPanel(msg.title, msg.content, msg.sourceExpr);
      restoreTerminalFocus();
      break;
    case 'open-image':
      openImagePanel(msg.title, msg.imageUrl, msg.contentType, msg.sourceExpr);
      restoreTerminalFocus();
      break;
    case 'refresh-visualizations':
      refreshAllVisualizations();
      break;
    case 'hashtable-refresh':
      handleHashtableRefresh(msg);
      break;
    case 'open-venn':
      openVennPanel(msg.setNames, msg.setMembers, msg.sourceExpr);
      restoreTerminalFocus();
      break;
    case 'venn-refresh':
      handleVennRefresh(msg);
      break;
    case 'html-refresh':
      handleHtmlRefresh(msg);
      break;
    case 'svg-refresh':
      handleSvgRefresh(msg);
      break;
    case 'viz-type-changed':
      handleVizTypeChanged(msg);
      break;
    case 'viz-refresh':
      handleVizRefresh(msg);
      break;
    case 'open-vega-lite':
      openVegaLitePanel(msg.title, msg.spec, msg.sourceExpr);
      restoreTerminalFocus();
      break;
    case 'vega-lite-refresh':
      handleVegaLiteRefresh(msg);
      break;
    case 'open-mermaid':
      openMermaidPanel(msg.title, msg.definition, msg.sourceExpr);
      restoreTerminalFocus();
      break;
    case 'mermaid-refresh':
      handleMermaidRefresh(msg);
      break;
    case 'open-regexp':
      openRegexpPanel(msg.title, msg.pattern, msg.sourceExpr);
      restoreTerminalFocus();
      break;
    case 'regexp-refresh':
      handleRegexpRefresh(msg);
      break;
    case 'open-source':
      openSourcePanel(msg.title, msg.path, msg.content, msg.position, msg.line);
      restoreTerminalFocus();
      break;
  }
};

// Restore focus to terminal after opening visualization panels
function restoreTerminalFocus() {
  setTimeout(() => { if (terminal) terminal.focus(); }, 150);
}

// Open Speedscope flame graph panel
let speedscopeCounter = 0;
function openSpeedscopePanel(profileId, title) {
  const panelId = 'speedscope-' + (++speedscopeCounter);
  const profileUrl = '/profile-data/' + profileId + '.json';
  if (dockviewApi) {
    dockviewApi.addPanel({
      id: panelId,
      component: 'speedscope',
      title: title || 'Flame Graph',
      params: { profileUrl: profileUrl },
      position: { referencePanel: 'terminal', direction: 'right' }
    });
  }
}

// Open coverage report panel (legacy HTML iframe)
let coverageCounter = 0;
function openCoveragePanel(title) {
  const panelId = 'coverage-' + (++coverageCounter);
  if (dockviewApi) {
    dockviewApi.addPanel({
      id: panelId,
      component: 'coverage',
      title: title || 'Coverage Report',
      params: {},
      position: { referencePanel: 'terminal', direction: 'right' }
    });
  }
}

// Open Monaco-style coverage panel with syntax highlighting
function openMonacoCoveragePanel(title, data) {
  const panelId = 'monaco-coverage-' + (++coverageCounter);
  if (dockviewApi) {
    dockviewApi.addPanel({
      id: panelId,
      component: 'monaco-coverage',
      title: title || 'Coverage Report',
      params: { data: data },
      position: { referencePanel: 'terminal', direction: 'right' }
    });
  }
}

// Open Monaco source viewer panel (for ,source command)
let sourceCounter = 0;
function openSourcePanel(title, path, content, position, line) {
  const panelId = 'source-' + (++sourceCounter);
  if (dockviewApi) {
    dockviewApi.addPanel({
      id: panelId,
      component: 'monaco-source',
      title: title || path.split('/').pop() || 'Source',
      params: { path: path, content: content, position: position, line: line },
      position: { referencePanel: 'terminal', direction: 'right' }
    });
  }
}

// Handle class graph data from server
function handleClassGraph(msg) {
  const panelId = msg['panel-id'] || msg.panelId;
  const panel = graphvizStates.get(panelId);
  if (panel) {
    panel.updateGraph(msg);
  } else {
    const pending = pendingClassGraphs.get(panelId) || [];
    pending.push(msg);
    pendingClassGraphs.set(panelId, pending);
  }
}

function handleClassGraphExpand(msg) {
  const panelId = msg['panel-id'] || msg.panelId;
  const panel = graphvizStates.get(panelId);
  if (panel) {
    panel.addGraph(msg);
  } else {
    const pending = pendingClassGraphs.get(panelId) || [];
    pending.push(msg);
    pendingClassGraphs.set(panelId, pending);
  }
}

function handleClassChildrenList(msg) {
  const panelId = msg['panel-id'] || msg.panelId;
  const panel = graphvizStates.get(panelId);
  if (panel) {
    panel.showChildSelector(msg);
  }
}

// Open class graph panel
function openClassGraphPanel(className, packageName) {
  console.log('openClassGraphPanel called:', className, packageName);
  const panelId = 'classgraph-' + (++classGraphCounter);
  if (dockviewApi) {
    console.log('Adding panel:', panelId);
    dockviewApi.addPanel({
      id: panelId,
      component: 'graphviz',
      title: 'Classes: ' + className,
      params: { panelId, className, packageName },
      position: { referencePanel: 'terminal', direction: 'right' }
    });
  } else {
    console.error('dockviewApi not available');
  }
}

// Open hash-table panel
let hashTableCounter = 0;
const hashtableStates = new Map();  // panelId -> HashTablePanel instance

function openHashTablePanel(title, count, entries, sourceExpr) {
  console.log('openHashTablePanel called:', title, count, entries?.length, sourceExpr);
  const panelId = 'hashtable-' + (++hashTableCounter);
  if (dockviewApi) {
    dockviewApi.addPanel({
      id: panelId,
      component: 'hashtable',
      title: title || 'Hash Table',
      params: { panelId, count, entries, sourceExpr },
      position: { referencePanel: 'terminal', direction: 'right' }
    });
  } else {
    console.error('dockviewApi not available');
  }
}

// Sanitize expression for use as panel ID (remove special chars)
function sanitizeForPanelId(expr) {
  if (!expr) return String(Date.now());
  // Replace special characters with safe alternatives
  return expr.replace(/[^a-zA-Z0-9-]/g, '_');
}

// Unified visualization state - tracks all viz panels by source expression
const vizStates = new Map();  // panelId -> {sourceExpr, element, type}

function registerVizPanel(panelId, sourceExpr, element, type) {
  vizStates.set(panelId, { sourceExpr, element, type });
}

function unregisterVizPanel(panelId) {
  vizStates.delete(panelId);
}

// Open SVG panel - tracks by source expression for updates
const svgStates = new Map();
function openSvgPanel(title, content, sourceExpr) {
  console.log('openSvgPanel called:', title, content?.length, sourceExpr);
  const panelId = 'svg-' + sanitizeForPanelId(sourceExpr);
  console.log('SVG panelId:', panelId, 'svgStates has:', svgStates.has(panelId));
  if (dockviewApi) {
    // Check if panel exists and update it
    if (svgStates.has(panelId)) {
      console.log('Updating existing SVG panel');
      const panelState = svgStates.get(panelId);
      if (panelState && panelState._element) {
        panelState._element.innerHTML = content;
        const svg = panelState._element.querySelector('svg');
        if (svg) {
          svg.style.maxWidth = '100%';
          svg.style.maxHeight = '100%';
          svg.style.width = 'auto';
          svg.style.height = 'auto';
        }
      }
    } else {
      console.log('Creating new SVG panel');
      dockviewApi.addPanel({
        id: panelId,
        component: 'svg',
        title: title || 'SVG',
        params: { content, sourceExpr, panelId },
        position: { referencePanel: 'terminal', direction: 'right' }
      });
    }
  }
}

// Open HTML panel - tracks by source expression for updates
const htmlStates = new Map();
function openHtmlPanel(title, content, sourceExpr) {
  console.log('openHtmlPanel called:', title, content?.length, sourceExpr);
  const panelId = 'html-' + sanitizeForPanelId(sourceExpr);
  console.log('HTML panelId:', panelId, 'htmlStates has:', htmlStates.has(panelId));
  if (dockviewApi) {
    // Check if panel exists and update it
    if (htmlStates.has(panelId)) {
      console.log('Updating existing HTML panel');
      const panelState = htmlStates.get(panelId);
      if (panelState && panelState._iframe) {
        panelState._iframe.srcdoc = content;
      }
    } else {
      console.log('Creating new HTML panel');
      dockviewApi.addPanel({
        id: panelId,
        component: 'html',
        title: title || 'HTML',
        params: { content, sourceExpr, panelId },
        position: { referencePanel: 'terminal', direction: 'right' }
      });
    }
  }
}

// Open JSON panel - tracks by source expression for updates
function openJsonPanel(title, content, sourceExpr) {
  console.log('openJsonPanel called:', title, content?.length, sourceExpr);
  const panelId = 'json-' + sanitizeForPanelId(sourceExpr);
  if (dockviewApi) {
    // Check if panel already exists via vizStates
    if (vizStates.has(panelId)) {
      console.log('Updating existing JSON panel');
      const state = vizStates.get(panelId);
      if (state && state.element) {
        renderJson(state.element, content);
      }
    } else {
      console.log('Creating new JSON panel');
      dockviewApi.addPanel({
        id: panelId,
        component: 'json',
        title: title || 'JSON',
        params: { content, sourceExpr, panelId },
        position: { referencePanel: 'terminal', direction: 'right' }
      });
    }
  }
}

// Open Image panel - tracks by source expression for updates
function openImagePanel(title, imageUrl, contentType, sourceExpr) {
  console.log('openImagePanel called:', title, imageUrl, contentType, sourceExpr);
  const panelId = 'image-' + sanitizeForPanelId(sourceExpr);
  if (dockviewApi) {
    // Check if panel already exists via vizStates
    if (vizStates.has(panelId)) {
      console.log('Updating existing Image panel');
      const state = vizStates.get(panelId);
      if (state && state.element) {
        renderImage(state.element, imageUrl);
      }
    } else {
      console.log('Creating new Image panel');
      dockviewApi.addPanel({
        id: panelId,
        component: 'image',
        title: title || 'Image',
        params: { imageUrl, contentType, sourceExpr, panelId },
        position: { referencePanel: 'terminal', direction: 'right' }
      });
    }
  }
}

// Render image into element
function renderImage(element, imageUrl) {
  const img = document.createElement('img');
  img.src = imageUrl;
  img.style.cssText = 'max-width:100%;max-height:100%;object-fit:contain;display:block;margin:auto;';
  img.onerror = () => {
    element.innerHTML = '<div style="padding:20px;color:var(--fg-secondary);">Failed to load image</div>';
  };
  element.innerHTML = '';
  element.style.cssText = 'display:flex;align-items:center;justify-content:center;height:100%;background:var(--bg-primary);';
  element.appendChild(img);
}

// Vega-Lite Panel state management
const vegaLiteStates = new Map();
let currentThemeIsDark = true;  // Track current theme for Vega-Lite

// Monaco iframe registry for theme updates
const monacoIframes = new Set();

// Open Vega-Lite panel - tracks by source expression for updates
function openVegaLitePanel(title, spec, sourceExpr) {
  console.log('openVegaLitePanel called:', title, spec?.length, sourceExpr);
  const panelId = 'vega-lite-' + sanitizeForPanelId(sourceExpr);
  if (dockviewApi) {
    // Check if panel exists and update it
    if (vegaLiteStates.has(panelId)) {
      console.log('Updating existing Vega-Lite panel');
      const panelState = vegaLiteStates.get(panelId);
      if (panelState && panelState._element) {
        renderVegaLite(panelState._element, spec);
      }
    } else {
      console.log('Creating new Vega-Lite panel');
      dockviewApi.addPanel({
        id: panelId,
        component: 'vega-lite',
        title: title || 'Vega-Lite',
        params: { spec, sourceExpr, panelId },
        position: { referencePanel: 'terminal', direction: 'right' }
      });
    }
  }
}

// Render Vega-Lite spec into element
function renderVegaLite(element, spec) {
  element.innerHTML = '';
  const container = document.createElement('div');
  container.style.cssText = 'width:100%;height:100%;';
  element.appendChild(container);
  try {
    const specObj = typeof spec === 'string' ? JSON.parse(spec) : spec;
    // Set responsive sizing if not explicitly set
    if (!specObj.width) specObj.width = 'container';
    if (!specObj.height) specObj.height = 'container';
    // Add padding to prevent clipping
    if (!specObj.padding) specObj.padding = 20;
    // Set background to transparent so theme shows through
    if (!specObj.background) specObj.background = 'transparent';
    // Use vega-embed to render the chart with theme-aware config
    // Security: expressionFunctions disabled to prevent custom function injection
    // Security: ast mode uses safer AST-based expression evaluation
    // These can be overridden with --unsafe-visualizations
    const embedConfig = {
      theme: currentThemeIsDark ? 'dark' : 'excel',
      actions: { source: false, compiled: false, editor: true },
      expressionFunctions: ICL_CONFIG.unsafeVisualizations ? undefined : false,
      ast: ICL_CONFIG.unsafeVisualizations ? false : true,
      config: {
        axis: {
          labelColor: currentThemeIsDark ? '#e0e0e0' : '#333333',
          titleColor: currentThemeIsDark ? '#e0e0e0' : '#333333',
          gridColor: currentThemeIsDark ? '#444444' : '#dddddd'
        },
        legend: {
          labelColor: currentThemeIsDark ? '#e0e0e0' : '#333333',
          titleColor: currentThemeIsDark ? '#e0e0e0' : '#333333'
        },
        title: {
          color: currentThemeIsDark ? '#e0e0e0' : '#333333'
        },
        view: {
          stroke: 'transparent'
        }
      }
    };
    vegaEmbed(container, specObj, embedConfig).catch(err => {
      console.error('Vega-Lite render error:', err);
      element.innerHTML = '<div style="padding:20px;color:var(--error);">' + err.message + '</div>';
    });
  } catch (err) {
    console.error('Vega-Lite JSON parse error:', err);
    element.innerHTML = '<div style="padding:20px;color:var(--error);">Invalid Vega-Lite spec: ' + err.message + '</div>';
  }
}

// Handle Vega-Lite refresh
function handleVegaLiteRefresh(msg) {
  const panelId = msg.panelId;
  const spec = msg.spec;
  if (vegaLiteStates.has(panelId)) {
    const panelState = vegaLiteStates.get(panelId);
    if (panelState && panelState._element) {
      renderVegaLite(panelState._element, spec);
    }
  }
}

// Mermaid Panel state management
const mermaidStates = new Map();
let mermaidIdCounter = 0;

// Initialize Mermaid with theme support
mermaid.initialize({
  startOnLoad: false,
  theme: currentThemeIsDark ? 'dark' : 'default',
  securityLevel: ICL_CONFIG.unsafeVisualizations ? 'loose' : 'strict'
});

// Open Mermaid panel - tracks by source expression for updates
function openMermaidPanel(title, definition, sourceExpr) {
  console.log('openMermaidPanel called:', title, definition?.length, sourceExpr);
  const panelId = 'mermaid-' + sanitizeForPanelId(sourceExpr);
  if (dockviewApi) {
    // Check if panel exists and update it
    if (mermaidStates.has(panelId)) {
      console.log('Updating existing Mermaid panel');
      const panelState = mermaidStates.get(panelId);
      if (panelState && panelState._element) {
        renderMermaid(panelState._element, definition);
      }
    } else {
      console.log('Creating new Mermaid panel');
      dockviewApi.addPanel({
        id: panelId,
        component: 'mermaid',
        title: title || 'Mermaid',
        params: { definition, sourceExpr, panelId },
        position: { referencePanel: 'terminal', direction: 'right' }
      });
    }
  }
}

// Render Mermaid definition into element
async function renderMermaid(element, definition) {
  element.innerHTML = '';
  const container = document.createElement('div');
  container.style.cssText = 'width:100%;height:100%;display:flex;align-items:center;justify-content:center;overflow:auto;';
  element.appendChild(container);
  try {
    // Update Mermaid theme based on current mode
    mermaid.initialize({
      startOnLoad: false,
      theme: currentThemeIsDark ? 'dark' : 'default',
      securityLevel: ICL_CONFIG.unsafeVisualizations ? 'loose' : 'strict'
    });
    const uniqueId = 'mermaid-render-' + (++mermaidIdCounter);
    const { svg } = await mermaid.render(uniqueId, definition);
    container.innerHTML = svg;
    // Make SVG responsive
    const svgEl = container.querySelector('svg');
    if (svgEl) {
      svgEl.style.maxWidth = '100%';
      svgEl.style.maxHeight = '100%';
      svgEl.style.height = 'auto';
    }
  } catch (err) {
    console.error('Mermaid render error:', err);
    container.innerHTML = '<div style="padding:20px;color:var(--error);white-space:pre-wrap;">Mermaid error: ' + err.message + '</div>';
  }
}

// Handle Mermaid refresh
function handleMermaidRefresh(msg) {
  const panelId = msg.panelId;
  const definition = msg.definition;
  if (mermaidStates.has(panelId)) {
    const panelState = mermaidStates.get(panelId);
    if (panelState && panelState._element) {
      renderMermaid(panelState._element, definition);
    }
  }
}

// ========================================
// Regexp Railroad Diagram Visualization
// ========================================

const regexpStates = new Map();

// Open Regexp panel - tracks by source expression for updates
function openRegexpPanel(title, pattern, sourceExpr) {
  console.log('openRegexpPanel called:', title, pattern, sourceExpr);
  const panelId = 'regexp-' + sanitizeForPanelId(sourceExpr);
  if (dockviewApi) {
    // Check if panel exists and update it
    if (regexpStates.has(panelId)) {
      console.log('Updating existing Regexp panel');
      const panelState = regexpStates.get(panelId);
      if (panelState && panelState._element) {
        renderRegexp(panelState._element, pattern);
      }
    } else {
      console.log('Creating new Regexp panel');
      dockviewApi.addPanel({
        id: panelId,
        component: 'regexp',
        title: title || 'Regexp',
        params: { pattern, sourceExpr, panelId },
        position: { referencePanel: 'terminal', direction: 'right' }
      });
    }
  }
}

// Render regexp pattern into element using Regulex
function renderRegexp(element, pattern) {
  element.innerHTML = '';
  const container = document.createElement('div');
  container.style.cssText = 'width:100%;height:100%;overflow:auto;background:white;display:flex;align-items:center;justify-content:center;padding:20px;box-sizing:border-box;';
  element.appendChild(container);

  // Create a unique container for Raphael with explicit dimensions
  const graphContainer = document.createElement('div');
  graphContainer.id = 'regexp-graph-' + Date.now();
  graphContainer.style.cssText = 'width:800px;height:400px;';
  container.appendChild(graphContainer);

  // Wait for layout to complete before rendering
  requestAnimationFrame(() => {
    try {
      // Get Regulex modules via AMD require
      const Raphael = require('regulex').Raphael;
      const parse = require('regulex').parse;
      const visualize = require('regulex').visualize;

      // Parse the regex pattern
      const ast = parse(pattern);

      // Create Raphael paper matching container dimensions
      const paper = Raphael(graphContainer, 800, 400);

      // Visualize the regex - empty flags for now
      visualize(ast, '', paper);

      // Wait another frame for SVG to settle
      requestAnimationFrame(() => {
        const svgEl = graphContainer.querySelector('svg');
        if (svgEl) {
          // Get the bounding box of all rendered content
          const bbox = svgEl.getBBox();

          // Set the SVG size to exactly fit the content plus padding
          const padding = 20;
          const svgWidth = bbox.x + bbox.width + padding;
          const svgHeight = bbox.y + bbox.height + padding;

          svgEl.setAttribute('width', svgWidth);
          svgEl.setAttribute('height', svgHeight);
          graphContainer.style.width = svgWidth + 'px';
          graphContainer.style.height = svgHeight + 'px';

          // Scale the container to fit the panel while maintaining aspect ratio
          const panelWidth = element.clientWidth - 40;
          const panelHeight = element.clientHeight - 40;
          const scaleX = panelWidth / svgWidth;
          const scaleY = panelHeight / svgHeight;
          const scale = Math.min(scaleX, scaleY, 3);

          if (scale < 1) {
            // Content is larger than panel, scale down
            graphContainer.style.transform = `scale(${scale})`;
            graphContainer.style.transformOrigin = 'center center';
          } else if (scale > 1) {
            // Content is smaller, scale up (but not too much)
            graphContainer.style.transform = `scale(${Math.min(scale, 2)})`;
            graphContainer.style.transformOrigin = 'center center';
          }
        }
      });
    } catch (err) {
      console.error('Regexp render error:', err);
      container.innerHTML = '<div style="padding:20px;color:#c00;white-space:pre-wrap;">Regexp error: ' + err.message + '</div>';
    }
  });
}

// Handle Regexp refresh
function handleRegexpRefresh(msg) {
  const panelId = msg.panelId;
  const pattern = msg.pattern;
  if (regexpStates.has(panelId)) {
    const panelState = regexpStates.get(panelId);
    if (panelState && panelState._element) {
      renderRegexp(panelState._element, pattern);
    }
  }
}

// Refresh all visualization panels
function refreshAllVisualizations() {
  // Refresh class graph panels (specialized - not expression-based)
  graphvizStates.forEach((panel, panelId) => {
    if (panel._className && panel._packageName) {
      ws.send(JSON.stringify({
        type: 'get-class-graph',
        className: panel._className,
        packageName: panel._packageName,
        panelId: panelId
      }));
    }
  });
  // Refresh Venn diagram panels (specialized - multi-expression)
  vennStates.forEach((panel, panelId) => {
    if (panel._sourceExpr) {
      ws.send(JSON.stringify({
        type: 'refresh-venn',
        sourceExpr: panel._sourceExpr,
        panelId: panelId
      }));
    }
  });
  // Unified refresh for all viz panels (HTML, SVG, hash-table, etc.)
  // This handles type changes automatically
  vizStates.forEach((state, panelId) => {
    if (state.sourceExpr) {
      ws.send(JSON.stringify({
        type: 'refresh-viz',
        sourceExpr: state.sourceExpr,
        panelId: panelId
      }));
    }
  });
}

// Handle hash-table refresh data
function handleHashtableRefresh(msg) {
  const panelId = msg.panelId;
  const panel = hashtableStates.get(panelId);
  if (panel) {
    if (msg.error) {
      console.log('Hash-table refresh error:', msg.error);
    } else {
      panel.updateData(msg.count, msg.entries);
    }
  }
}

// Handle HTML panel refresh data
function handleHtmlRefresh(msg) {
  const panelId = msg.panelId;
  const panel = htmlStates.get(panelId);
  if (panel) {
    if (msg.error) {
      console.log('HTML refresh error:', msg.error);
    } else if (panel._iframe) {
      panel._iframe.srcdoc = msg.content;
    }
  }
}

// Handle SVG panel refresh data
function handleSvgRefresh(msg) {
  const panelId = msg.panelId;
  const panel = svgStates.get(panelId);
  if (panel) {
    if (msg.error) {
      console.log('SVG refresh error:', msg.error);
    } else {
      panel._element.innerHTML = msg.content;
      const svg = panel._element.querySelector('svg');
      if (svg) {
        svg.style.maxWidth = '100%';
        svg.style.maxHeight = '100%';
        svg.style.width = 'auto';
        svg.style.height = 'auto';
      }
    }
  }
}

// Handle visualization type change - close old panel and open new one
function handleVizTypeChanged(msg) {
  const oldPanelId = msg.panelId;
  const sourceExpr = msg.sourceExpr;
  const newType = msg.newType;
  console.log('handleVizTypeChanged:', oldPanelId, '->', newType, 'for', sourceExpr);

  // Remove old panel from state maps
  if (htmlStates.has(oldPanelId)) {
    htmlStates.delete(oldPanelId);
  }
  if (svgStates.has(oldPanelId)) {
    svgStates.delete(oldPanelId);
  }

  // Close old panel in dockview
  const oldPanel = dockviewApi?.getPanel(oldPanelId);
  if (oldPanel) {
    dockviewApi.removePanel(oldPanel);
  }

  // Open new panel of appropriate type
  if (newType === 'hash-table') {
    openHashTablePanel(sourceExpr, msg.count, msg.entries, sourceExpr);
  }
  // Add other type handlers as needed
}

// Unified viz refresh - updates panel content based on current value type
function handleVizRefresh(msg) {
  const panelId = msg.panelId;
  const sourceExpr = msg.sourceExpr;
  const vizType = msg.vizType;
  const state = vizStates.get(panelId);

  if (!state) {
    console.log('handleVizRefresh: panel not found:', panelId);
    return;
  }

  console.log('handleVizRefresh:', panelId, 'rendering as:', vizType);

  // Update the panel's content based on current type
  // The panel stays the same, only its content changes
  state.type = vizType;  // Track current type

  if (state.element) {
    state.element.innerHTML = '';  // Clear existing content

    switch (vizType) {
      case 'html': {
        const iframe = document.createElement('iframe');
        iframe.style.cssText = 'width:100%;height:100%;border:none;background:white;';
        iframe.sandbox = 'allow-same-origin';
        iframe.srcdoc = msg.content;
        state.element.appendChild(iframe);
        break;
      }
      case 'svg': {
        state.element.innerHTML = msg.content;
        state.element.style.display = 'flex';
        state.element.style.alignItems = 'center';
        state.element.style.justifyContent = 'center';
        const svg = state.element.querySelector('svg');
        if (svg) {
          svg.style.maxWidth = '100%';
          svg.style.maxHeight = '100%';
        }
        break;
      }
      case 'json': {
        renderJson(state.element, msg.content);
        break;
      }
      case 'hash-table': {
        renderHashTable(state.element, msg.count, msg.entries);
        break;
      }
      case 'image': {
        renderImage(state.element, msg.imageUrl);
        break;
      }
      default:
        state.element.innerHTML = '<div style="padding:20px;color:var(--fg-secondary);">Unknown type: ' + vizType + '</div>';
    }
  }
}

// Render hash table into element
function renderHashTable(element, count, entries) {
  let html = '<div style="font-family:monospace;font-size:13px;color:var(--fg-primary);padding:10px;overflow:auto;height:100%;">';
  html += '<div style="margin-bottom:8px;color:var(--fg-secondary);">Hash Table (' + count + ' entries)</div>';
  html += '<table style="border-collapse:collapse;width:100%;">';
  html += '<thead><tr style="background:var(--bg-tertiary);">';
  html += '<th style="text-align:left;padding:4px 8px;border:1px solid var(--border);">Key</th>';
  html += '<th style="text-align:left;padding:4px 8px;border:1px solid var(--border);">Value</th>';
  html += '</tr></thead><tbody>';
  (entries || []).forEach(([key, value]) => {
    const escKey = (key || '').replace(/</g, '&lt;').replace(/>/g, '&gt;');
    const escValue = (value || '').replace(/</g, '&lt;').replace(/>/g, '&gt;');
    html += '<tr style="background:var(--bg-secondary);">';
    html += '<td style="padding:4px 8px;border:1px solid var(--border);white-space:pre-wrap;">' + escKey + '</td>';
    html += '<td style="padding:4px 8px;border:1px solid var(--border);white-space:pre-wrap;">' + escValue + '</td>';
    html += '</tr>';
  });
  if (count > (entries || []).length) {
    html += '<tr style="background:var(--bg-secondary);color:var(--fg-secondary);">';
    html += '<td colspan="2" style="padding:4px 8px;border:1px solid var(--border);text-align:center;font-style:italic;">';
    html += '... and ' + (count - entries.length) + ' more entries</td></tr>';
  }
  html += '</tbody></table></div>';
  element.innerHTML = html;
}

// Render code/text with syntax highlighting via Monaco colorizer
function renderJson(element, content) {
  let formatted = content;
  let language = 'plaintext';

  // Try to pretty-print if it looks like JSON
  try {
    const parsed = JSON.parse(content);
    formatted = JSON.stringify(parsed, null, 2);
    language = 'json';
  } catch (e) {
    // Not valid JSON - try to detect language
    if (content.trim().startsWith('(') || content.includes('defun') || content.includes('defmacro')) {
      language = 'scheme';  // Close to Lisp
    }
  }

  // Create container
  const pre = document.createElement('pre');
  pre.style.cssText = 'margin:0;padding:12px;font-family:monospace;font-size:13px;' +
    'background:var(--bg-primary);overflow:auto;height:100%;';
  element.innerHTML = '';
  element.appendChild(pre);

  // Use Monaco colorizer if available
  if (monacoReady && typeof monaco !== 'undefined') {
    const isDark = document.body.classList.contains('dark');
    monaco.editor.setTheme(isDark ? 'vs-dark' : 'vs');
    monaco.editor.colorize(formatted, language, {}).then(html => {
      pre.innerHTML = html;
    });
  } else {
    // Fallback: plain text until Monaco loads
    pre.textContent = formatted;
    // Retry when Monaco is ready
    initMonaco().then(() => {
      if (typeof monaco !== 'undefined') {
        const isDark = document.body.classList.contains('dark');
        monaco.editor.setTheme(isDark ? 'vs-dark' : 'vs');
        monaco.editor.colorize(formatted, language, {}).then(html => {
          pre.innerHTML = html;
        });
      }
    });
  }
}

// Open Venn diagram panel
let vennCounter = 0;
const vennStates = new Map();  // panelId -> VennPanel instance

function openVennPanel(setNames, setMembers, sourceExpr) {
  console.log('openVennPanel called:', setNames, setMembers?.map(m => m?.length), sourceExpr);
  const panelId = 'venn-' + (++vennCounter);
  const title = setNames.length === 1 ? setNames[0] : 'Venn: ' + setNames.join(' ∩ ');
  if (dockviewApi) {
    dockviewApi.addPanel({
      id: panelId,
      component: 'venn',
      title: title,
      params: { panelId, setNames, setMembers, sourceExpr },
      position: { referencePanel: 'terminal', direction: 'right' }
    });
  } else {
    console.error('dockviewApi not available');
  }
}

// Handle Venn refresh data
function handleVennRefresh(msg) {
  const panelId = msg.panelId;
  const panel = vennStates.get(panelId);
  if (panel) {
    if (msg.error) {
      console.log('Venn refresh error:', msg.error);
    } else {
      panel.updateData(msg.setMembers);
    }
  }
}

// Theme application
function applyTheme(themeData) {
  if (!themeData) return;
  console.log('Applying theme:', themeData.displayName);

  // Apply CSS variables by injecting a style tag
  let styleEl = document.getElementById('theme-styles');
  if (!styleEl) {
    styleEl = document.createElement('style');
    styleEl.id = 'theme-styles';
    document.head.appendChild(styleEl);
  }
  styleEl.textContent = themeData.css;

  // Update body styles using CSS variables
  document.body.style.background = 'var(--bg-primary)';
  document.body.style.color = 'var(--fg-primary)';

  // Apply xterm.js theme if terminal exists, otherwise store for later
  if (themeData.xterm) {
    if (terminal) {
      const xtermTheme = {};
      Object.entries(themeData.xterm).forEach(([k, v]) => {
        xtermTheme[k] = v;
      });
      terminal.options.theme = xtermTheme;
    } else {
      // Terminal not ready yet, store theme for when it's created
      pendingTheme = themeData;
    }
  }

  // Apply dockview theme class
  if (themeData.dockviewTheme && dockviewApi) {
    const container = document.getElementById('layout-container');
    container.className = themeData.dockviewTheme;
  }

  // Re-render all graphviz panels to apply new theme colors
  graphvizStates.forEach((panel) => {
    if (panel._nodes && panel._nodes.size > 0) {
      panel._render();
    }
  });

  // Detect dark/light mode from theme (darkP is sent from Lisp)
  if (themeData.darkP !== undefined) {
    const isDark = themeData.darkP;
    currentThemeIsDark = isDark;

    // Set body class for dark mode detection
    if (isDark) {
      document.body.classList.add('dark');
    } else {
      document.body.classList.remove('dark');
    }

    // Re-render all Vega-Lite panels to apply new theme
    vegaLiteStates.forEach((panel) => {
      if (panel.reRender) {
        panel.reRender();
      }
    });

    // Re-render all Mermaid panels to apply new theme
    mermaidStates.forEach((panel) => {
      if (panel.reRender) {
        panel.reRender();
      }
    });

    // Update all Monaco iframes with new theme
    monacoIframes.forEach((iframe) => {
      if (iframe.contentWindow) {
        iframe.contentWindow.postMessage({
          type: 'set-theme',
          theme: isDark ? 'dark' : 'light'
        }, '*');
      }
    });
  }
}

// Send dark mode preference on connect
function sendDarkModePreference() {
  const isDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
  ws.send(JSON.stringify({type: 'dark-mode-preference', dark: isDark}));
}

// Listen for system dark mode changes
window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', (e) => {
  if (ws.readyState === WebSocket.OPEN) {
    ws.send(JSON.stringify({type: 'dark-mode-preference', dark: e.matches}));
  }
});

function handleSymbolClicked(msg) {
  // Clear filters when navigating via REPL symbol click
  const pkgFilter = document.getElementById('package-filter');
  const symFilter = document.getElementById('symbol-filter');
  if (pkgFilter) pkgFilter.value = '';
  if (symFilter) symFilter.value = '';

  // Update Packages panel - select the package
  selectedPackage = msg.package;
  renderPackages();
  // Scroll package into view
  setTimeout(() => {
    const pkgEl = document.querySelector('#package-list .selected');
    if (pkgEl) pkgEl.scrollIntoView({block: 'center'});
  }, 50);

  // Update Symbols panel - set symbols and select the symbol
  symbols = msg.symbols || [];
  selectedSymbol = msg.symbol;
  renderSymbols();
  // Scroll symbol into view
  setTimeout(() => {
    const symEl = document.querySelector('#symbol-list .selected');
    if (symEl) symEl.scrollIntoView({block: 'center'});
  }, 50);

  // Update Symbol Info panel - use same rendering as Symbol list click
  if (msg.symbolInfo) {
    renderSymbolInfo(msg.symbolInfo);
  }
  // Activate the Symbol Info panel to bring it to the surface
  // (only for REPL clicks, not class graph clicks which should stay in place)
  if (dockviewApi && msg.source !== 'class-graph') {
    const panel = dockviewApi.getPanel('inspector');
    if (panel) panel.api.setActive();
  }
  // Restore focus to terminal after all updates (only for REPL clicks)
  if (msg.source !== 'class-graph') {
    setTimeout(() => { if (terminal) terminal.focus(); }, 100);
  }
}

// Panel rendering functions (using data attributes for CSP compliance)
function renderPackages(filter = '') {
  const el = document.getElementById('package-list');
  if (!el) return;
  const f = filter.toLowerCase();
  el.innerHTML = packages
    .filter(p => !f || p.toLowerCase().includes(f))
    .map(p => {
      const selected = p === selectedPackage ? 'selected' : '';
      return `<div class="list-item ${selected}" data-package="${p.replace(/"/g, '&quot;')}"><span>${p}</span></div>`;
    }).join('');
}

function renderSymbols(filter = '') {
  const el = document.getElementById('symbol-list');
  if (!el) return;
  if (!selectedPackage) {
    el.innerHTML = '<div class="empty-state">Select a package to see symbols</div>';
    return;
  }
  const f = filter.toLowerCase();
  const filtered = (symbols || []).filter(s => !f || s[0].toLowerCase().includes(f));
  if (filtered.length === 0) {
    el.innerHTML = '<div class="empty-state">' + (f ? 'No matching symbols' : 'No symbols in package') + '</div>';
    return;
  }
  el.innerHTML = filtered
    .map(s => {
      const [name] = s;
      const selected = (selectedSymbol && name.toUpperCase() === selectedSymbol.toUpperCase()) ? 'selected' : '';
      return `<div class="list-item ${selected}" data-symbol="${name.replace(/"/g, '&quot;')}">${name}</div>`;
    }).join('');
}

// Event delegation for package/symbol lists and inspect links (CSP-compliant)
document.addEventListener('click', (e) => {
  // Package selection
  const pkgItem = e.target.closest('[data-package]');
  if (pkgItem) {
    selectPackage(pkgItem.dataset.package);
    return;
  }
  // Symbol selection
  const symItem = e.target.closest('[data-symbol]');
  if (symItem) {
    selectSymbol(symItem.dataset.symbol);
    return;
  }
  // Inspect class
  const inspectClassEl = e.target.closest('[data-inspect-class]');
  if (inspectClassEl) {
    inspectClass(inspectClassEl.dataset.inspectClass, inspectClassEl.dataset.pkg);
    return;
  }
  // Inspect function
  const inspectFnEl = e.target.closest('[data-inspect-function]');
  if (inspectFnEl) {
    inspectFunction(inspectFnEl.dataset.inspectFunction, inspectFnEl.dataset.pkg, inspectFnEl.dataset.fnType);
    return;
  }
  // Inspect variable
  const inspectVarEl = e.target.closest('[data-inspect-variable]');
  if (inspectVarEl) {
    inspectVariable(inspectVarEl.dataset.inspectVariable, inspectVarEl.dataset.pkg);
    return;
  }
  // Expand/collapse toggle in inspector tree
  const expandToggleEl = e.target.closest('.expand-toggle');
  if (expandToggleEl) {
    const entryId = expandToggleEl.dataset.entryId;
    const actionIndex = parseInt(expandToggleEl.dataset.action, 10);
    const panelId = expandToggleEl.dataset.panelId;
    toggleInspectorExpansion(panelId, entryId, actionIndex);
    return;
  }
  // Inspector action (drill down into object) - legacy handler, kept for compatibility
  const inspectActionEl = e.target.closest('[data-inspect-action]');
  if (inspectActionEl) {
    inspectAction(parseInt(inspectActionEl.dataset.inspectAction, 10), inspectActionEl.dataset.panelId);
    return;
  }
  // Inspector back button
  const inspectBackEl = e.target.closest('[data-inspect-back]');
  if (inspectBackEl) {
    inspectBack(inspectBackEl.dataset.inspectBack);
    return;
  }
  // Phase 6: Navigation buttons
  const navUpEl = e.target.closest('[data-nav-up]');
  if (navUpEl && !navUpEl.disabled) {
    inspectNav(navUpEl.dataset.navUp, 'up');
    return;
  }
  const navCarEl = e.target.closest('[data-nav-car]');
  if (navCarEl && !navCarEl.disabled) {
    inspectNav(navCarEl.dataset.navCar, 'car');
    return;
  }
  const navCdrEl = e.target.closest('[data-nav-cdr]');
  if (navCdrEl && !navCdrEl.disabled) {
    inspectNav(navCdrEl.dataset.navCdr, 'cdr');
    return;
  }
  const navLeftEl = e.target.closest('[data-nav-left]');
  if (navLeftEl && !navLeftEl.disabled) {
    inspectNav(navLeftEl.dataset.navLeft, 'left');
    return;
  }
  const navRightEl = e.target.closest('[data-nav-right]');
  if (navRightEl && !navRightEl.disabled) {
    inspectNav(navRightEl.dataset.navRight, 'right');
    return;
  }
  const histBackEl = e.target.closest('[data-history-back]');
  if (histBackEl && !histBackEl.disabled) {
    inspectHistoryNav(histBackEl.dataset.historyBack, 'back');
    return;
  }
  const histForwardEl = e.target.closest('[data-history-forward]');
  if (histForwardEl && !histForwardEl.disabled) {
    inspectHistoryNav(histForwardEl.dataset.historyForward, 'forward');
    return;
  }
});

// Handle Enter key on inspector eval input
document.addEventListener('keydown', (e) => {
  if (e.key === 'Enter') {
    const evalInput = e.target.closest('[data-eval-input]');
    if (evalInput) {
      e.preventDefault();
      const panelId = evalInput.dataset.evalInput;
      const form = evalInput.value.trim();
      if (form) {
        inspectorEval(panelId, form);
        evalInput.value = '';
      }
    }
  }
});

// Store current symbol info for inspect links
let currentSymbolInfo = null;

function inspectClass(name, pkg) {
  const prefix = pkg ? (pkg + "::") : "";
  openInspector("(find-class '" + prefix + name + ")", pkg);
}

function inspectFunction(name, pkg, type) {
  const prefix = pkg ? (pkg + "::") : "";
  if (type === 'macro') {
    openInspector("(macro-function '" + prefix + name + ")", pkg);
  } else {
    openInspector("#'" + prefix + name, pkg);
  }
}

function inspectVariable(name, pkg) {
  const prefix = pkg ? (pkg + "::") : "";
  openInspector(prefix + name, pkg);
}

function renderSymbolInfo(info) {
  const el = document.getElementById('detail-content');
  if (!el || !info) return;
  currentSymbolInfo = info;
  const name = info.name || 'Symbol';
  const pkg = info.package || selectedPackage || null;
  const pkgStr = pkg || '';
  let html = `<strong>${name}</strong>\n\n`;

  // Class binding
  if (info.class) {
    html += `<span class='binding-header'>[Class]</span>`;
    html += `<span class='inspect-link' data-inspect-class="${name}" data-pkg="${pkgStr}">[Inspect]</span>\n`;
    if (info.class.superclasses && info.class.superclasses.length > 0) {
      html += `<strong>Superclasses:</strong> ${info.class.superclasses.join(', ')}\n`;
    }
    if (info.class.slots && info.class.slots.length > 0) {
      html += `<strong>Slots:</strong> ${info.class.slots.join(', ')}\n`;
    }
    html += '\n';
  }

  // Function/macro/generic binding
  if (info.function) {
    const typeLabel = {function: 'Function', macro: 'Macro', generic: 'Generic Function'}[info.function.type] || 'Function';
    const fnType = info.function.type || 'function';
    html += `<span class='binding-header'>[${typeLabel}]</span>`;
    html += `<span class='inspect-link' data-inspect-function="${name}" data-pkg="${pkgStr}" data-fn-type="${fnType}">[Inspect]</span>\n`;
    if (info.function.arglist) html += `<strong>Arguments:</strong> ${info.function.arglist}\n`;
    if (info.function.documentation) html += `<strong>Documentation:</strong>\n${info.function.documentation}\n`;
    html += '\n';
  }

  // Variable binding
  if (info.variable) {
    const varType = info.variable.constantp ? 'Constant' : 'Variable';
    html += `<span class='binding-header'>[${varType}]</span>`;
    html += `<span class='inspect-link' data-inspect-variable="${name}" data-pkg="${pkgStr}">[Inspect]</span>\n`;
    if (info.variable.value) html += `<strong>Value:</strong> ${info.variable.value}\n`;
    if (info.variable.documentation) html += `<strong>Documentation:</strong>\n${info.variable.documentation}\n`;
    html += '\n';
  }

  // Special operator
  if (info['special-operator']) {
    html += `<span class='binding-header'>[Special Operator]</span>\n`;
    html += 'Built-in special form\n\n';
  }

  // If no bindings found
  if (!info.class && !info.function && !info.variable && !info['special-operator']) {
    html += '<em>Unbound symbol</em>\n';
  }

  el.innerHTML = html;
}

function renderInspection(msg, panelId) {
  console.log('renderInspection panelId:', panelId, 'states:', inspectorStates);
  const state = inspectorStates.get(panelId);
  if (!state) {
    console.log('No state found for panelId:', panelId, 'stashing pending inspection');
    pendingInspections.set(panelId, msg);
    return;
  }
  console.log('state found:', state);

  const { element, header, nav } = state;
  if (!element) return;

  // Track depth for Back button
  if (msg.action === 'push') state.depth++;
  else if (msg.action === 'pop') state.depth = Math.max(0, state.depth - 1);
  else state.depth = 1;

  // Show/hide Back button
  if (header) header.style.display = state.depth > 1 ? 'block' : 'none';

  // Update navigation button states and store action indices
  if (nav) {
    const navUp = nav.querySelector('[data-nav-up]');
    const navCar = nav.querySelector('[data-nav-car]');
    const navCdr = nav.querySelector('[data-nav-cdr]');

    // Enable Up button when we have depth (can go back)
    if (navUp) navUp.disabled = state.depth <= 1;

    // Store and enable car/cdr based on server-provided action indices
    const carAction = msg['car-action'];
    const cdrAction = msg['cdr-action'];
    state.carAction = carAction;
    state.cdrAction = cdrAction;

    if (navCar) navCar.disabled = (typeof carAction !== 'number');
    if (navCdr) navCdr.disabled = (typeof cdrAction !== 'number');
  }

  // Store root entries for tree expansion
  state.rootTitle = msg.title || 'Object';
  state.rootEntries = msg.entries || [];
  state.expansions = state.expansions || new Map();  // Preserve existing expansions on refresh

  // When replacing the root (new inspection), clear old expansions
  if (msg.action === 'new') {
    state.expansions.clear();
  }

  // Render as expandable tree
  renderInspectorTree(panelId);
}

// Generate unique entry ID based on depth and position
function generateEntryId(prefix, index) {
  return prefix + '-' + index;
}

// Render the inspector as a tree with expand/collapse
function renderInspectorTree(panelId) {
  const state = inspectorStates.get(panelId);
  if (!state || !state.element) return;

  let html = `<div class="inspector-tree">`;
  html += `<div class="inspector-root"><strong>${escapeHtml(state.rootTitle)}</strong></div>`;
  html += renderInspectorEntries(state.rootEntries, state.expansions, panelId, 0, 'root');
  html += `</div>`;
  state.element.innerHTML = html;
}

// Recursively render entries with indentation
function renderInspectorEntries(entries, expansions, panelId, depth, parentId) {
  if (!entries || entries.length === 0) return '';

  let html = '';
  entries.forEach((entry, index) => {
    const [label, value, action] = entry;
    const entryId = generateEntryId(parentId, index);
    const expandable = action !== null && action !== undefined;
    const expansion = expansions.get(entryId);
    const isExpanded = expansion?.expanded;

    html += `<div class="inspector-entry">`;

    if (expandable) {
      const icon = isExpanded ? '▼' : '▶';
      html += `<span class="expand-toggle" data-entry-id="${entryId}" data-action="${action}" data-panel-id="${panelId}">${icon}</span>`;
    } else {
      html += `<span class="expand-placeholder"></span>`;
    }

    html += `<span class="entry-label">${escapeHtml(label)}</span> `;
    html += `<span class="entry-value">${escapeHtml(String(value))}</span>`;
    html += `</div>`;

    // Render children if expanded with colored depth boxes
    if (isExpanded && expansion.children && expansion.children.length > 0) {
      const childDepth = (depth + 1) % 6;  // Cycle through 6 colors
      html += `<div class="inspector-children" data-depth="${childDepth}">`;
      // Show type title if available
      if (expansion.title) {
        html += `<div class="inspector-child-title">${escapeHtml(expansion.title)}</div>`;
      }
      html += renderInspectorEntries(expansion.children, expansions, panelId, depth + 1, entryId);
      html += `</div>`;
    }
  });

  return html;
}

// Handle expand/collapse toggle
function toggleInspectorExpansion(panelId, entryId, actionIndex) {
  const state = inspectorStates.get(panelId);
  if (!state) return;

  const expansion = state.expansions.get(entryId);

  if (expansion?.expanded) {
    // Collapse: just toggle the flag, keep cached children
    expansion.expanded = false;
    renderInspectorTree(panelId);
  } else if (expansion?.children) {
    // Expand with cached children
    expansion.expanded = true;
    renderInspectorTree(panelId);
  } else {
    // Fetch children from server
    ws.send(JSON.stringify({
      type: 'inspector-expand',
      panelId: panelId,
      actionIndex: actionIndex,
      entryId: entryId
    }));
  }
}

// Handle expansion result from server
function handleInspectorExpandResult(msg) {
  const panelId = msg.panelId;
  const entryId = msg.entryId;
  const state = inspectorStates.get(panelId);

  if (!state) {
    console.log('handleInspectorExpandResult: no state for panel', panelId);
    return;
  }

  if (msg.error) {
    console.log('handleInspectorExpandResult: error', msg.error);
    return;
  }

  // Store children and mark expanded
  state.expansions.set(entryId, {
    expanded: true,
    children: msg.entries || [],
    title: msg.title
  });

  // Re-render the tree
  renderInspectorTree(panelId);
}

// Escape HTML for safe display (only < and > to match original behavior)
function escapeHtml(text) {
  if (!text) return '';
  return String(text).replace(/</g, '&lt;').replace(/>/g, '&gt;');
}

function selectPackage(pkg) {
  selectedPackage = pkg;
  selectedSymbol = null;  // Clear symbol selection when package changes
  renderPackages(document.getElementById('package-filter')?.value || '');
  ws.send(JSON.stringify({type: 'get-symbols', package: pkg}));
}

function selectSymbol(name) {
  selectedSymbol = name;
  renderSymbols(document.getElementById('symbol-filter')?.value || '');
  ws.send(JSON.stringify({type: 'get-symbol-info', package: selectedPackage, name: name}));
  // Activate the Symbol Info panel to bring it to the surface
  if (dockviewApi) {
    const panel = dockviewApi.getPanel('inspector');
    if (panel) panel.api.setActive();
  }
}

function inspectAction(action, panelId) {
  ws.send(JSON.stringify({type: 'inspector-action', index: action, panelId: panelId}));
}

function inspectBack(panelId) {
  ws.send(JSON.stringify({type: 'inspector-pop', panelId: panelId}));
}

// Phase 6: Advanced navigation functions
function inspectNav(panelId, direction) {
  const state = inspectorStates.get(panelId);
  const msg = {type: 'inspector-nav', panelId: panelId, direction: direction};

  // For car/cdr, include the action index from stored state
  if (state) {
    if (direction === 'car' && state.carAction !== undefined) {
      msg.actionIndex = state.carAction;
    } else if (direction === 'cdr' && state.cdrAction !== undefined) {
      msg.actionIndex = state.cdrAction;
    }
  }

  ws.send(JSON.stringify(msg));
}

function inspectHistoryNav(panelId, direction) {
  ws.send(JSON.stringify({type: 'inspector-history', panelId: panelId, direction: direction}));
}

function updateInspectorBreadcrumb(panelId, path) {
  const state = inspectorStates.get(panelId);
  if (state && state.breadcrumb) {
    state.path = path || ['root'];
    state.breadcrumb.textContent = state.path.join(' > ');
  }
}

function updateInspectorSiblingPosition(panelId, current, total) {
  const state = inspectorStates.get(panelId);
  if (state && state.position) {
    if (current && total && total > 1) {
      state.position.textContent = `[${current}/${total}]`;
    } else {
      state.position.textContent = '';
    }
  }
}

// Inspector context evaluation
function inspectorEval(panelId, form) {
  if (!form || !form.trim()) return;
  ws.send(JSON.stringify({type: 'inspector-eval', panelId: panelId, form: form}));
}

function handleInspectorEvalResult(msg) {
  const panelId = msg.panelId;
  const result = msg.result;
  const resultEl = document.getElementById('eval-result-' + panelId);
  if (resultEl) {
    resultEl.textContent = '=> ' + result;
    // Fade result after 5 seconds
    resultEl.classList.remove('fade-out');
    setTimeout(() => {
      resultEl.classList.add('fade-out');
    }, 5000);
  }
}

function openInspector(form, pkg) {
  const panelId = 'inspector-' + (++inspectorCounter);
  console.log('openInspector:', form, panelId);
  // Create new inspector panel in Dockview
  if (dockviewApi) {
    dockviewApi.addPanel({
      id: panelId,
      component: 'dynamic-inspector',
      title: 'Inspector',
      params: { panelId: panelId },
      position: { referencePanel: 'inspector', direction: 'within' }
    });
  }
  // Send inspect request to server after panel creation to reduce races
  ws.send(JSON.stringify({type: 'inspect', form: form, panelId: panelId, package: pkg}));
}

// Lisp symbol characters: alphanumeric, -, *, +, /, <, >, =, ?, !, $, %, &, :
const isSymbolChar = (c) => /[a-zA-Z0-9\-*+/<>=?!$%&:_]/.test(c);

// Extract Lisp symbol at position in terminal buffer
function getSymbolAtPosition(col, row) {
  const info = getSymbolBounds(col, row);
  return info ? info.symbol : null;
}

// Get symbol bounds (start, end, symbol) at position
function getSymbolBounds(col, row) {
  if (!terminal) return null;
  const buffer = terminal.buffer.active;
  const line = buffer.getLine(row);
  if (!line) return null;
  const lineText = line.translateToString();

  if (col >= lineText.length || !isSymbolChar(lineText[col])) return null;

  // Find symbol boundaries
  let start = col, end = col;
  while (start > 0 && isSymbolChar(lineText[start - 1])) start--;
  while (end < lineText.length && isSymbolChar(lineText[end])) end++;

  const symbol = lineText.substring(start, end).trim();
  return symbol.length > 0 ? { start, end, symbol } : null;
}

function inspectSymbolAtCursor(e) {
  if (!terminal) return;
  const rect = terminal.element.getBoundingClientRect();
  const renderer = terminal._core._renderService.dimensions;
  const col = Math.floor((e.clientX - rect.left) / renderer.css.cell.width);
  const row = Math.floor((e.clientY - rect.top) / renderer.css.cell.height) + terminal.buffer.active.viewportY;

  const symbol = getSymbolAtPosition(col, row);
  if (symbol) {
    ws.send(JSON.stringify({type: 'inspect', form: symbol}));
  }
}

// Panel classes for Dockview
class PackagesPanel {
  constructor() {
    this._element = document.createElement('div');
    this._element.className = 'panel';
    this._element.innerHTML = `
      <div class='panel-header'>
        <input id='package-filter' placeholder='Filter packages...' oninput='renderPackages(this.value)'>
      </div>
      <div class='panel-content' id='package-list'></div>`;
  }
  get element() { return this._element; }
  init(params) { setTimeout(renderPackages, 100); }
}

class SymbolsPanel {
  constructor() {
    this._element = document.createElement('div');
    this._element.className = 'panel';
    this._element.innerHTML = `
      <div class='panel-header'>
        <input id='symbol-filter' placeholder='Filter symbols...' oninput='renderSymbols(this.value)'>
      </div>
      <div class='panel-content' id='symbol-list'></div>`;
  }
  get element() { return this._element; }
  init(params) {}
}

class InspectorPanel {
  constructor() {
    this._element = document.createElement('div');
    this._element.className = 'panel';
    this._element.innerHTML = `
      <div class='panel-content detail-content' id='detail-content'>
        <div class="empty-state">Select a symbol to see details</div>
      </div>`;
  }
  get element() { return this._element; }
  init(params) {}
}

class SpeedscopePanel {
  constructor() {
    this._element = document.createElement('div');
    this._element.style.cssText = 'position:absolute;top:0;left:0;right:0;bottom:0;';
  }
  get element() { return this._element; }
  init(params) {
    const profileUrl = params.params?.profileUrl || '';
    const iframeSrc = '/speedscope/index.html#profileURL=' + encodeURIComponent(profileUrl);
    this._element.innerHTML = '<iframe src="' + iframeSrc + '" style="width:100%;height:100%;border:none;"></iframe>';
  }
}

class CoveragePanel {
  constructor() {
    this._element = document.createElement('div');
    this._element.style.cssText = 'position:absolute;top:0;left:0;right:0;bottom:0;';
  }
  get element() { return this._element; }
  init(params) {
    this._element.innerHTML = '<iframe src="/coverage/cover-index.html" style="width:100%;height:100%;border:none;background:white;"></iframe>';
  }
}

// Monaco editor coverage panel with real Monaco editor and line decorations
// Monaco runs entirely inside an iframe to isolate from regulex.js AMD conflicts

class MonacoCoveragePanel {
  constructor() {
    this._element = document.createElement('div');
    this._element.style.cssText = 'position:absolute;top:0;left:0;right:0;bottom:0;display:flex;flex-direction:column;background:var(--bg-primary);';
    this._currentFileIndex = 0;
    this._files = [];
    this._iframe = null;
    this._iframeReady = false;
    this._pendingUpdate = null;
  }

  get element() { return this._element; }

  init(params) {
    const data = params.params?.data;
    if (!data) {
      this._element.innerHTML = '<div style="padding:20px;color:var(--fg-secondary);">No coverage data available</div>';
      return;
    }

    // Parse JSON if it's a string
    const coverageData = typeof data === 'string' ? JSON.parse(data) : data;
    this._files = coverageData.files || [];

    if (this._files.length === 0) {
      this._element.innerHTML = '<div style="padding:20px;color:var(--fg-secondary);">No files with coverage data</div>';
      return;
    }

    this._render();
  }

  _render() {
    const file = this._files[this._currentFileIndex];
    const summary = file.summary || {};
    const exprPct = summary.exprTotal > 0 ? ((summary.exprCovered / summary.exprTotal) * 100).toFixed(1) : '0.0';
    const branchPct = summary.branchTotal > 0 ? ((summary.branchCovered / summary.branchTotal) * 100).toFixed(1) : '0.0';

    // File selector if multiple files
    let fileSelector = '';
    if (this._files.length > 1) {
      fileSelector = '<select id="coverage-file-select" style="margin-left:10px;padding:4px 8px;background:var(--bg-tertiary);color:var(--fg-primary);border:1px solid var(--border);border-radius:4px;">';
      this._files.forEach((f, i) => {
        const name = f.path.split('/').pop();
        const selected = i === this._currentFileIndex ? ' selected' : '';
        // Calculate coverage percentage for this file
        const sum = f.summary || {};
        const total = (sum.exprTotal || 0) + (sum.branchTotal || 0);
        const covered = (sum.exprCovered || 0) + (sum.branchCovered || 0);
        const pct = total > 0 ? Math.round((covered / total) * 100) : 0;
        fileSelector += '<option value="' + i + '"' + selected + '>' + this._escapeHtml(name) + ' (' + pct + '%)</option>';
      });
      fileSelector += '</select>';
    }

    this._element.innerHTML = `
      <div style="padding:10px 15px;background:var(--bg-secondary);border-bottom:1px solid var(--border);display:flex;align-items:center;gap:15px;flex-shrink:0;">
        <div id="coverage-filename" style="font-weight:600;color:var(--fg-primary);font-size:13px;">${this._escapeHtml(file.path.split('/').pop())}</div>
        ${fileSelector}
        <div style="flex:1;"></div>
        <div id="coverage-summary" style="display:flex;gap:15px;font-size:12px;">
          <span style="color:var(--fg-secondary);">Expressions: <span style="color:${exprPct === '100.0' ? '#3fb950' : exprPct === '0.0' ? '#f85149' : '#d29922'}">${summary.exprCovered}/${summary.exprTotal} (${exprPct}%)</span></span>
          <span style="color:var(--fg-secondary);">Branches: <span style="color:${branchPct === '100.0' ? '#3fb950' : branchPct === '0.0' ? '#f85149' : '#d29922'}">${summary.branchCovered}/${summary.branchTotal} (${branchPct}%)</span></span>
        </div>
      </div>
      <div style="padding:5px 10px;background:var(--bg-tertiary);border-bottom:1px solid var(--border);font-size:11px;color:var(--fg-secondary);flex-shrink:0;">
        <span style="display:inline-block;width:12px;height:12px;background:rgba(35,134,54,0.5);border-radius:2px;vertical-align:middle;margin-right:4px;"></span>Executed
        <span style="display:inline-block;width:12px;height:12px;background:rgba(218,54,51,0.5);border-radius:2px;vertical-align:middle;margin-left:15px;margin-right:4px;"></span>Not executed
        <span style="display:inline-block;width:12px;height:12px;background:rgba(180,40,40,0.6);border-radius:2px;vertical-align:middle;margin-left:15px;margin-right:4px;"></span>Neither branch
        <span style="display:inline-block;width:12px;height:12px;background:rgba(158,106,3,0.5);border-radius:2px;vertical-align:middle;margin-left:15px;margin-right:4px;"></span>Partial
      </div>
      <div id="monaco-container" style="flex:1;position:relative;"></div>
    `;

    // Add file selector event listener
    const select = this._element.querySelector('#coverage-file-select');
    if (select) {
      select.addEventListener('change', (e) => {
        this._currentFileIndex = parseInt(e.target.value, 10);
        this._updateEditor();
        this._updateSummary();
      });
    }

    // Create iframe for Monaco editor
    this._createMonacoIframe();
  }

  _createMonacoIframe() {
    const container = this._element.querySelector('#monaco-container');
    if (!container) return;

    this._iframe = document.createElement('iframe');
    this._iframe.style.cssText = 'width:100%;height:100%;border:none;';
    this._iframe.src = '/assets/monaco/editor.html';

    // Listen for messages from iframe
    this._messageHandler = (e) => {
      if (e.source !== this._iframe?.contentWindow) return;
      if (e.data && e.data.type === 'monaco-ready') {
        this._iframeReady = true;
        this._sendContentToEditor();
      } else if (e.data && e.data.type === 'copy-to-repl') {
        // Send the code to the REPL as a bracketed paste
        const code = e.data.code;
        if (code && ws && ws.readyState === WebSocket.OPEN) {
          // Send as paste (uses bracketed paste protocol) - preserves formatting
          ws.send(JSON.stringify({type: 'paste', data: code}));
        }
      }
    };
    window.addEventListener('message', this._messageHandler);

    container.appendChild(this._iframe);
    monacoIframes.add(this._iframe);
  }

  _sendContentToEditor() {
    if (!this._iframe || !this._iframeReady) return;

    const file = this._files[this._currentFileIndex];
    const isDark = document.body.classList.contains('dark');

    this._iframe.contentWindow.postMessage({
      type: 'set-content',
      content: file.content,
      theme: isDark ? 'dark' : 'light',
      annotations: file.annotations || []  // Pass raw annotations for precise highlighting
    }, '*');
  }

  _updateEditor() {
    this._sendContentToEditor();
  }

  _updateSummary() {
    const file = this._files[this._currentFileIndex];
    const summary = file.summary || {};
    const exprPct = summary.exprTotal > 0 ? ((summary.exprCovered / summary.exprTotal) * 100).toFixed(1) : '0.0';
    const branchPct = summary.branchTotal > 0 ? ((summary.branchCovered / summary.branchTotal) * 100).toFixed(1) : '0.0';

    const summaryEl = this._element.querySelector('#coverage-summary');
    if (summaryEl) {
      summaryEl.innerHTML = `
        <span style="color:var(--fg-secondary);">Expressions: <span style="color:${exprPct === '100.0' ? '#3fb950' : exprPct === '0.0' ? '#f85149' : '#d29922'}">${summary.exprCovered}/${summary.exprTotal} (${exprPct}%)</span></span>
        <span style="color:var(--fg-secondary);">Branches: <span style="color:${branchPct === '100.0' ? '#3fb950' : branchPct === '0.0' ? '#f85149' : '#d29922'}">${summary.branchCovered}/${summary.branchTotal} (${branchPct}%)</span></span>
      `;
    }

    // Update file name
    const nameEl = this._element.querySelector('#coverage-filename');
    if (nameEl) {
      nameEl.textContent = file.path.split('/').pop();
    }
  }

  _buildLineCoverage(file) {
    // Map: line number -> { executed: bool, notExecuted: bool, partialBranch: bool }
    const coverage = {};
    (file.annotations || []).forEach(ann => {
      for (let line = ann.startLine; line <= ann.endLine; line++) {
        if (!coverage[line]) {
          coverage[line] = { executed: false, notExecuted: false, partialBranch: false };
        }
        if (ann.state === 'executed' || ann.state === 'then-taken' || ann.state === 'else-taken') {
          coverage[line].executed = true;
        }
        if (ann.state === 'not-executed' || ann.state === 'then-not-taken' || ann.state === 'else-not-taken') {
          coverage[line].notExecuted = true;
        }
        // Partial branch: both taken and not-taken states on same line
        if (coverage[line].executed && coverage[line].notExecuted) {
          coverage[line].partialBranch = true;
        }
      }
    });
    return coverage;
  }

  _escapeHtml(text) {
    return text
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;');
  }

  dispose() {
    if (this._messageHandler) {
      window.removeEventListener('message', this._messageHandler);
    }
    if (this._iframe) {
      monacoIframes.delete(this._iframe);
    }
    this._iframe = null;
  }
}

// Monaco-based source viewer panel (for ,source command)
class MonacoSourcePanel {
  constructor() {
    this._element = document.createElement('div');
    this._element.style.cssText = 'position:absolute;top:0;left:0;right:0;bottom:0;display:flex;flex-direction:column;background:var(--bg-primary);';
    this._iframe = null;
    this._iframeReady = false;
    this._content = '';
    this._path = '';
    this._line = null;
  }

  get element() { return this._element; }

  init(params) {
    this._content = params.params?.content || '';
    this._path = params.params?.path || '';
    this._line = params.params?.line || null;

    if (!this._content) {
      this._element.innerHTML = '<div style="padding:20px;color:var(--fg-secondary);">No source available</div>';
      return;
    }

    this._render();
  }

  _render() {
    const filename = this._path.split('/').pop() || 'source';

    this._element.innerHTML = `
      <div style="padding:8px 15px;background:var(--bg-secondary);border-bottom:1px solid var(--border);display:flex;align-items:center;gap:10px;flex-shrink:0;">
        <div style="font-weight:600;color:var(--fg-primary);font-size:13px;">${this._escapeHtml(filename)}</div>
        <div style="flex:1;color:var(--fg-tertiary);font-size:11px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;">${this._escapeHtml(this._path)}</div>
      </div>
      <div id="monaco-container" style="flex:1;position:relative;"></div>
    `;

    this._createMonacoIframe();
  }

  _createMonacoIframe() {
    const container = this._element.querySelector('#monaco-container');
    if (!container) return;

    this._iframe = document.createElement('iframe');
    this._iframe.style.cssText = 'width:100%;height:100%;border:none;';
    this._iframe.src = '/assets/monaco/editor.html';

    this._messageHandler = (e) => {
      if (e.source !== this._iframe?.contentWindow) return;
      if (e.data && e.data.type === 'monaco-ready') {
        this._iframeReady = true;
        this._sendContentToEditor();
      } else if (e.data && e.data.type === 'copy-to-repl') {
        const code = e.data.code;
        if (code && ws && ws.readyState === WebSocket.OPEN) {
          ws.send(JSON.stringify({type: 'paste', data: code}));
        }
      }
    };
    window.addEventListener('message', this._messageHandler);

    container.appendChild(this._iframe);
    monacoIframes.add(this._iframe);
  }

  _sendContentToEditor() {
    if (!this._iframe || !this._iframeReady) return;

    const isDark = document.body.classList.contains('dark');

    this._iframe.contentWindow.postMessage({
      type: 'set-content',
      content: this._content,
      theme: isDark ? 'dark' : 'light',
      line: this._line
    }, '*');
  }

  _escapeHtml(text) {
    return text
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;');
  }

  dispose() {
    if (this._messageHandler) {
      window.removeEventListener('message', this._messageHandler);
    }
    if (this._iframe) {
      monacoIframes.delete(this._iframe);
    }
    this._iframe = null;
  }
}

class HashTablePanel {
  constructor() {
    this._element = document.createElement('div');
    this._element.style.cssText = 'position:absolute;top:0;left:0;right:0;bottom:0;background:var(--bg-primary);overflow:auto;padding:8px;';
    this._panelId = null;
    this._sourceExpr = null;
  }
  get element() { return this._element; }
  init(params) {
    this._panelId = params.params?.panelId;
    this._sourceExpr = params.params?.sourceExpr;
    const count = params.params?.count || 0;
    const entries = params.params?.entries || [];

    // Register with unified vizStates for refresh and type-change detection
    if (this._panelId) {
      registerVizPanel(this._panelId, this._sourceExpr, this._element, 'hash-table');
    }

    this._renderTable(count, entries);
  }

  updateData(count, entries) {
    this._renderTable(count, entries);
  }

  _renderTable(count, entries) {
    // Build HTML table
    let html = '<div style="font-family:monospace;font-size:13px;color:var(--fg-primary);">';
    html += '<div style="margin-bottom:8px;color:var(--fg-secondary);">Hash Table (' + count + ' entries)</div>';
    html += '<table style="border-collapse:collapse;width:100%;">';
    html += '<thead><tr style="background:var(--bg-tertiary);">';
    html += '<th style="text-align:left;padding:4px 8px;border:1px solid var(--border);">Key</th>';
    html += '<th style="text-align:left;padding:4px 8px;border:1px solid var(--border);">Value</th>';
    html += '</tr></thead><tbody>';

    (entries || []).forEach(([key, value]) => {
      const escKey = (key || '').replace(/</g, '&lt;').replace(/>/g, '&gt;');
      const escValue = (value || '').replace(/</g, '&lt;').replace(/>/g, '&gt;');
      html += '<tr style="background:var(--bg-secondary);">';
      html += '<td style="padding:4px 8px;border:1px solid var(--border);white-space:pre-wrap;">' + escKey + '</td>';
      html += '<td style="padding:4px 8px;border:1px solid var(--border);white-space:pre-wrap;">' + escValue + '</td>';
      html += '</tr>';
    });

    if (count > (entries || []).length) {
      html += '<tr style="background:var(--bg-secondary);color:var(--fg-secondary);">';
      html += '<td colspan="2" style="padding:4px 8px;border:1px solid var(--border);text-align:center;font-style:italic;">';
      html += '... and ' + (count - entries.length) + ' more entries</td></tr>';
    }

    html += '</tbody></table></div>';
    this._element.innerHTML = html;
  }
}

// SVG Panel - renders SVG content directly
class SvgPanel {
  constructor() {
    this._element = document.createElement('div');
    this._element.style.cssText = 'position:absolute;top:0;left:0;right:0;bottom:0;background:var(--bg-primary);overflow:auto;display:flex;align-items:center;justify-content:center;';
    this._panelId = null;
    this._sourceExpr = null;
  }
  get element() { return this._element; }
  init(params) {
    // Use panelId from params (set by openSvgPanel) or fall back to dockview id
    this._panelId = params.params?.panelId || params.id;
    this._sourceExpr = params.params?.sourceExpr;
    const content = params.params?.content || '';
    console.log('SvgPanel.init panelId:', this._panelId, 'sourceExpr:', this._sourceExpr);
    this._element.innerHTML = content;
    // Scale SVG to fit if needed
    const svg = this._element.querySelector('svg');
    if (svg) {
      svg.style.maxWidth = '100%';
      svg.style.maxHeight = '100%';
      svg.style.width = 'auto';
      svg.style.height = 'auto';
    }
    // Register with unified vizStates
    if (this._panelId) {
      registerVizPanel(this._panelId, this._sourceExpr, this._element, 'svg');
      console.log('SvgPanel registered:', this._panelId);
    }
  }
}

// HTML Panel - renders HTML content in a sandboxed iframe
class HtmlPanel {
  constructor() {
    this._element = document.createElement('div');
    this._element.style.cssText = 'position:absolute;top:0;left:0;right:0;bottom:0;background:var(--bg-primary);';
    this._panelId = null;
    this._sourceExpr = null;
    this._iframe = null;
  }
  get element() { return this._element; }
  init(params) {
    // Use panelId from params (set by openHtmlPanel) or fall back to dockview id
    this._panelId = params.params?.panelId || params.id;
    this._sourceExpr = params.params?.sourceExpr;
    const content = params.params?.content || '';
    console.log('HtmlPanel.init panelId:', this._panelId, 'sourceExpr:', this._sourceExpr);
    // Use srcdoc for sandboxed HTML rendering
    this._iframe = document.createElement('iframe');
    this._iframe.style.cssText = 'width:100%;height:100%;border:none;background:white;';
    this._iframe.sandbox = 'allow-same-origin';  // Minimal permissions
    this._iframe.srcdoc = content;
    this._element.appendChild(this._iframe);
    // Register with unified vizStates
    if (this._panelId) {
      registerVizPanel(this._panelId, this._sourceExpr, this._element, 'html');
      console.log('HtmlPanel registered:', this._panelId);
    }
  }
}

// JSON Panel - renders JSON/code with syntax highlighting
class JsonPanel {
  constructor() {
    this._element = document.createElement('div');
    this._element.style.cssText = 'position:absolute;top:0;left:0;right:0;bottom:0;background:var(--bg-primary);overflow:auto;';
    this._panelId = null;
    this._sourceExpr = null;
  }
  get element() { return this._element; }
  init(params) {
    this._panelId = params.params?.panelId || params.id;
    this._sourceExpr = params.params?.sourceExpr;
    const content = params.params?.content || '';
    console.log('JsonPanel.init panelId:', this._panelId, 'sourceExpr:', this._sourceExpr);
    // Render with highlight.js
    renderJson(this._element, content);
    // Register with unified vizStates
    if (this._panelId) {
      registerVizPanel(this._panelId, this._sourceExpr, this._element, 'json');
      console.log('JsonPanel registered:', this._panelId);
    }
  }
}

// Image Panel - displays images
class ImagePanel {
  constructor() {
    this._element = document.createElement('div');
    this._element.style.cssText = 'position:absolute;top:0;left:0;right:0;bottom:0;background:var(--bg-primary);display:flex;align-items:center;justify-content:center;';
    this._panelId = null;
    this._sourceExpr = null;
  }
  get element() { return this._element; }
  init(params) {
    this._panelId = params.params?.panelId || params.id;
    this._sourceExpr = params.params?.sourceExpr;
    const imageUrl = params.params?.imageUrl || '';
    console.log('ImagePanel.init panelId:', this._panelId, 'sourceExpr:', this._sourceExpr);
    // Render image
    renderImage(this._element, imageUrl);
    // Register with unified vizStates
    if (this._panelId) {
      registerVizPanel(this._panelId, this._sourceExpr, this._element, 'image');
      console.log('ImagePanel registered:', this._panelId);
    }
  }
}

// Vega-Lite Panel - renders Vega-Lite specifications
class VegaLitePanel {
  constructor() {
    this._element = document.createElement('div');
    this._element.style.cssText = 'position:absolute;top:0;left:0;right:0;bottom:0;background:var(--bg-primary);overflow:hidden;';
    this._panelId = null;
    this._sourceExpr = null;
    this._spec = null;
  }
  get element() { return this._element; }
  init(params) {
    this._panelId = params.params?.panelId || params.id;
    this._sourceExpr = params.params?.sourceExpr;
    this._spec = params.params?.spec || '';
    const spec = this._spec;
    console.log('VegaLitePanel.init panelId:', this._panelId, 'sourceExpr:', this._sourceExpr);
    // Render with Vega-Embed
    renderVegaLite(this._element, spec);
    // Register with vegaLiteStates for updates
    if (this._panelId) {
      vegaLiteStates.set(this._panelId, this);
      console.log('VegaLitePanel registered:', this._panelId);
    }
    // Re-render on resize (debounced)
    let resizeTimeout = null;
    const resizeObserver = new ResizeObserver(() => {
      if (resizeTimeout) clearTimeout(resizeTimeout);
      resizeTimeout = setTimeout(() => this.reRender(), 150);
    });
    resizeObserver.observe(this._element);
  }
  reRender() {
    if (this._spec) {
      renderVegaLite(this._element, this._spec);
    }
  }
}

class MermaidPanel {
  constructor() {
    this._element = document.createElement('div');
    this._element.style.cssText = 'position:absolute;top:0;left:0;right:0;bottom:0;background:var(--bg-primary);overflow:auto;';
    this._panelId = null;
    this._sourceExpr = null;
    this._definition = '';
  }
  get element() { return this._element; }
  init(params) {
    this._panelId = params.params?.panelId || params.id;
    this._sourceExpr = params.params?.sourceExpr;
    this._definition = params.params?.definition || '';
    console.log('MermaidPanel.init panelId:', this._panelId, 'sourceExpr:', this._sourceExpr);
    // Render diagram
    renderMermaid(this._element, this._definition);
    // Register with mermaidStates for updates
    if (this._panelId) {
      mermaidStates.set(this._panelId, this);
      console.log('MermaidPanel registered:', this._panelId);
    }
    // Re-render on resize (debounced)
    let resizeTimeout = null;
    const resizeObserver = new ResizeObserver(() => {
      if (resizeTimeout) clearTimeout(resizeTimeout);
      resizeTimeout = setTimeout(() => this.reRender(), 150);
    });
    resizeObserver.observe(this._element);
  }
  reRender() {
    if (this._definition) {
      renderMermaid(this._element, this._definition);
    }
  }
}

class RegexpPanel {
  constructor() {
    this._element = document.createElement('div');
    this._element.style.cssText = 'position:absolute;top:0;left:0;right:0;bottom:0;background:white;overflow:auto;';
    this._panelId = null;
    this._sourceExpr = null;
    this._pattern = '';
  }
  get element() { return this._element; }
  init(params) {
    this._panelId = params.params?.panelId || params.id;
    this._sourceExpr = params.params?.sourceExpr;
    this._pattern = params.params?.pattern || '';
    console.log('RegexpPanel.init panelId:', this._panelId, 'sourceExpr:', this._sourceExpr);
    // Render diagram
    renderRegexp(this._element, this._pattern);
    // Register with regexpStates for updates
    if (this._panelId) {
      regexpStates.set(this._panelId, this);
      console.log('RegexpPanel registered:', this._panelId);
    }
    // Re-render on resize (debounced)
    let resizeTimeout = null;
    const resizeObserver = new ResizeObserver(() => {
      if (resizeTimeout) clearTimeout(resizeTimeout);
      resizeTimeout = setTimeout(() => this.reRender(), 150);
    });
    resizeObserver.observe(this._element);
  }
  reRender() {
    if (this._pattern) {
      renderRegexp(this._element, this._pattern);
    }
  }
}

class VennPanel {
  constructor() {
    this._element = document.createElement('div');
    this._element.style.cssText = 'position:absolute;top:0;left:0;right:0;bottom:0;background:var(--bg-primary);overflow:auto;';
    this._panelId = null;
    this._sourceExpr = null;
    this._setNames = [];
    this._setMembers = [];
  }
  get element() { return this._element; }

  init(params) {
    this._panelId = params.params?.panelId;
    this._sourceExpr = params.params?.sourceExpr;
    this._setNames = params.params?.setNames || [];
    this._setMembers = params.params?.setMembers || [];

    // Register for refresh updates
    if (this._panelId) {
      vennStates.set(this._panelId, this);
    }

    this._render();

    // Re-render on resize (debounced)
    let resizeTimeout = null;
    const resizeObserver = new ResizeObserver(() => {
      if (resizeTimeout) clearTimeout(resizeTimeout);
      resizeTimeout = setTimeout(() => this._render(), 100);
    });
    resizeObserver.observe(this._element);
  }

  updateData(setMembers) {
    this._setMembers = setMembers || [];
    this._render();
  }

  _render() {
    const width = this._element.clientWidth || 400;
    const height = this._element.clientHeight || 300;
    const cx = width / 2;
    const cy = height / 2;

    if (this._setMembers.length === 1) {
      this._renderSingleSet(width, height, cx, cy);
    } else if (this._setMembers.length === 2) {
      this._renderTwoSets(width, height, cx, cy);
    } else if (this._setMembers.length === 3) {
      this._renderThreeSets(width, height, cx, cy);
    } else {
      this._element.innerHTML = '<div style="padding:20px;color:var(--fg-secondary);">Venn diagrams support 1-3 sets</div>';
    }
  }

  _renderSingleSet(width, height, cx, cy) {
    const r = Math.min(width, height) * 0.35;
    const members = this._setMembers[0] || [];
    const name = this._setNames[0] || 'Set';

    let svg = '<svg width="' + width + '" height="' + height + '" style="display:block;">';
    svg += '<circle cx="' + cx + '" cy="' + cy + '" r="' + r + '" fill="var(--accent)" fill-opacity="0.3" stroke="var(--accent)" stroke-width="2"/>';
    svg += '<text x="' + cx + '" y="' + (cy - r - 10) + '" text-anchor="middle" fill="var(--fg-primary)" font-size="14" font-weight="bold">' + this._escapeHtml(name) + '</text>';
    svg += '<text x="' + cx + '" y="' + (cy - r + 20) + '" text-anchor="middle" fill="var(--fg-secondary)" font-size="12">(' + members.length + ' members)</text>';

    // List members inside circle
    const maxShow = 15;
    const lineHeight = 16;
    const startY = cy - Math.min(members.length, maxShow) * lineHeight / 2;
    members.slice(0, maxShow).forEach((m, i) => {
      svg += '<text x="' + cx + '" y="' + (startY + i * lineHeight) + '" text-anchor="middle" fill="var(--fg-primary)" font-size="11" font-family="monospace">' + this._escapeHtml(m) + '</text>';
    });
    if (members.length > maxShow) {
      svg += '<text x="' + cx + '" y="' + (startY + maxShow * lineHeight) + '" text-anchor="middle" fill="var(--fg-secondary)" font-size="11" font-style="italic">... and ' + (members.length - maxShow) + ' more</text>';
    }

    svg += '</svg>';
    this._element.innerHTML = svg;
  }

  _renderTwoSets(width, height, cx, cy) {
    const r = Math.min(width, height) * 0.3;
    const offset = r * 0.6;  // Circle centers offset from center
    const setA = new Set(this._setMembers[0] || []);
    const setB = new Set(this._setMembers[1] || []);
    const nameA = this._setNames[0] || 'Set A';
    const nameB = this._setNames[1] || 'Set B';

    // Compute regions
    const onlyA = [...setA].filter(x => !setB.has(x));
    const onlyB = [...setB].filter(x => !setA.has(x));
    const intersection = [...setA].filter(x => setB.has(x));

    const cxA = cx - offset;
    const cxB = cx + offset;

    let svg = '<svg width="' + width + '" height="' + height + '" style="display:block;">';

    // Circle A
    svg += '<circle cx="' + cxA + '" cy="' + cy + '" r="' + r + '" fill="#89b4fa" fill-opacity="0.35" stroke="#89b4fa" stroke-width="2"/>';
    // Circle B
    svg += '<circle cx="' + cxB + '" cy="' + cy + '" r="' + r + '" fill="#f38ba8" fill-opacity="0.35" stroke="#f38ba8" stroke-width="2"/>';

    // Labels
    svg += '<text x="' + cxA + '" y="' + (cy - r - 10) + '" text-anchor="middle" fill="var(--fg-primary)" font-size="13" font-weight="bold">' + this._escapeHtml(nameA) + '</text>';
    svg += '<text x="' + cxB + '" y="' + (cy - r - 10) + '" text-anchor="middle" fill="var(--fg-primary)" font-size="13" font-weight="bold">' + this._escapeHtml(nameB) + '</text>';

    // Member counts
    svg += '<text x="' + (cxA - r * 0.5) + '" y="' + (cy - r * 0.6) + '" text-anchor="middle" fill="var(--fg-secondary)" font-size="11">' + onlyA.length + ' only</text>';
    svg += '<text x="' + cx + '" y="' + (cy - r * 0.6) + '" text-anchor="middle" fill="var(--fg-secondary)" font-size="11">' + intersection.length + ' shared</text>';
    svg += '<text x="' + (cxB + r * 0.5) + '" y="' + (cy - r * 0.6) + '" text-anchor="middle" fill="var(--fg-secondary)" font-size="11">' + onlyB.length + ' only</text>';

    // Render members in each region
    const maxPerRegion = 8;
    const lineHeight = 14;

    // Only A (left region)
    const leftX = cxA - r * 0.5;
    const startYA = cy - Math.min(onlyA.length, maxPerRegion) * lineHeight / 2;
    onlyA.slice(0, maxPerRegion).forEach((m, i) => {
      svg += '<text x="' + leftX + '" y="' + (startYA + i * lineHeight) + '" text-anchor="middle" fill="var(--fg-primary)" font-size="10" font-family="monospace">' + this._escapeHtml(this._truncate(m, 12)) + '</text>';
    });
    if (onlyA.length > maxPerRegion) {
      svg += '<text x="' + leftX + '" y="' + (startYA + maxPerRegion * lineHeight) + '" text-anchor="middle" fill="var(--fg-secondary)" font-size="9">+' + (onlyA.length - maxPerRegion) + '</text>';
    }

    // Intersection (center)
    const startYI = cy - Math.min(intersection.length, maxPerRegion) * lineHeight / 2;
    intersection.slice(0, maxPerRegion).forEach((m, i) => {
      svg += '<text x="' + cx + '" y="' + (startYI + i * lineHeight) + '" text-anchor="middle" fill="var(--fg-primary)" font-size="10" font-family="monospace" font-weight="bold">' + this._escapeHtml(this._truncate(m, 12)) + '</text>';
    });
    if (intersection.length > maxPerRegion) {
      svg += '<text x="' + cx + '" y="' + (startYI + maxPerRegion * lineHeight) + '" text-anchor="middle" fill="var(--fg-secondary)" font-size="9">+' + (intersection.length - maxPerRegion) + '</text>';
    }

    // Only B (right region)
    const rightX = cxB + r * 0.5;
    const startYB = cy - Math.min(onlyB.length, maxPerRegion) * lineHeight / 2;
    onlyB.slice(0, maxPerRegion).forEach((m, i) => {
      svg += '<text x="' + rightX + '" y="' + (startYB + i * lineHeight) + '" text-anchor="middle" fill="var(--fg-primary)" font-size="10" font-family="monospace">' + this._escapeHtml(this._truncate(m, 12)) + '</text>';
    });
    if (onlyB.length > maxPerRegion) {
      svg += '<text x="' + rightX + '" y="' + (startYB + maxPerRegion * lineHeight) + '" text-anchor="middle" fill="var(--fg-secondary)" font-size="9">+' + (onlyB.length - maxPerRegion) + '</text>';
    }

    svg += '</svg>';
    this._element.innerHTML = svg;
  }

  _renderThreeSets(width, height, cx, cy) {
    const r = Math.min(width, height) * 0.25;
    const offset = r * 0.7;
    const setA = new Set(this._setMembers[0] || []);
    const setB = new Set(this._setMembers[1] || []);
    const setC = new Set(this._setMembers[2] || []);
    const nameA = this._setNames[0] || 'A';
    const nameB = this._setNames[1] || 'B';
    const nameC = this._setNames[2] || 'C';

    // Circle positions (triangle arrangement)
    const cxA = cx;
    const cyA = cy - offset * 0.8;
    const cxB = cx - offset;
    const cyB = cy + offset * 0.5;
    const cxC = cx + offset;
    const cyC = cy + offset * 0.5;

    // Compute regions
    const onlyA = [...setA].filter(x => !setB.has(x) && !setC.has(x)).length;
    const onlyB = [...setB].filter(x => !setA.has(x) && !setC.has(x)).length;
    const onlyC = [...setC].filter(x => !setA.has(x) && !setB.has(x)).length;
    const abOnly = [...setA].filter(x => setB.has(x) && !setC.has(x)).length;
    const acOnly = [...setA].filter(x => setC.has(x) && !setB.has(x)).length;
    const bcOnly = [...setB].filter(x => setC.has(x) && !setA.has(x)).length;
    const abc = [...setA].filter(x => setB.has(x) && setC.has(x)).length;

    let svg = '<svg width="' + width + '" height="' + height + '" style="display:block;">';

    // Circles
    svg += '<circle cx="' + cxA + '" cy="' + cyA + '" r="' + r + '" fill="#89b4fa" fill-opacity="0.3" stroke="#89b4fa" stroke-width="2"/>';
    svg += '<circle cx="' + cxB + '" cy="' + cyB + '" r="' + r + '" fill="#f38ba8" fill-opacity="0.3" stroke="#f38ba8" stroke-width="2"/>';
    svg += '<circle cx="' + cxC + '" cy="' + cyC + '" r="' + r + '" fill="#a6e3a1" fill-opacity="0.3" stroke="#a6e3a1" stroke-width="2"/>';

    // Labels
    svg += '<text x="' + cxA + '" y="' + (cyA - r - 8) + '" text-anchor="middle" fill="var(--fg-primary)" font-size="12" font-weight="bold">' + this._escapeHtml(nameA) + '</text>';
    svg += '<text x="' + (cxB - r * 0.7) + '" y="' + (cyB + r * 0.3) + '" text-anchor="middle" fill="var(--fg-primary)" font-size="12" font-weight="bold">' + this._escapeHtml(nameB) + '</text>';
    svg += '<text x="' + (cxC + r * 0.7) + '" y="' + (cyC + r * 0.3) + '" text-anchor="middle" fill="var(--fg-primary)" font-size="12" font-weight="bold">' + this._escapeHtml(nameC) + '</text>';

    // Counts in regions (approximate positions)
    svg += '<text x="' + cxA + '" y="' + (cyA - r * 0.3) + '" text-anchor="middle" fill="var(--fg-primary)" font-size="11">' + onlyA + '</text>';
    svg += '<text x="' + (cxB - r * 0.4) + '" y="' + (cyB + r * 0.2) + '" text-anchor="middle" fill="var(--fg-primary)" font-size="11">' + onlyB + '</text>';
    svg += '<text x="' + (cxC + r * 0.4) + '" y="' + (cyC + r * 0.2) + '" text-anchor="middle" fill="var(--fg-primary)" font-size="11">' + onlyC + '</text>';
    svg += '<text x="' + (cx - offset * 0.35) + '" y="' + (cy - offset * 0.15) + '" text-anchor="middle" fill="var(--fg-primary)" font-size="10">' + abOnly + '</text>';
    svg += '<text x="' + (cx + offset * 0.35) + '" y="' + (cy - offset * 0.15) + '" text-anchor="middle" fill="var(--fg-primary)" font-size="10">' + acOnly + '</text>';
    svg += '<text x="' + cx + '" y="' + (cy + offset * 0.5) + '" text-anchor="middle" fill="var(--fg-primary)" font-size="10">' + bcOnly + '</text>';
    svg += '<text x="' + cx + '" y="' + cy + '" text-anchor="middle" fill="var(--fg-primary)" font-size="12" font-weight="bold">' + abc + '</text>';

    svg += '</svg>';
    this._element.innerHTML = svg;
  }

  _escapeHtml(str) {
    return (str || '').replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  }

  _truncate(str, len) {
    return str.length > len ? str.slice(0, len - 1) + '…' : str;
  }
}

class GraphvizPanel {
  constructor() {
    console.log('GraphvizPanel constructor');
    this._element = document.createElement('div');
    this._element.style.cssText = 'position:absolute;top:0;left:0;right:0;bottom:0;background:var(--bg-primary);overflow:auto;user-select:none;-webkit-user-select:none;';
    this._element.tabIndex = 0;  // Make focusable for keyboard events
    this._panelId = null;
    this._className = null;
    this._packageName = null;
    this._nodes = new Map();  // name -> {pkg, slots, expanded}
    this._edges = new Set();  // 'from->to'
    this._rootClass = null;
    this._focusNode = null;  // Node to center on after render
    this._viz = null;
    this._zoom = 1.0;
    this._baseWidth = 0;
    this._baseHeight = 0;
    this._svg = null;

    // Create tooltip element for showing slots on hover
    this._tooltip = document.createElement('div');
    this._tooltip.style.cssText = 'position:fixed;display:none;padding:8px 12px;background:var(--bg-secondary);color:var(--fg-primary);border:1px solid var(--border);border-radius:4px;font-size:12px;font-family:monospace;max-width:300px;z-index:1000;pointer-events:none;opacity:0.95;box-shadow:0 2px 8px rgba(0,0,0,0.3);';
    document.body.appendChild(this._tooltip);

    // Create child selector popup
    this._childSelector = document.createElement('div');
    this._childSelector.style.cssText = 'position:fixed;display:none;padding:4px 0;background:var(--bg-secondary);color:var(--fg-primary);border:1px solid var(--border);border-radius:4px;font-size:12px;font-family:monospace;max-height:300px;overflow-y:auto;z-index:1001;box-shadow:0 4px 12px rgba(0,0,0,0.4);min-width:150px;';
    document.body.appendChild(this._childSelector);
    this._pendingParent = null;  // Parent node waiting for child selection

    // Keyboard zoom handlers
    this._element.addEventListener('keydown', (e) => {
      if (e.key === '+' || e.key === '=') {
        e.preventDefault();
        this._setZoom(this._zoom * 1.2);
      } else if (e.key === '-' || e.key === '_') {
        e.preventDefault();
        this._setZoom(this._zoom / 1.2);
      } else if (e.key === '0') {
        e.preventDefault();
        this._setZoom(1.0);  // Reset zoom
      }
    });

    // Mouse wheel zoom (with Ctrl)
    this._element.addEventListener('wheel', (e) => {
      if (e.ctrlKey) {
        e.preventDefault();
        const delta = e.deltaY > 0 ? 0.9 : 1.1;
        this._setZoom(this._zoom * delta);
      }
    }, { passive: false });

    // Click-and-drag panning
    this._isPanning = false;
    this._panStartX = 0;
    this._panStartY = 0;
    this._scrollStartX = 0;
    this._scrollStartY = 0;
    this._didPan = false;  // Track if we actually panned (vs clicked)

    this._element.addEventListener('mousedown', (e) => {
      if (e.button !== 0) return;
      this._isPanning = true;
      this._didPan = false;
      this._panStartX = e.clientX;
      this._panStartY = e.clientY;
      this._scrollStartX = this._element.scrollLeft;
      this._scrollStartY = this._element.scrollTop;
    });

    this._element.addEventListener('mousemove', (e) => {
      if (!this._isPanning) return;
      const dx = e.clientX - this._panStartX;
      const dy = e.clientY - this._panStartY;
      // Only start panning after moving 5+ pixels (prevents accidental drags)
      if (Math.abs(dx) > 5 || Math.abs(dy) > 5) {
        this._didPan = true;
        this._element.style.cursor = 'grabbing';
      }
      if (this._didPan) {
        this._element.scrollLeft = this._scrollStartX - dx;
        this._element.scrollTop = this._scrollStartY - dy;
      }
    });

    this._element.addEventListener('mouseup', () => {
      this._isPanning = false;
      this._element.style.cursor = 'grab';
    });

    this._element.addEventListener('mouseleave', () => {
      this._isPanning = false;
      this._element.style.cursor = 'grab';
    });

    // Set initial cursor
    this._element.style.cursor = 'grab';
  }
  get element() { return this._element; }

  _setZoom(newZoom) {
    // Clamp zoom between 0.1 and 5.0
    this._zoom = Math.max(0.1, Math.min(5.0, newZoom));
    this._applyZoom();
  }

  _applyZoom() {
    if (!this._svg || !this._baseWidth) return;
    const w = Math.round(this._baseWidth * this._zoom);
    const h = Math.round(this._baseHeight * this._zoom);
    this._svg.style.width = w + 'px';
    this._svg.style.height = h + 'px';
  }

  async init(params) {
    console.log('GraphvizPanel init:', params);
    this._panelId = params.params?.panelId;
    this._className = params.params?.className;
    this._packageName = params.params?.packageName;

    // Register for data updates
    graphvizStates.set(this._panelId, this);

    // Initialize viz.js
    this._viz = await Viz.instance();

    // Request data if we have class info
    if (this._className && this._packageName) {
      ws.send(JSON.stringify({
        type: 'get-class-graph',
        className: this._className,
        packageName: this._packageName,
        panelId: this._panelId
      }));
    }

    // Handle pending data
    const pending = pendingClassGraphs.get(this._panelId);
    if (pending && pending.length) {
      pendingClassGraphs.delete(this._panelId);
      pending.forEach((msg) => {
        if (msg.type === 'class-graph-expand') this.addGraph(msg);
        else this.updateGraph(msg);
      });
    }

    // Re-render on resize (debounced)
    let resizeTimeout = null;
    const resizeObserver = new ResizeObserver(() => {
      if (resizeTimeout) clearTimeout(resizeTimeout);
      resizeTimeout = setTimeout(() => {
        if (this._nodes.size > 0) {
          this._render();
        }
      }, 150);
    });
    resizeObserver.observe(this._element);
  }

  _escapeLabel(str) {
    // Escape for DOT format
    return str.replace(/\\/g, '\\\\').replace(/"/g, '\\"').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  }

  _getThemeColors() {
    // Read current theme colors from CSS variables
    const style = getComputedStyle(document.body);
    return {
      bg: style.getPropertyValue('--bg-primary').trim() || '#1e1e2e',
      nodeFill: style.getPropertyValue('--bg-tertiary').trim() || '#45475a',
      rootFill: style.getPropertyValue('--accent').trim() || '#89b4fa',
      text: style.getPropertyValue('--fg-primary').trim() || '#cdd6f4',
      rootText: '#ffffff',  // White text on accent background for readability
      edge: style.getPropertyValue('--fg-muted').trim() || '#6c7086',
      border: style.getPropertyValue('--border').trim() || '#45475a'
    };
  }

  _getAncestors(nodeName) {
    // Find all ancestors of a node by traversing edges upward
    // Edges are stored as 'superclass->subclass'
    const ancestors = new Set();
    const toVisit = [nodeName];
    while (toVisit.length > 0) {
      const current = toVisit.pop();
      for (const edge of this._edges) {
        const [parent, child] = edge.split('->');
        if (child === current && !ancestors.has(parent)) {
          ancestors.add(parent);
          toVisit.push(parent);
        }
      }
    }
    return ancestors;
  }

  _highlightAncestors(nodeName, highlight) {
    // Add or remove highlight from ancestor nodes AND the hovered node itself
    const ancestors = this._getAncestors(nodeName);
    ancestors.add(nodeName);  // Include the hovered node
    const svg = this._svg;
    if (!svg) return;

    svg.querySelectorAll('g.node').forEach(g => {
      const name = g.dataset.nodeName;
      if (ancestors.has(name)) {
        const polygon = g.querySelector('polygon') || g.querySelector('path');
        if (polygon) {
          if (highlight) {
            polygon.setAttribute('stroke', 'var(--accent)');
            polygon.setAttribute('stroke-width', '2.5');
          } else {
            // Restore original
            const colors = this._getThemeColors();
            polygon.setAttribute('stroke', colors.border);
            polygon.setAttribute('stroke-width', '1');
          }
        }
      }
    });
  }

  _generateDot(direction = 'TB') {
    const colors = this._getThemeColors();

    let dot = 'digraph G {\n';
    dot += '  rankdir=' + direction + ';\n';  // TB=top-to-bottom, LR=left-to-right
    dot += '  newrank=true;\n';  // Use newer ranking algorithm for consistent hierarchy
    dot += '  splines=ortho;\n';  // Orthogonal edges for cleaner look
    dot += '  ranksep=0.5;\n';  // Vertical spacing between ranks
    dot += '  nodesep=0.3;\n';  // Horizontal spacing between nodes
    dot += '  bgcolor="' + colors.bg + '";\n';
    dot += '  node [shape=box, style="filled,rounded", fontname="Helvetica", fontsize=10, fontcolor="' + colors.text + '", color="' + colors.border + '", margin="0.15,0.08"];\n';
    dot += '  edge [arrowhead=empty, color="' + colors.edge + '"];\n';
    dot += '\n';

    // Add nodes (simple labels - slots shown via hover tooltip)
    for (const [name, info] of this._nodes) {
      const escName = this._escapeLabel(name);
      const isRoot = name === this._rootClass;
      const fillColor = isRoot ? colors.rootFill : colors.nodeFill;
      const textColor = isRoot ? colors.rootText : colors.text;
      dot += '  "' + name + '" [label="' + escName + '", fillcolor="' + fillColor + '", fontcolor="' + textColor + '"];\n';
    }

    dot += '\n';

    // Add edges - stored as 'superclass->subclass', output same way
    for (const edge of this._edges) {
      const [from, to] = edge.split('->');
      dot += '  "' + from + '" -> "' + to + '";\n';
    }

    dot += '}\n';
    return dot;
  }

  _render() {
    if (!this._viz || this._nodes.size === 0) return;

    // Get container dimensions (with fallbacks)
    const containerWidth = this._element.clientWidth || 400;
    const containerHeight = this._element.clientHeight || 300;

    // Helper to parse SVG dimensions
    const parseSvgDimensions = (svg) => {
      let w = 300, h = 200;
      const widthAttr = svg.getAttribute('width');
      const heightAttr = svg.getAttribute('height');
      if (widthAttr) {
        w = parseFloat(widthAttr);
        if (widthAttr.endsWith('pt')) w *= 1.33;
      }
      if (heightAttr) {
        h = parseFloat(heightAttr);
        if (heightAttr.endsWith('pt')) h *= 1.33;
      }
      return { width: w, height: h };
    };

    try {
      // Try TB (top-to-bottom) first
      let dot = this._generateDot('TB');
      let svg = this._viz.renderSVGElement(dot);
      let dims = parseSvgDimensions(svg);

      // If graph is too wide (more than 2x container width), switch to LR (left-to-right)
      if (dims.width > containerWidth * 2) {
        console.log('Graph too wide (' + dims.width + 'px), switching to LR layout');
        dot = this._generateDot('LR');
        svg = this._viz.renderSVGElement(dot);
        dims = parseSvgDimensions(svg);
      }

      const svgWidth = dims.width;
      const svgHeight = dims.height;

      // Don't shrink huge graphs to unreadable sizes - just show part and let user pan
      // Use scale 1.0 (natural size) for most graphs, only shrink if reasonably small
      const fitScale = Math.min(containerWidth / svgWidth, containerHeight / svgHeight);
      const initialScale = fitScale > 0.3 ? fitScale : 1.0;  // If would shrink below 30%, just use natural size

      // Store base dimensions for zoom calculations
      this._baseWidth = Math.round(svgWidth * initialScale);
      this._baseHeight = Math.round(svgHeight * initialScale);
      this._zoom = 1.0;  // Reset zoom on new render
      this._svg = svg;

      // Apply dimensions
      svg.removeAttribute('width');
      svg.removeAttribute('height');
      svg.style.width = this._baseWidth + 'px';
      svg.style.height = this._baseHeight + 'px';
      svg.style.display = 'block';

      // Center small graphs that fit within the container
      if (this._baseWidth < containerWidth) {
        svg.style.marginLeft = Math.round((containerWidth - this._baseWidth) / 2) + 'px';
      }
      if (this._baseHeight < containerHeight) {
        svg.style.marginTop = Math.round((containerHeight - this._baseHeight) / 2) + 'px';
      }

      // Process nodes: extract names, then remove ALL title elements to disable native tooltips
      svg.querySelectorAll('g.node').forEach(g => {
        const title = g.querySelector('title');
        if (title) {
          g.dataset.nodeName = title.textContent;
        }
      });
      svg.querySelectorAll('title').forEach(t => t.remove());

      // Clear and add SVG
      this._element.innerHTML = '';
      this._element.appendChild(svg);
      this._element.focus();  // Focus for keyboard events

      // Add click and hover handlers to nodes
      svg.querySelectorAll('g.node').forEach(g => {
        g.style.cursor = 'pointer';
        const name = g.dataset.nodeName;
        if (name) {
          const info = this._nodes.get(name);

          // Click handler
          g.addEventListener('click', (e) => {
            // Ignore click if we just finished panning
            if (this._didPan) {
              this._didPan = false;
              return;
            }
            this._onNodeClick(name, e.clientX, e.clientY);
          });

          // Hover handlers for slot tooltip and ancestor highlighting
          g.addEventListener('mouseenter', (e) => {
            if (!info) {
              console.log('No info for node:', name, 'available:', Array.from(this._nodes.keys()));
              return;
            }
            // Highlight ancestor path
            this._highlightAncestors(name, true);

            // Show tooltip
            let content = '<strong>' + name + '</strong>';
            if (info.pkg) content += '<br><span style="color:var(--fg-muted);">' + info.pkg + '</span>';
            if (info.slots && info.slots.length > 0) {
              const slotList = info.slots.slice(0, 15).join('\n');
              const more = info.slots.length > 15 ? '\n...' + (info.slots.length - 15) + ' more' : '';
              content += '<br><br><em>Direct slots:</em><pre style="margin:4px 0 0 0;white-space:pre-wrap;">' + slotList + more + '</pre>';
            } else {
              content += '<br><span style="color:var(--fg-muted);font-style:italic;">No direct slots</span>';
            }
            this._tooltip.innerHTML = content;
            this._tooltip.style.display = 'block';
            this._tooltip.style.left = (e.clientX + 10) + 'px';
            this._tooltip.style.top = (e.clientY + 10) + 'px';
          });

          g.addEventListener('mousemove', (e) => {
            if (this._tooltip.style.display === 'block') {
              this._tooltip.style.left = (e.clientX + 10) + 'px';
              this._tooltip.style.top = (e.clientY + 10) + 'px';
            }
          });

          g.addEventListener('mouseleave', () => {
            this._tooltip.style.display = 'none';
            // Remove ancestor highlighting
            this._highlightAncestors(name, false);
          });
        }
      });

      // Scroll to center on focus node (clicked node, or root on initial load)
      const targetNode = this._focusNode || this._rootClass;
      if (targetNode) {
        const nodes = svg.querySelectorAll('g.node');
        for (const g of nodes) {
          if (g.dataset.nodeName === targetNode) {
            const bbox = g.getBoundingClientRect();
            const containerRect = this._element.getBoundingClientRect();
            // Scroll to center the node horizontally and vertically
            const scrollX = (bbox.left - containerRect.left) - (containerWidth / 2) + (bbox.width / 2);
            const scrollY = (bbox.top - containerRect.top) - (containerHeight / 2) + (bbox.height / 2);
            this._element.scrollLeft = Math.max(0, scrollX);
            this._element.scrollTop = Math.max(0, scrollY);
            break;
          }
        }
      }
    } catch (e) {
      console.error('Graphviz render error:', e);
      this._element.innerHTML = '<div style="padding:20px;color:#f38ba8;">Render error: ' + e.message + '</div>';
    }
  }

  _onNodeClick(name, mouseX, mouseY) {
    const info = this._nodes.get(name);
    if (!info) return;

    // Send symbol click for info panel (with source to prevent tab switching)
    ws.send(JSON.stringify({type: 'symbol-click', symbol: info.pkg + '::' + name, source: 'class-graph'}));

    // Request children list for selector popup (if not already expanded)
    if (!info.expanded) {
      this._pendingParent = { name, pkg: info.pkg, x: mouseX, y: mouseY };
      ws.send(JSON.stringify({
        type: 'list-class-children',
        className: name,
        packageName: info.pkg,
        panelId: this._panelId
      }));
    }
  }

  showChildSelector(msg) {
    const children = msg.children || [];
    const parentName = msg['parent-name'] || msg.parentName;
    const parentPkg = msg['parent-package'] || msg.parentPackage;

    // Hide selector if no children
    if (children.length === 0) {
      this._childSelector.style.display = 'none';
      // Mark as expanded (no children to show)
      const info = this._nodes.get(parentName);
      if (info) info.expanded = true;
      return;
    }

    // Position near the click
    const pos = this._pendingParent || { x: 100, y: 100 };
    this._childSelector.style.left = pos.x + 'px';
    this._childSelector.style.top = pos.y + 'px';

    // Build list of children
    this._childSelector.innerHTML = '';
    children.forEach(([childName, childPkg]) => {
      // Skip if already in graph
      if (this._nodes.has(childName)) return;

      const item = document.createElement('div');
      item.style.cssText = 'padding:6px 12px;cursor:pointer;white-space:nowrap;';
      item.textContent = childName;
      item.addEventListener('mouseenter', () => {
        item.style.background = 'var(--accent)';
        item.style.color = '#ffffff';
      });
      item.addEventListener('mouseleave', () => {
        item.style.background = '';
        item.style.color = '';
      });
      item.addEventListener('click', (e) => {
        e.stopPropagation();
        this._childSelector.style.display = 'none';
        // Add just this child
        this._focusNode = childName;
        ws.send(JSON.stringify({
          type: 'add-class-child',
          parentName: parentName,
          childName: childName,
          childPackage: childPkg,
          panelId: this._panelId
        }));
      });
      this._childSelector.appendChild(item);
    });

    // If all children already in graph, mark as expanded
    if (this._childSelector.children.length === 0) {
      const info = this._nodes.get(parentName);
      if (info) info.expanded = true;
      return;
    }

    this._childSelector.style.display = 'block';

    // Close on click outside
    const closeHandler = (e) => {
      if (!this._childSelector.contains(e.target)) {
        this._childSelector.style.display = 'none';
        document.removeEventListener('click', closeHandler);
      }
    };
    setTimeout(() => document.addEventListener('click', closeHandler), 0);
  }

  updateGraph(msg) {
    console.log('updateGraph called:', msg);
    if (!this._viz) {
      const pending = pendingClassGraphs.get(this._panelId) || [];
      pending.push(msg);
      pendingClassGraphs.set(this._panelId, pending);
      return;
    }
    if (msg.error) {
      this._element.innerHTML = '<div style="padding:20px;color:#a6adc8;">Error: ' + msg.error + '</div>';
      return;
    }

    this._rootClass = msg['class-name'] || msg.className;
    this._focusNode = this._rootClass;  // Center on root initially
    this._nodes.clear();
    this._edges.clear();

    // Add nodes - format: [name, pkg, [slot1, slot2, ...]]
    (msg.nodes || []).forEach((nodeData) => {
      const name = nodeData[0];
      const pkg = nodeData[1];
      const slots = Array.isArray(nodeData[2]) ? nodeData[2] : [];
      this._nodes.set(name, { pkg, slots, expanded: false });
    });

    // Add edges (from superclass to subclass in msg, stored as 'from->to')
    (msg.edges || []).forEach(([from, to]) => {
      if (this._nodes.has(from) && this._nodes.has(to)) {
        this._edges.add(from + '->' + to);
      }
    });

    this._render();
  }

  addGraph(msg) {
    console.log('addGraph called:', msg);
    if (!this._viz) {
      const pending = pendingClassGraphs.get(this._panelId) || [];
      pending.push(msg);
      pendingClassGraphs.set(this._panelId, pending);
      return;
    }
    if (msg.error) {
      console.warn('class graph expand error:', msg.error);
      return;
    }

    let changed = false;

    (msg.nodes || []).forEach(([name, pkg, slots]) => {
      if (!this._nodes.has(name)) {
        this._nodes.set(name, { pkg, slots: slots || [], expanded: false });
        changed = true;
      }
    });

    (msg.edges || []).forEach(([from, to]) => {
      const edgeKey = from + '->' + to;
      if (!this._edges.has(edgeKey) && this._nodes.has(from) && this._nodes.has(to)) {
        this._edges.add(edgeKey);
        changed = true;
      }
    });

    if (changed) {
      this._render();
    }
  }

  dispose() {
    if (this._panelId) graphvizStates.delete(this._panelId);
    if (this._tooltip && this._tooltip.parentNode) {
      this._tooltip.parentNode.removeChild(this._tooltip);
    }
    if (this._childSelector && this._childSelector.parentNode) {
      this._childSelector.parentNode.removeChild(this._childSelector);
    }
  }
}

class DynamicInspectorPanel {
  constructor() {
    this._element = document.createElement('div');
    this._element.className = 'panel';
    this._panelId = null;
  }
  get element() { return this._element; }
  init(params) {
    this._panelId = params.params?.panelId;
    const headerId = 'header-' + this._panelId;
    const navId = 'nav-' + this._panelId;
    const breadcrumbId = 'breadcrumb-' + this._panelId;
    const contentId = 'content-' + this._panelId;
    this._element.innerHTML = `
      <div class='inspector-nav' id='${navId}'>
        <div class='inspector-breadcrumb' id='${breadcrumbId}'>root</div>
        <div class='inspector-nav-buttons'>
          <button class='nav-up' data-nav-up="${this._panelId}" title='Up (u)' disabled>&#8593;</button>
          <button class='nav-car' data-nav-car="${this._panelId}" title='CAR (a)' disabled>car</button>
          <button class='nav-cdr' data-nav-cdr="${this._panelId}" title='CDR (d)' disabled>cdr</button>
          <button class='nav-left' data-nav-left="${this._panelId}" title='Sibling left (h)' disabled>&#8592;</button>
          <button class='nav-right' data-nav-right="${this._panelId}" title='Sibling right (l)' disabled>&#8594;</button>
          <button class='nav-back' data-history-back="${this._panelId}" title='History back ([)' disabled>&#9664;</button>
          <button class='nav-forward' data-history-forward="${this._panelId}" title='History forward (])' disabled>&#9654;</button>
        </div>
        <span class='nav-position' id='pos-${this._panelId}'></span>
      </div>
      <div class='inspector-eval-bar' id='eval-${this._panelId}'>
        <input type='text' class='inspector-eval-input' data-eval-input='${this._panelId}' placeholder='Eval: * = current, ** = root'>
        <span class='inspector-eval-result' id='eval-result-${this._panelId}'></span>
      </div>
      <div class='panel-header' id='${headerId}' style='display:none;'>
        <button data-inspect-back="${this._panelId}" style='padding:2px 8px;background:var(--bg-tertiary);border:1px solid var(--border);color:var(--fg-primary);border-radius:3px;cursor:pointer;'>&#8592; Back</button>
      </div>
      <div class='panel-content detail-content' id='${contentId}'>
        Loading...
      </div>`;
    // Register this panel's state - use querySelector on this element since it's not in DOM yet
    const contentEl = this._element.querySelector('#' + contentId);
    const headerEl = this._element.querySelector('#' + headerId);
    const navEl = this._element.querySelector('#' + navId);
    const breadcrumbEl = this._element.querySelector('#' + breadcrumbId);
    const posEl = this._element.querySelector('#pos-' + this._panelId);
    console.log('DynamicInspectorPanel init:', this._panelId, 'contentEl:', contentEl, 'headerEl:', headerEl);
    inspectorStates.set(this._panelId, {
      depth: 0,
      element: contentEl,
      header: headerEl,
      nav: navEl,
      breadcrumb: breadcrumbEl,
      position: posEl,
      path: ['root'],        // Breadcrumb path
      history: [],           // Visit history
      historyIndex: -1       // Current position in history
    });
    // Apply any inspection that arrived before this panel initialized.
    const pending = pendingInspections.get(this._panelId);
    if (pending) {
      pendingInspections.delete(this._panelId);
      renderInspection(pending, this._panelId);
    }
  }
  dispose() {
    if (this._panelId) {
      inspectorStates.delete(this._panelId);
    }
  }
}

class TerminalPanel {
  constructor() {
    this._element = document.createElement('div');
    this._element.className = 'terminal-container';
    this._element.id = 'terminal';
  }
  get element() { return this._element; }
  init(params) {
    setTimeout(() => {
      // Build initial theme - use pending theme if available, otherwise default dark
      let initialTheme = { background: '#1e1e1e' };
      if (pendingTheme && pendingTheme.xterm) {
        initialTheme = {};
        Object.entries(pendingTheme.xterm).forEach(([k, v]) => {
          initialTheme[k] = v;
        });
        pendingTheme = null;
      }
      terminal = new Terminal({
        cursorBlink: true,
        fontFamily: "'JetBrains Mono', monospace",
        fontSize: 14,
        theme: initialTheme
      });
      fitAddon = new FitAddon.FitAddon();
      terminal.loadAddon(fitAddon);
      terminal.open(this._element);

      // Send all input directly to Lisp - the editor handles everything
      terminal.onData(data => {
        ws.send(JSON.stringify({type: 'input', data: data}));
      });

      // Click to inspect symbol - use click (not mousedown) to allow text selection
      // Store detected symbol from mousemove for click to use
      let hoveredSymbol = null;
      let mouseDownPos = null;
      let isDragging = false;

      // Track mousedown position to detect drag vs click
      terminal.element.addEventListener('mousedown', (e) => {
        mouseDownPos = { x: e.clientX, y: e.clientY };
        isDragging = false;
      });

      // Inspect on mouseup when no drag selection was made
      terminal.element.addEventListener('mouseup', (e) => {
        // Skip symbol inspect if a selection exists (click-drag selection)
        if (terminal && terminal.hasSelection && terminal.hasSelection()) {
          mouseDownPos = null;
          return;
        }
        // Only inspect if we have a symbol and didn't drag
        if (hoveredSymbol && mouseDownPos && !isDragging) {
          const dx = Math.abs(e.clientX - mouseDownPos.x);
          const dy = Math.abs(e.clientY - mouseDownPos.y);
          // If mouse moved less than 5 pixels, treat as a click not a drag
          if (dx < 5 && dy < 5) {
            if (e.ctrlKey || e.metaKey) {
              // Ctrl+click (or Cmd+click on Mac): open inspector panel
              openInspector(hoveredSymbol, null);
            } else {
              // Regular click: update Symbol Info
              ws.send(JSON.stringify({type: 'symbol-click', symbol: hoveredSymbol}));
            }
            terminal.focus();
          }
        }
        mouseDownPos = null;
      });

      // Enable right-click copy via context menu
      terminal.attachCustomKeyEventHandler((e) => {
        // Allow Ctrl+C to copy when there's a selection (not send SIGINT)
        if (e.ctrlKey && e.key === 'c' && terminal.hasSelection()) {
          return false; // Let browser handle copy
        }
        // Allow Ctrl+Shift+C for copy
        if (e.ctrlKey && e.shiftKey && e.key === 'C') {
          return false;
        }
        // Allow Ctrl+Shift+V for paste
        if (e.ctrlKey && e.shiftKey && e.key === 'V') {
          return false;
        }
        return true; // Let xterm handle other keys
      });

      // Hover highlight box for symbols
      let highlightBox = null;
      const termEl = this._element;
      this._element.addEventListener('mousemove', (e) => {
        if (!terminal) return;
        if (mouseDownPos) {
          const dx = Math.abs(e.clientX - mouseDownPos.x);
          const dy = Math.abs(e.clientY - mouseDownPos.y);
          if (dx > 5 || dy > 5) {
            isDragging = true;
            hoveredSymbol = null;
            if (highlightBox) highlightBox.style.display = 'none';
            terminal.element.style.cursor = '';
            return;
          }
        }
        const rect = terminal.element.getBoundingClientRect();
        const renderer = terminal._core._renderService.dimensions;
        if (!renderer.css.cell.width) return;

        const col = Math.floor((e.clientX - rect.left) / renderer.css.cell.width);
        const row = Math.floor((e.clientY - rect.top) / renderer.css.cell.height);
        const bufferRow = row + terminal.buffer.active.viewportY;
        const symbolInfo = getSymbolBounds(col, bufferRow);

        if (symbolInfo) {
          hoveredSymbol = symbolInfo.symbol;  // Store for click handler
          if (!highlightBox) {
            highlightBox = document.createElement('div');
            highlightBox.style.cssText = 'position:absolute;border:1px solid var(--accent);border-radius:2px;pointer-events:none;z-index:10;';
            termEl.appendChild(highlightBox);
          }
          const cellW = renderer.css.cell.width;
          const cellH = renderer.css.cell.height;
          highlightBox.style.left = (symbolInfo.start * cellW) + 'px';
          highlightBox.style.top = (row * cellH) + 'px';
          highlightBox.style.width = ((symbolInfo.end - symbolInfo.start) * cellW) + 'px';
          highlightBox.style.height = cellH + 'px';
          highlightBox.style.display = 'block';
          terminal.element.style.cursor = 'pointer';
        } else {
          hoveredSymbol = null;  // Clear when not over symbol
          if (highlightBox) highlightBox.style.display = 'none';
          terminal.element.style.cursor = '';
        }
      });

      this._element.addEventListener('mouseleave', () => {
        hoveredSymbol = null;
        if (highlightBox) highlightBox.style.display = 'none';
        if (terminal) terminal.element.style.cursor = '';
      });

      // Refit on resize
      const doFit = () => { try { fitAddon.fit(); } catch(e) {} };
      new ResizeObserver(doFit).observe(this._element);

      // Signal terminal ready after layout has settled
      const signalReady = () => {
        doFit();
        if (ws.readyState === WebSocket.OPEN) {
          ws.send(JSON.stringify({type: 'terminal-ready'}));
        } else {
          ws.addEventListener('open', () => {
            ws.send(JSON.stringify({type: 'terminal-ready'}));
          }, {once: true});
        }
      };

      // Wait for Dockview layout to complete before signaling ready
      setTimeout(signalReady, 300);
    }, 100);
  }
}

// Create Dockview layout
const container = document.getElementById('layout-container');
const dv = window['dockview-core'];

const api = dv.createDockview(container, {
  className: 'icl-dockview-theme',
  disableAutoResizing: false,
  createComponent: (options) => {
    switch (options.name) {
      case 'packages': return new PackagesPanel();
      case 'symbols': return new SymbolsPanel();
      case 'inspector': return new InspectorPanel();
      case 'dynamic-inspector': return new DynamicInspectorPanel();
      case 'speedscope': return new SpeedscopePanel();
      case 'coverage': return new CoveragePanel();
      case 'monaco-coverage': return new MonacoCoveragePanel();
      case 'monaco-source': return new MonacoSourcePanel();
      case 'hashtable': return new HashTablePanel();
      case 'svg': return new SvgPanel();
      case 'html': return new HtmlPanel();
      case 'json': return new JsonPanel();
      case 'image': return new ImagePanel();
      case 'vega-lite': return new VegaLitePanel();
      case 'mermaid': return new MermaidPanel();
      case 'regexp': return new RegexpPanel();
      case 'venn': return new VennPanel();
      case 'graphviz': return new GraphvizPanel();
      case 'terminal': return new TerminalPanel();
    }
  }
});

// Store API globally for dynamic panel creation
dockviewApi = api;

// Layout: Packages/Symbols/Symbol Info side by side at top (1/5 height), REPL Console below (4/5)
api.addPanel({ id: 'terminal', component: 'terminal', title: 'REPL Console' });
api.addPanel({ id: 'packages', component: 'packages', title: 'Packages', position: { referencePanel: 'terminal', direction: 'above' } });
api.addPanel({ id: 'symbols', component: 'symbols', title: 'Symbols', position: { referencePanel: 'packages', direction: 'right' } });
api.addPanel({ id: 'inspector', component: 'inspector', title: 'Symbol Info', position: { referencePanel: 'symbols', direction: 'right' } });

// Set the top row to 20% height (1/5)
setTimeout(() => {
  try {
    const groups = api.groups;
    if (groups && groups.length >= 2) {
      api.layout(window.innerWidth, window.innerHeight);
      // Find the top group and resize
      const topGroup = groups.find(g => g.panels.some(p => p.id === 'packages'));
      if (topGroup) {
        topGroup.api.setSize({ height: Math.floor(window.innerHeight * 0.2) });
      }
    }
  } catch(e) { console.log('Layout resize:', e); }
}, 300);

// Double-click on tab to maximize/restore panel
document.addEventListener('dblclick', (e) => {
  const tab = e.target.closest('.dv-tab');
  if (tab) {
    const activePanel = api.activePanel;
    if (activePanel) {
      if (activePanel.api.isMaximized()) {
        activePanel.api.exitMaximized();
      } else {
        activePanel.api.maximize();
      }
    }
  }
});

// Keyboard shortcut: F11 or Escape to maximize/restore active panel
document.addEventListener('keydown', (e) => {
  // F11 to toggle maximize (prevent default fullscreen)
  if (e.key === 'F11') {
    e.preventDefault();
    const activePanel = api.activePanel;
    if (activePanel) {
      if (activePanel.api.isMaximized()) {
        activePanel.api.exitMaximized();
      } else {
        activePanel.api.maximize();
      }
    }
  }
  // Escape to exit maximized state
  if (e.key === 'Escape' && api.hasMaximizedGroup()) {
    api.exitMaximizedGroup();
  }
});

// Create connection status indicator
const connStatus = document.createElement('div');
connStatus.id = 'connection-status';
connStatus.className = 'connection-status connected';
connStatus.title = 'Connected';
document.body.appendChild(connStatus);

// Create menu button
const menuBtn = document.createElement('button');
menuBtn.className = 'menu-button';
menuBtn.innerHTML = '&#9776;';  // Hamburger icon
menuBtn.title = 'Menu';
menuBtn.onclick = (e) => {
  e.stopPropagation();
  const existing = document.getElementById('app-menu');
  if (existing) { existing.remove(); return; }

  const menu = document.createElement('div');
  menu.id = 'app-menu';
  menu.className = 'app-menu';
  menu.innerHTML = `
    <div class="menu-item" data-action="check-updates">Check for Updates</div>
    <div class="menu-item" data-action="about">About ICL</div>
  `;
  // Setup menu item click handlers (CSP-compliant)
  menu.querySelector('[data-action="check-updates"]').addEventListener('click', () => { menu.remove(); checkForUpdates(); });
  menu.querySelector('[data-action="about"]').addEventListener('click', () => { menu.remove(); showAboutDialog(); });
  menu.style.top = (menuBtn.offsetTop + menuBtn.offsetHeight + 4) + 'px';
  menu.style.right = '8px';
  document.body.appendChild(menu);

  // Close menu on outside click
  setTimeout(() => {
    document.addEventListener('click', function closeMenu() {
      menu.remove();
      document.removeEventListener('click', closeMenu);
    }, { once: true });
  }, 0);
};
document.body.appendChild(menuBtn);