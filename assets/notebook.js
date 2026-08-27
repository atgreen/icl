// notebook.js --- Browser notebook panel for ICL
//
// SPDX-License-Identifier: MIT
// Copyright (C) 2025  Anthony Green <green@moxielogic.com>
//
// A Dockview panel that renders a column of cells. Code cells run on the
// backend via the `run-cell` websocket message and render the `cell-result`
// they get back; markdown cells render locally. Save round-trips the whole
// notebook to disk via `save-notebook`. Depends on globals from browser.js:
// `ws`, `dockviewApi`.

// cellId -> { outputEl, execEl, textarea, wrap, term, termDiv } for routing.
const notebookCells = new Map();
let notebookCellSeq = 0;
let nbPanel = null;   // the live NotebookPanel, for message handlers
function nbNewCellId() { return 'nbcell-' + (++notebookCellSeq); }

// ── ICL editor (xterm) messages ──

function handleCellTerm(msg) {
  const e = notebookCells.get(msg.cellId);
  // Match the REPL terminal: a bare \n must also carry a carriage return,
  // or text after a newline lands at the previous column.
  if (e && e.term) e.term.write(msg.data.replace(/\n/g, '\r\n'));
}

// The editor reports its exact height (visual rows) so we size the xterm to fit.
function handleCellRows(msg) {
  const e = notebookCells.get(msg.cellId);
  if (e && e.term) {
    const want = Math.min(60, Math.max(3, (msg.rows | 0) + 1));
    if (want !== e.term.rows) e.term.resize(e.term.cols, want);
  }
}

function handleCellEdited(msg) {
  const e = notebookCells.get(msg.cellId);
  if (!e) return;
  if (e.term) { try { e.term.dispose(); } catch (x) {} e.term = null; }
  if (e.termDiv) { e.termDiv.remove(); e.termDiv = null; }
  e.textarea.value = msg.source || '';
  e.textarea.style.display = '';
  if (nbPanel) nbPanel._markDirty();
  if (msg.submitted && nbPanel) {
    nbPanel._runCell(e.wrap);
    const action = e.postRun; e.postRun = null;
    if (action === 'advance') nbPanel._advanceFrom(e.wrap);
    else if (action === 'insert') nbPanel._insertAndEditAfter(e.wrap);
    else nbPanel._selectCell(e.wrap);          // inplace -> command mode on this cell
  } else if (nbPanel) {
    nbPanel._selectCell(e.wrap);               // cancelled edit -> command mode
  }
}

// getSymbolBounds() in browser.js is bound to the REPL terminal; this variant
// reads the given cell terminal's buffer.
function nbSymbolBoundsFor(term, col, row) {
  const line = term.buffer.active.getLine(row);
  if (!line) return null;
  const lineText = line.translateToString();
  if (col >= lineText.length || !isSymbolChar(lineText[col])) return null;
  let start = col, end = col;
  while (start > 0 && isSymbolChar(lineText[start - 1])) start--;
  while (end < lineText.length && isSymbolChar(lineText[end])) end++;
  const symbol = lineText.substring(start, end).trim();
  return symbol.length > 0 ? { start, end, symbol } : null;
}

// Give a cell terminal the same symbol hover/click behaviour as the REPL:
// hover highlights a symbol, click updates Symbol Info, Ctrl/Cmd-click inspects.
function nbWireSymbolInteraction(term, containerEl) {
  let hovered = null, downPos = null, dragging = false, box = null;
  term.element.addEventListener('mousedown', (e) => { downPos = { x: e.clientX, y: e.clientY }; dragging = false; });
  term.element.addEventListener('mouseup', (e) => {
    if (term.hasSelection && term.hasSelection()) { downPos = null; return; }
    if (hovered && downPos && !dragging &&
        Math.abs(e.clientX - downPos.x) < 5 && Math.abs(e.clientY - downPos.y) < 5) {
      if (e.ctrlKey || e.metaKey) openInspector(hovered, null);
      else ws.send(JSON.stringify({ type: 'symbol-click', symbol: hovered }));
      term.focus();
    }
    downPos = null;
  });
  containerEl.addEventListener('mousemove', (e) => {
    if (downPos && (Math.abs(e.clientX - downPos.x) > 5 || Math.abs(e.clientY - downPos.y) > 5)) {
      dragging = true; hovered = null; if (box) box.style.display = 'none'; term.element.style.cursor = ''; return;
    }
    const rect = term.element.getBoundingClientRect();
    const dims = term._core._renderService.dimensions;
    if (!dims.css.cell.width) return;
    const col = Math.floor((e.clientX - rect.left) / dims.css.cell.width);
    const row = Math.floor((e.clientY - rect.top) / dims.css.cell.height);
    const si = nbSymbolBoundsFor(term, col, row + term.buffer.active.viewportY);
    if (si) {
      hovered = si.symbol;
      if (!box) {
        box = document.createElement('div');
        box.style.cssText = 'position:absolute;border:1px solid var(--accent);border-radius:2px;pointer-events:none;z-index:10;';
        containerEl.style.position = 'relative';
        containerEl.appendChild(box);
      }
      const cw = dims.css.cell.width, ch = dims.css.cell.height;
      // Row/col were measured from term.element, but the box lives in containerEl;
      // offset by the terminal's position within the container (padding, etc.)
      // or the highlight lands a row/column off.
      const crect = containerEl.getBoundingClientRect();
      box.style.left = (rect.left - crect.left + si.start * cw) + 'px';
      box.style.top = (rect.top - crect.top + row * ch) + 'px';
      box.style.width = ((si.end - si.start) * cw) + 'px'; box.style.height = ch + 'px';
      box.style.display = 'block'; term.element.style.cursor = 'pointer';
    } else {
      hovered = null; if (box) box.style.display = 'none'; term.element.style.cursor = '';
    }
  });
  containerEl.addEventListener('mouseleave', () => {
    hovered = null; if (box) box.style.display = 'none'; term.element.style.cursor = '';
  });
}

// ── Server → client entry points (called from browser.js onmessage) ──

function openNotebookPanel(msg) {
  const panelId = 'notebook';
  if (!dockviewApi) return;
  const existing = dockviewApi.getPanel(panelId);
  if (existing) { dockviewApi.removePanel(existing); }
  // Open as a tab in the main (terminal) group so the notebook fills the
  // large area, with the REPL Console one tab away.
  const hasTerminal = !!dockviewApi.getPanel('terminal');
  dockviewApi.addPanel({
    id: panelId,
    component: 'notebook',
    title: msg.title ? ('Notebook: ' + msg.title) : 'Notebook',
    params: { title: msg.title, path: msg.path, cells: msg.cells || [] },
    position: hasTerminal ? { referencePanel: 'terminal' } : undefined
  });
  const p = dockviewApi.getPanel(panelId);
  if (p && p.api && p.api.setActive) p.api.setActive();
}

function handleCellResult(msg) {
  const entry = notebookCells.get(msg.cellId);
  if (!entry) return;
  nbRenderOutputs(entry.outputEl, msg.outputs || []);
  if (entry.execEl) {
    entry.execEl.textContent = msg.execCount ? ('[' + msg.execCount + ']') : '';
    entry.execEl.style.color = '';   // clear the running indicator colour
  }
  if (entry.timeEl) entry.timeEl.textContent = nbFmtDuration(msg.execMs);
}

// Human-readable cell run time: "12 ms", "0.34 s", "2.1 s".
function nbFmtDuration(ms) {
  if (ms == null || ms < 0) return '';
  if (ms < 1000) return ms + ' ms';
  return (ms / 1000).toFixed(ms < 10000 ? 2 : 1) + ' s';
}

function handleNotebookSaved(msg) {
  if (typeof terminal !== 'undefined' && terminal) {
    terminal.write('\r\n; Notebook saved to ' + msg.path + '\r\n');
  }
}

function handleNotebookError(msg) {
  if (typeof terminal !== 'undefined' && terminal) {
    terminal.write('\r\n; Notebook error: ' + msg.message + '\r\n');
  }
}

function handleNotebookRestarted(msg) {
  // Fresh image: clear the execution-count badges.
  for (const [, e] of notebookCells) { if (e.execEl) e.execEl.textContent = ''; }
  if (typeof terminal !== 'undefined' && terminal) terminal.write('\r\n; Backend image restarted\r\n');
  if (msg.runAll && nbPanel) nbPanel._runAll();
  if (msg.runBelow && nbPanel) {
    const wrap = nbPanel._pendingRunBelowId
      ? nbPanel._cellsEl.querySelector('[data-cell-id="' + nbPanel._pendingRunBelowId + '"]') : null;
    nbPanel._pendingRunBelowId = null;
    if (wrap) nbPanel._runBelow(wrap);
  }
}

// ── Minimal markdown → HTML (headings, emphasis, code, lists, links) ──

function nbEscapeHtml(s) {
  return s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
}

function nbMarkdownInline(t) {
  // Pull KaTeX math out first — its HTML must not be escaped or *-formatted.
  const math = [];
  t = t.replace(/\$\$([^$]+)\$\$|\$([^$\n]+)\$/g, (m, disp, inl) => {
    const tex = disp != null ? disp : inl;
    let html = m;
    try {
      if (typeof katex !== 'undefined')
        html = katex.renderToString(tex, { displayMode: disp != null, throwOnError: false });
    } catch (e) { html = m; }
    math.push(html);
    return ' M' + (math.length - 1) + ' ';
  });
  let out = nbEscapeHtml(t)
    .replace(/`([^`]+)`/g, '<code style="background:var(--bg-tertiary);padding:1px 4px;border-radius:3px;">$1</code>')
    .replace(/\*\*([^*]+)\*\*/g, '<strong>$1</strong>')
    .replace(/(^|[^*])\*([^*]+)\*/g, '$1<em>$2</em>')
    .replace(/\[([^\]]+)\]\(([^)]+)\)/g, '<a href="$2" target="_blank" rel="noopener">$1</a>');
  return out.replace(/ M(\d+) /g, (m, i) => math[+i]);
}

function nbParseRow(line) {
  let s = line.trim();
  if (s.startsWith('|')) s = s.slice(1);
  if (s.endsWith('|')) s = s.slice(0, -1);
  return s.split('|').map(c => c.trim());
}

function nbIsTableSep(line) {
  return !!line && line.includes('-') && /^\s*\|?[\s:|-]+\|?\s*$/.test(line);
}

function nbRenderMarkdown(md) {
  const lines = (md || '').split('\n');
  let html = '', inList = false, inCode = false, code = '', para = [];
  const closeList = () => { if (inList) { html += '</ul>'; inList = false; } };
  const flushPara = () => { if (para.length) { html += '<p>' + nbMarkdownInline(para.join(' ')) + '</p>'; para = []; } };
  const cell = (tag, text) =>
    '<' + tag + ' style="border:1px solid var(--border);padding:3px 8px;' +
    (tag === 'th' ? 'background:var(--bg-tertiary);text-align:left;' : '') + '">' +
    nbMarkdownInline(text) + '</' + tag + '>';
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    if (line.trim().startsWith('```')) {
      flushPara();
      if (inCode) {
        html += '<pre style="background:var(--bg-tertiary);padding:8px;border-radius:4px;overflow:auto;"><code>' + nbEscapeHtml(code) + '</code></pre>';
        code = ''; inCode = false;
      } else { closeList(); inCode = true; }
      continue;
    }
    if (inCode) { code += line + '\n'; continue; }
    // GFM table: a header row followed by a separator row.
    if (line.includes('|') && nbIsTableSep(lines[i + 1])) {
      flushPara(); closeList();
      const header = nbParseRow(line);
      let body = '';
      i += 2;  // consume header + separator
      while (i < lines.length && lines[i].includes('|') && lines[i].trim() !== '') {
        body += '<tr>' + nbParseRow(lines[i]).map(c => cell('td', c)).join('') + '</tr>';
        i++;
      }
      i--;  // the for-loop will re-increment
      html += '<table style="border-collapse:collapse;margin:6px 0;">' +
              '<thead><tr>' + header.map(h => cell('th', h)).join('') + '</tr></thead>' +
              '<tbody>' + body + '</tbody></table>';
      continue;
    }
    const h = line.match(/^(#{1,6})\s+(.*)$/);
    if (h) { flushPara(); closeList(); const n = h[1].length; html += '<h' + n + '>' + nbMarkdownInline(h[2]) + '</h' + n + '>'; continue; }
    // GitHub-style admonition: > [!NOTE] / TIP / IMPORTANT / WARNING / CAUTION
    const adm = line.match(/^>\s*\[!(NOTE|TIP|IMPORTANT|WARNING|CAUTION)\]\s*(.*)$/i);
    if (adm) {
      flushPara(); closeList();
      const type = adm[1].toUpperCase(), body = [];
      if (adm[2].trim()) body.push(adm[2]);
      while (i + 1 < lines.length && /^>\s?/.test(lines[i + 1])) body.push(lines[++i].replace(/^>\s?/, ''));
      const colors = { NOTE: '#4098ff', TIP: '#2ea043', IMPORTANT: '#8957e5', WARNING: '#d29922', CAUTION: '#f85149' };
      const c = colors[type];
      html += '<div style="border-left:4px solid ' + c + ';background:var(--bg-tertiary,#2a2a2a);' +
        'padding:6px 10px;margin:6px 0;border-radius:0 4px 4px 0;">' +
        '<div style="font-weight:bold;color:' + c + ';font-size:0.82em;">' + type + '</div>' +
        nbRenderMarkdown(body.join('\n')) + '</div>';
      continue;
    }
    // Plain blockquote.
    if (/^>\s?/.test(line)) {
      flushPara(); closeList();
      const body = [line.replace(/^>\s?/, '')];
      while (i + 1 < lines.length && /^>\s?/.test(lines[i + 1])) body.push(lines[++i].replace(/^>\s?/, ''));
      html += '<blockquote style="border-left:3px solid var(--border);margin:6px 0;padding:2px 10px;' +
        'color:var(--fg-secondary);">' + nbRenderMarkdown(body.join('\n')) + '</blockquote>';
      continue;
    }
    if (/^\s*[-*]\s+/.test(line)) { flushPara(); if (!inList) { html += '<ul>'; inList = true; } html += '<li>' + nbMarkdownInline(line.replace(/^\s*[-*]\s+/, '')) + '</li>'; continue; }
    if (/^\s*---+\s*$/.test(line)) { flushPara(); closeList(); html += '<hr>'; continue; }
    if (line.trim() === '') { flushPara(); closeList(); continue; }
    closeList();
    para.push(line);
  }
  flushPara();
  closeList();
  if (inCode) html += '<pre><code>' + nbEscapeHtml(code) + '</code></pre>';
  return html;
}

// ── Output rendering (blob kinds mirror the server taxonomy) ──

const NB_RICH_KINDS = new Set(['hash-table', 'vega-lite', 'mermaid', 'json', 'svg', 'html', 'image', 'table', 'arrow']);

// A fixed-height viewport that fits its content to width, then allows
// scroll-to-zoom and drag-to-pan. Used for diagrams that can be large.
function nbZoomContainer(heightPx) {
  const container = document.createElement('div');
  container.style.cssText = 'position:relative;overflow:hidden;height:' + heightPx +
    'px;border:1px solid var(--border);border-radius:4px;background:var(--bg-primary);cursor:grab;';
  const content = document.createElement('div');
  content.style.cssText = 'transform-origin:0 0;position:absolute;top:0;left:0;';
  container.appendChild(content);
  let scale = 1, tx = 4, ty = 4, dragging = false, sx = 0, sy = 0;
  const apply = () => { content.style.transform = 'translate(' + tx + 'px,' + ty + 'px) scale(' + scale + ')'; };
  container.addEventListener('wheel', (e) => {
    e.preventDefault();
    const r = container.getBoundingClientRect(), mx = e.clientX - r.left, my = e.clientY - r.top;
    const ns = Math.min(8, Math.max(0.1, scale * (e.deltaY < 0 ? 1.1 : 1 / 1.1)));
    tx = mx - (mx - tx) * (ns / scale); ty = my - (my - ty) * (ns / scale); scale = ns; apply();
  }, { passive: false });
  container.addEventListener('mousedown', (e) => { dragging = true; sx = e.clientX - tx; sy = e.clientY - ty; container.style.cursor = 'grabbing'; e.preventDefault(); });
  window.addEventListener('mousemove', (e) => { if (dragging) { tx = e.clientX - sx; ty = e.clientY - sy; apply(); } });
  window.addEventListener('mouseup', () => { if (dragging) { dragging = false; container.style.cursor = 'grab'; } });
  const fit = () => {
    const svg = content.querySelector('svg') || content.firstElementChild;
    if (!svg) return;
    const cw = container.clientWidth || 1;
    const sw = (svg.getBoundingClientRect && svg.getBoundingClientRect().width) || svg.clientWidth || cw;
    scale = sw > cw - 8 ? (cw - 8) / sw : 1; tx = 4; ty = 4; apply();
  };
  const autofit = () => {
    let n = 90;
    const step = () => {
      if (container.clientWidth > 0 && (content.querySelector('svg') || content.firstElementChild)) fit();
      else if (--n > 0) requestAnimationFrame(step);
    };
    requestAnimationFrame(step);
  };
  const hint = document.createElement('div');
  hint.textContent = 'scroll to zoom · drag to pan';
  hint.style.cssText = 'position:absolute;bottom:2px;right:6px;font:10px sans-serif;color:var(--fg-secondary);pointer-events:none;opacity:.6;';
  container.appendChild(hint);
  return { container, content, autofit };
}

// Render a rich blob by reusing browser.js's existing panel renderers.
function nbRenderRich(outEl, o) {
  const div = document.createElement('div');
  div.style.cssText = 'padding:6px 8px;';
  try {
    switch (o.kind) {
      case 'hash-table': renderHashTable(div, o.count, o.entries); break;
      case 'vega-lite':
        // A definite height lets Vega-Lite's height:"container" size correctly.
        div.style.height = '340px'; div.style.overflow = 'auto';
        renderVegaLite(div, o.payload);
        break;
      case 'mermaid': { const z = nbZoomContainer(360); renderMermaid(z.content, o.payload); z.autofit(); div.appendChild(z.container); break; }
      case 'json': renderJson(div, o.payload); break;
      case 'svg': { const z = nbZoomContainer(360); z.content.innerHTML = o.payload; z.autofit(); div.appendChild(z.container); break; }
      case 'html': {
        const f = document.createElement('iframe');
        f.style.cssText = 'width:100%;height:300px;border:none;background:white;';
        f.sandbox = 'allow-same-origin'; f.srcdoc = o.payload;
        div.appendChild(f); break;
      }
      case 'image': renderImage(div, o.payload); break;
      case 'table': nbRenderTable(div, o); break;
      case 'arrow': nbRenderArrow(div, o); break;
    }
    // Grids/charts/diagrams are cramped in a cell; let the user pop one out to
    // fill the window and restore it.
    if (['arrow', 'table', 'vega-lite', 'mermaid', 'svg', 'html', 'image'].includes(o.kind)) {
      nbAddMaximize(div);
    }
    if (['svg', 'mermaid'].includes(o.kind)) {
      nbAddDownload(div, 'Download SVG', () => {
        const svg = div.querySelector('svg');
        if (svg) nbDownload('diagram.svg', new XMLSerializer().serializeToString(svg), 'image/svg+xml');
      });
    } else if (o.kind === 'image') {
      nbAddDownload(div, 'Download image', () => {
        const img = div.querySelector('img');
        if (img && img.src) { const a = document.createElement('a'); a.href = img.src; a.download = 'image'; a.click(); }
      });
    }
  } catch (e) { div.textContent = 'render error: ' + e; }
  outEl.appendChild(div);
}

// Render a base64 Arrow IPC stream (from cl-arrow) as a Perspective pivot grid —
// the typed, binary path: no JSON, columns keep their Arrow types.
// Add a maximize/restore toggle to a rich-output CONTAINER.  Maximized, it fills
// the window (fixed overlay) so a Perspective grid / chart is usable; restore
// returns it to the in-cell size.  Esc also restores.
// Trigger a browser download of DATA (string or Blob) as FILENAME.
function nbDownload(filename, data, type) {
  const blob = data instanceof Blob ? data : new Blob([data], { type: type || 'text/plain' });
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url; a.download = filename; a.click();
  setTimeout(() => URL.revokeObjectURL(url), 1000);
}

// Add a ⬇ download button to a rich-output CONTAINER (sits left of maximize).
// PRODUCER is an (async) callback that performs the download.
function nbAddDownload(container, title, producer) {
  container.style.position = 'relative';
  const btn = document.createElement('button');
  btn.textContent = '⬇'; btn.title = title || 'Download';
  btn.style.cssText =
    'position:absolute;top:4px;right:34px;z-index:6;cursor:pointer;line-height:1;' +
    'border:1px solid var(--border);border-radius:4px;background:var(--bg-secondary);' +
    'color:var(--fg-secondary);font-size:12px;padding:2px 5px;';
  btn.addEventListener('click', async (e) => { e.stopPropagation(); try { await producer(); } catch (err) {} });
  container.appendChild(btn);
}

function nbAddMaximize(container) {
  container.style.position = 'relative';
  const btn = document.createElement('button');
  btn.textContent = '⤢';
  btn.title = 'Maximize (Esc to restore)';
  btn.style.cssText =
    'position:absolute;top:4px;right:6px;z-index:6;cursor:pointer;line-height:1;' +
    'border:1px solid var(--border);border-radius:4px;background:var(--bg-secondary);' +
    'color:var(--fg-secondary);font-size:12px;padding:2px 5px;';
  let saved = null, onKey = null;
  const restore = () => {
    if (saved === null) return;
    container.setAttribute('style', saved); saved = null;
    btn.textContent = '⤢'; btn.title = 'Maximize (Esc to restore)';
    if (onKey) { document.removeEventListener('keydown', onKey); onKey = null; }
    container.appendChild(btn);
    window.dispatchEvent(new Event('resize'));
  };
  const maximize = () => {
    saved = container.getAttribute('style') || '';
    container.style.cssText =
      'position:fixed;inset:0;z-index:1000;margin:0;padding:8px;box-sizing:border-box;' +
      'background:var(--bg-primary);overflow:auto;';
    btn.textContent = '⤡'; btn.title = 'Restore';
    onKey = (e) => { if (e.key === 'Escape') { e.stopPropagation(); restore(); } };
    document.addEventListener('keydown', onKey);
    container.appendChild(btn);   // keep the button on top after cssText reset
    window.dispatchEvent(new Event('resize'));
  };
  btn.addEventListener('click', (e) => { e.stopPropagation(); (saved === null ? maximize : restore)(); });
  container.appendChild(btn);
}

// Restore a viewer's config (saved pivot state if the notebook carried one,
// else DEFAULTCONFIG), register the viewer on its cell for save, and mark the
// notebook dirty whenever the user changes the pivot/sort/filter live.
async function nbApplyViewerConfig(div, viewer, defaultConfig) {
  const wrap = div.closest && div.closest('[data-cell-id]');
  const entry = wrap && notebookCells.get(wrap.dataset.cellId);
  let cfg = defaultConfig;
  if (entry && entry.savedViewConfig) {
    try { cfg = JSON.parse(entry.savedViewConfig); } catch (e) {}
  }
  await viewer.restore(cfg);
  if (entry) {
    entry.viewer = viewer;
    viewer.addEventListener('perspective-config-update', () => { if (nbPanel) nbPanel._markDirty(); });
  }
  nbFitViewer(div, viewer);
  nbAddDownload(div, 'Download the current view as CSV', async () => {
    const view = await viewer.getView();
    const csv = await view.to_csv();
    nbDownload('table.csv', csv, 'text/csv');
  });
}

// Size a grid's container to its actual (post-pivot) row count so a small
// result doesn't get a big empty box; capped so large tables stay scrollable.
// Recomputes when the user pivots/filters. No-op while maximized (fixed inset).
function nbFitViewer(container, viewer) {
  const MIN = 88, MAX = 480, ROW = 22, CHROME = 66;   // header + borders
  const fit = async () => {
    try {
      if (container.style.position === 'fixed') return;   // maximized — leave it
      if (!viewer.getView) return;
      const view = await viewer.getView();
      const rows = await view.num_rows();
      container.style.height =
        Math.max(MIN, Math.min(CHROME + (rows || 0) * ROW, MAX)) + 'px';
    } catch (e) {}
  };
  fit();
  viewer.addEventListener('perspective-config-update', fit);
}

function nbRenderArrow(div, o) {
  const b64 = o.payload || '';
  if (!b64) { div.textContent = 'empty table'; return; }
  div.style.height = '380px';
  div.style.overflow = 'hidden';
  const host = document.createElement('div');
  host.style.cssText = 'width:100%;height:100%;';
  div.appendChild(host);
  // base64 -> ArrayBuffer
  const bin = atob(b64);
  const bytes = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i);
  nbLoadPerspective()
    .then(async ({ worker }) => {
      const table = await worker.table(bytes.buffer);   // <-- Arrow IPC straight in
      const viewer = document.createElement('perspective-viewer');
      viewer.style.cssText = 'width:100%;height:100%;';
      host.appendChild(viewer);
      await viewer.load(table);
      const dark = document.body.classList.contains('dark');
      await nbApplyViewerConfig(div, viewer, { plugin: 'Datagrid', theme: dark ? 'Pro Dark' : 'Pro Light' });
    })
    .catch((err) => { host.textContent = 'Perspective load failed: ' + err; });
}

// ── Perspective pivot grid — vendored (Apache-2.0), served self-hosted from
// /assets/perspective so grids work fully offline. The ESM modules load their
// WASM via `new URL("../wasm/*.wasm", import.meta.url)`, so the cdn/ + wasm/
// layout under assets/perspective/ must be preserved.
let nbPerspectivePromise = null;
function nbLoadPerspective() {
  if (nbPerspectivePromise) return nbPerspectivePromise;
  const base = '/assets/perspective';
  nbPerspectivePromise = (async () => {
    // Viewer theme + d3fc chart stylesheets (once).
    for (const href of [`${base}/css/themes.css`, `${base}/css/perspective-viewer-d3fc.css`]) {
      const css = document.createElement('link');
      css.rel = 'stylesheet'; css.href = href;
      document.head.appendChild(css);
    }
    // Engine + viewer + plugins. Importing the viewer/plugin modules registers
    // the <perspective-viewer> custom element, the Datagrid, and the d3fc chart
    // suite (bar/line/scatter/heatmap/treemap/…) as selectable plugins.
    const engine = await import(`${base}/cdn/perspective.js`);
    await import(`${base}/cdn/perspective-viewer.js`);
    await import(`${base}/cdn/perspective-viewer-datagrid.js`);
    await import(`${base}/cdn/perspective-viewer-d3fc.js`);
    // Perspective 3.x exports `worker` as a named export; older builds hang it
    // off the default export.
    const mkWorker = engine.worker || (engine.default && engine.default.worker);
    const worker = await mkWorker.call(engine);
    return { worker };
  })().catch((e) => { nbPerspectivePromise = null; throw e; });
  return nbPerspectivePromise;
}

// Render a tabular blob {columns:[...], rows:[[...],...]} as an interactive,
// pivotable Perspective grid.
function nbRenderTable(div, o) {
  const columns = o.columns || [];
  const rows = o.rows || [];
  if (!columns.length || !rows.length) {
    div.textContent = 'empty table';
    return;
  }
  div.style.height = '380px';
  div.style.overflow = 'hidden';
  const host = document.createElement('div');
  host.style.cssText = 'width:100%;height:100%;';
  div.appendChild(host);
  // Row-object form; '' (backend's missing-cell marker) becomes null so
  // Perspective infers a column's type from its present values.
  const data = rows.map((r) => {
    const obj = {};
    columns.forEach((c, i) => { obj[c] = (r[i] === '') ? null : r[i]; });
    return obj;
  });
  nbLoadPerspective()
    .then(async ({ worker }) => {
      const table = await worker.table(data);
      const viewer = document.createElement('perspective-viewer');
      viewer.style.cssText = 'width:100%;height:100%;';
      host.appendChild(viewer);
      await viewer.load(table);
      const dark = document.body.classList.contains('dark');
      await nbApplyViewerConfig(div, viewer, { plugin: 'Datagrid', theme: dark ? 'Pro Dark' : 'Pro Light' });
    })
    .catch((err) => { host.textContent = 'Perspective load failed: ' + err; });
}

// The printed value, collapsed under a toggle (shown when a rich view exists).
function nbRenderValueToggle(outEl, payload) {
  const wrap = document.createElement('div');
  const toggle = document.createElement('div');
  toggle.textContent = '▸ printed value';
  toggle.style.cssText = 'cursor:pointer;font:11px monospace;color:var(--fg-secondary);padding:2px 8px;user-select:none;';
  const val = document.createElement('div');
  val.style.cssText = 'display:none;white-space:pre-wrap;font-family:monospace;font-size:13px;padding:2px 8px;color:var(--fg-primary);';
  val.textContent = payload;
  toggle.onclick = () => {
    const show = val.style.display === 'none';
    val.style.display = show ? 'block' : 'none';
    toggle.textContent = (show ? '▾' : '▸') + ' printed value';
  };
  wrap.appendChild(toggle); wrap.appendChild(val);
  outEl.appendChild(wrap);
}

function nbRenderTextBlob(outEl, o) {
  const div = document.createElement('div');
  const base = 'white-space:pre-wrap;font-family:monospace;font-size:13px;padding:4px 8px;';
  switch (o.kind) {
    case 'value':  div.style.cssText = base + 'color:var(--fg-primary);'; div.textContent = o.payload; break;
    case 'stdout': div.style.cssText = base + 'color:var(--fg-secondary);'; div.textContent = o.payload; break;
    case 'error':  div.style.cssText = base + 'color:#e06c75;'; div.textContent = o.payload; break;
    case 'markdown':
      div.style.cssText = 'padding:4px 12px;color:var(--fg-primary);line-height:1.5;font-family:system-ui,-apple-system,sans-serif;';
      div.innerHTML = nbRenderMarkdown(o.payload); break;
    default: div.style.cssText = base + 'color:var(--fg-secondary);'; div.textContent = o.payload;
  }
  outEl.appendChild(div);
}

function nbRenderOutputs(outEl, outputs) {
  outEl.innerHTML = '';
  if (!outputs || outputs.length === 0) { outEl.style.display = 'none'; return; }
  outEl.style.display = 'block';
  const hasRich = outputs.some(o => NB_RICH_KINDS.has(o.kind));
  for (const o of outputs) {
    if (NB_RICH_KINDS.has(o.kind)) { nbRenderRich(outEl, o); }
    else if (hasRich && o.kind === 'value') { nbRenderValueToggle(outEl, o.payload); }
    else { nbRenderTextBlob(outEl, o); }
  }
}

// ── Panel ──

class NotebookPanel {
  constructor() {
    this._element = document.createElement('div');
    this._element.style.cssText =
      'position:absolute;inset:0;display:flex;flex-direction:column;' +
      'background:var(--bg-primary);color:var(--fg-primary);overflow:auto;';
    this._cellsEl = null;
    this._path = null;
    this._title = 'Untitled';
  }
  get element() { return this._element; }

  init(params) {
    nbPanel = this;
    const p = params.params || {};
    this._path = p.path || null;
    this._title = p.title || 'Untitled';
    this._dirty = false;
    this._selected = null;
    this._dPending = false;
    let cells = p.cells && p.cells.length ? p.cells : [{ kind: 'code', source: '', outputs: [] }];
    this._build(cells);
    // Command-mode key handling: the panel is focusable and, when no cell
    // editor is active, keys drive cell selection/insertion/deletion.
    this._element.tabIndex = -1;
    this._element.addEventListener('keydown', (e) => this._onKeyDown(e));
    // Autosave every 30s once the notebook has a path.
    if (!this._autosaveTimer) {
      this._autosaveTimer = setInterval(() => {
        if (this._dirty && this._path) this._writeNotebook(this._path);
      }, 30000);
    }
  }

  _markDirty() { this._dirty = true; if (this._dirtyEl) this._dirtyEl.textContent = '●'; }

  // Render a cell's tag chips from its stored tags.
  _renderTags(cellId) {
    const e = notebookCells.get(cellId);
    if (!e || !e.tagsEl) return;
    e.tagsEl.innerHTML = '';
    for (const t of (e.tags || [])) {
      const chip = document.createElement('span');
      chip.textContent = t;
      chip.style.cssText = 'font:10px monospace;background:var(--bg-tertiary,#2a2a2a);' +
        'color:var(--fg-secondary);border:1px solid var(--border);border-radius:8px;padding:0 6px;';
      e.tagsEl.appendChild(chip);
    }
  }

  // Edit a cell's tags (comma/space-separated); persists with the notebook.
  _editTags(wrap) {
    const e = notebookCells.get(wrap.dataset.cellId);
    if (!e) return;
    const current = (e.tags || []).join(', ');
    const input = window.prompt('Cell tags (comma-separated) — e.g. parameters, hide-input, slide:', current);
    if (input == null) return;
    e.tags = Array.from(new Set(input.split(/[,\s]+/).map(s => s.trim()).filter(Boolean)));
    this._renderTags(wrap.dataset.cellId);
    this._applyHideInput(wrap);
    this._markDirty();
  }

  // Toggle the "hide-input" tag and show/hide the cell's editor accordingly.
  _toggleHideInput(wrap) {
    const e = notebookCells.get(wrap.dataset.cellId);
    if (!e) return;
    const hidden = !(e.tags || []).includes('hide-input');
    e.tags = (e.tags || []).filter(t => t !== 'hide-input');
    if (hidden) e.tags.push('hide-input');
    this._applyHideInput(wrap);
    this._renderTags(wrap.dataset.cellId);
    this._markDirty();
  }
  // Reflect the "hide-input" tag: collapse the input to show only output.
  _applyHideInput(wrap) {
    const e = notebookCells.get(wrap.dataset.cellId);
    if (!e) return;
    const hide = (e.tags || []).includes('hide-input');
    if (e.textarea) e.textarea.style.display = hide ? 'none' : '';
    if (e.termDiv) e.termDiv.style.display = hide ? 'none' : '';
  }

  async _writeNotebook(path) {
    const cells = [];
    for (const w of this._cellsEl.querySelectorAll('[data-cell-id]')) {
      const cell = { kind: w.dataset.kind, source: w.querySelector('textarea').value };
      // Capture the live Perspective grid config (pivots/sort/filter/aggregates)
      // so it survives save/reopen. A cell has a viewer only after it has run.
      const entry = notebookCells.get(w.dataset.cellId);
      if (entry && entry.viewer && entry.viewer.save) {
        try { cell.viewConfig = JSON.stringify(await entry.viewer.save()); } catch (e) {}
      }
      if (entry && entry.tags && entry.tags.length) cell.tags = entry.tags;
      cells.push(cell);
    }
    ws.send(JSON.stringify({ type: 'save-notebook', path, title: this._title, cells }));
    this._dirty = false; if (this._dirtyEl) this._dirtyEl.textContent = '';
  }

  // Run-and-advance: edit the next cell, creating a trailing code cell if none.
  _advanceFrom(wrap) {
    const next = wrap.nextElementSibling;
    if (next && next.dataset && next.dataset.cellId) {
      if (next.dataset.kind === 'code') this._startIclEdit(next);
      else { const ta = next.querySelector('textarea'); if (ta) { this._editMarkdown(next); } }
    } else {
      this._insertAndEditAfter(wrap);
    }
  }

  // Run-and-insert: add a fresh code cell after WRAP and edit it.
  _insertAndEditAfter(wrap) {
    const w = this._appendCell('code', '', [], wrap);
    this._startIclEdit(w);
  }

  // Open the real ICL editor (an xterm bound to a backend editor session)
  // for a code cell, seeded with its current source.
  _startIclEdit(wrap) {
    const entry = notebookCells.get(wrap.dataset.cellId);
    if (!entry || entry.term) return;
    entry.textarea.style.display = 'none';
    const termDiv = document.createElement('div');
    termDiv.style.cssText = 'padding:4px 8px;background:var(--bg-primary);';
    wrap.insertBefore(termDiv, entry.outputEl);
    const theme = (typeof terminal !== 'undefined' && terminal && terminal.options)
      ? terminal.options.theme : undefined;
    const initialRows = Math.min(30, Math.max(4, entry.textarea.value.split('\n').length + 1));
    const term = new Terminal({
      cursorBlink: true, fontFamily: "'JetBrains Mono', monospace", fontSize: 13,
      cols: 100, rows: initialRows, theme, convertEol: false
    });
    term.open(termDiv);
    // xterm sends plain \r for both Enter and Shift-Enter; deliver Shift-Enter
    // as the kitty sequence the editor decodes as :shift-enter (submit).
    term.attachCustomKeyEventHandler((e) => {
      // Shift/Ctrl/Alt-Enter all submit; the modifier picks the post-run action.
      if (e.type === 'keydown' && e.key === 'Enter' && (e.shiftKey || e.ctrlKey || e.altKey)) {
        const entry = notebookCells.get(wrap.dataset.cellId);
        if (entry) entry.postRun = e.ctrlKey ? 'inplace' : (e.altKey ? 'insert' : 'advance');
        ws.send(JSON.stringify({ type: 'cell-key', cellId: wrap.dataset.cellId, data: '\x1b[13;2u' }));
        return false;
      }
      return true;
    });
    term.onData(d => ws.send(JSON.stringify({ type: 'cell-key', cellId: wrap.dataset.cellId, data: d })));
    nbWireSymbolInteraction(term, termDiv);
    term.focus();
    entry.term = term;
    entry.termDiv = termDiv;
    ws.send(JSON.stringify({ type: 'edit-cell', cellId: wrap.dataset.cellId, source: entry.textarea.value }));
  }

  _button(label, fn, title) {
    const b = document.createElement('button');
    b.type = 'button';
    b.textContent = label;
    b.title = title || label;
    b.style.cssText =
      'font:12px sans-serif;padding:3px 8px;cursor:pointer;border:1px solid var(--border);' +
      'border-radius:4px;background:var(--bg-tertiary);color:var(--fg-primary);position:relative;z-index:3;';
    // Keep the click from reaching an underlying cell editor / terminal.
    b.addEventListener('mousedown', (e) => e.stopPropagation());
    b.addEventListener('click', (e) => { e.stopPropagation(); fn(); });
    return b;
  }

  _build(cells) {
    this._element.innerHTML = '';
    const bar = document.createElement('div');
    bar.style.cssText =
      'display:flex;gap:8px;padding:6px 8px;border-bottom:1px solid var(--border);' +
      'position:sticky;top:0;background:var(--bg-secondary);z-index:1;';
    bar.appendChild(this._button('+ Code', () => this._addCell('code'), 'Add a code cell'));
    bar.appendChild(this._button('+ Markdown', () => this._addCell('markdown'), 'Add a markdown cell'));
    bar.appendChild(this._button('Run all', () => this._runAll(), 'Run every cell'));
    bar.appendChild(this._button('Run ↑', () => { const c = this._selectedOrFirst(); if (c) this._runAbove(c); }, 'Run all cells above the selected cell'));
    bar.appendChild(this._button('Run ↓', () => { const c = this._selectedOrFirst(); if (c) this._runBelow(c); }, 'Run the selected cell and everything below'));
    bar.appendChild(this._button('⏹ Interrupt', () => ws.send(JSON.stringify({ type: 'notebook-interrupt' })), 'Interrupt the running cell'));
    bar.appendChild(this._button('⟳ Restart', () => ws.send(JSON.stringify({ type: 'notebook-restart' })), 'Restart the backend image'));
    bar.appendChild(this._button('⟳▶ Run all', () => ws.send(JSON.stringify({ type: 'notebook-restart', runAll: true })), 'Restart the image and run all cells'));
    bar.appendChild(this._button('⟳↓ below', () => { const c = this._selectedOrFirst(); if (c) this._restartRunBelow(c); }, 'Restart the image and run the selected cell and below'));
    bar.appendChild(this._button('Contents', () => this._toggleToc(), 'Toggle the table of contents'));
    bar.appendChild(this._button('Clear outputs', () => this._clearAllOutputs(), 'Clear all cell outputs'));
    bar.appendChild(this._button('Find', () => this._openFind(), 'Find & replace across cells'));
    bar.appendChild(this._button('Save', () => this._save(), 'Save the notebook (.iclnb)'));
    bar.appendChild(this._button('→.lisp', () => this._exportLisp(), 'Export to a loadable .lisp file'));
    bar.appendChild(this._button('→html', () => this._exportHtml(), 'Export to a self-contained HTML file'));
    bar.appendChild(this._button('→slides', () => this._exportSlides(), 'Export a reveal.js slide deck (use slide/subslide/fragment/notes/skip tags)'));
    this._dirtyEl = document.createElement('span');
    this._dirtyEl.title = 'Unsaved changes';
    this._dirtyEl.style.cssText = 'color:var(--accent,#4098ff);margin-left:auto;padding:0 8px;font-size:14px;';
    this._dirtyEl.textContent = this._dirty ? '●' : '';
    bar.appendChild(this._dirtyEl);
    this._element.appendChild(bar);

    this._cellsEl = document.createElement('div');
    this._cellsEl.style.cssText = 'flex:1;padding:8px;';
    this._element.appendChild(this._cellsEl);

    for (const c of cells) this._appendCell(c.kind || 'code', c.source || '', c.outputs || [], undefined, c.viewConfig, c.tags);
  }

  _appendCell(kind, source, outputs, afterEl, viewConfig, tags) {
    const cellId = nbNewCellId();
    const wrap = document.createElement('div');
    wrap.dataset.cellId = cellId;
    wrap.dataset.kind = kind;
    wrap.style.cssText =
      'margin-bottom:10px;border:1px solid var(--border);border-radius:6px;overflow:hidden;';

    // header row: exec badge + kind + controls
    const head = document.createElement('div');
    head.style.cssText =
      'display:flex;align-items:center;gap:6px;padding:2px 6px;background:var(--bg-secondary);' +
      'font:11px monospace;color:var(--fg-secondary);position:relative;z-index:2;';
    const exec = document.createElement('span');
    exec.style.cssText = 'min-width:34px;';
    exec.textContent = '';
    const kindLabel = document.createElement('span');
    kindLabel.textContent = kind;
    kindLabel.style.cssText = 'flex:1;';
    // Per-cell execution time (filled in on cell-result).
    const timeEl = document.createElement('span');
    timeEl.style.cssText = 'color:var(--fg-secondary);opacity:0.7;margin-right:6px;';
    // Cell tags (chips) + editor.
    const tagsEl = document.createElement('span');
    tagsEl.style.cssText = 'display:inline-flex;gap:4px;margin-right:6px;';
    head.appendChild(exec);
    head.appendChild(kindLabel);
    head.appendChild(timeEl);
    head.appendChild(tagsEl);
    head.appendChild(this._button('🏷', () => this._editTags(wrap), 'Edit cell tags'));
    head.appendChild(this._button('Run', () => this._runCell(wrap), 'Run this cell'));
    head.appendChild(this._button('▾', (function () {
      const o = notebookCells.get(cellId).outputEl;
      o.dataset.collapsed = o.dataset.collapsed === '1' ? '' : '1';
      o.style.display = (o.dataset.collapsed === '1' || !o.firstChild) ? 'none' : 'block';
    }), 'Collapse/expand output'));
    head.appendChild(this._button('⊟', () => this._toggleHideInput(wrap), 'Hide/show input (only output)'));
    head.appendChild(this._button('∅', () => this._clearCellOutput(wrap), "Clear this cell's output"));
    head.appendChild(this._button('↑', () => this._moveCell(wrap, -1), 'Move cell up'));
    head.appendChild(this._button('↓', () => this._moveCell(wrap, 1), 'Move cell down'));
    head.appendChild(this._button('⧉', () => this._duplicateCell(wrap), 'Duplicate cell'));
    head.appendChild(this._button('✕', () => this._removeCell(wrap), 'Delete cell'));
    // Clicking the header (not a button — those stopPropagation) selects the cell.
    head.addEventListener('click', () => this._selectCell(wrap));

    // input textarea
    const ta = document.createElement('textarea');
    ta.value = source;
    ta.spellcheck = false;
    ta.style.cssText =
      'width:100%;box-sizing:border-box;border:none;outline:none;resize:vertical;' +
      'min-height:2.4em;padding:6px 8px;font-family:monospace;font-size:13px;' +
      'background:var(--bg-primary);color:var(--fg-primary);';
    ta.addEventListener('keydown', (e) => {
      if (e.key === 'Enter' && e.shiftKey) { e.preventDefault(); this._runCell(wrap); }
    });
    // auto-grow
    const grow = () => { ta.style.height = 'auto'; ta.style.height = (ta.scrollHeight + 2) + 'px'; };
    ta.addEventListener('input', grow);
    setTimeout(grow, 0);

    // output (scrollable, capped height for long stdout/tables)
    const out = document.createElement('div');
    out.style.cssText = 'border-top:1px solid var(--border);display:none;max-height:480px;overflow:auto;';
    nbRenderOutputs(out, outputs);

    wrap.appendChild(head);
    wrap.appendChild(ta);
    wrap.appendChild(out);
    notebookCells.set(cellId, { outputEl: out, execEl: exec, timeEl, tagsEl,
                                tags: Array.isArray(tags) ? tags.slice() : [], textarea: ta, wrap,
                                viewer: null, savedViewConfig: viewConfig || null });
    this._renderTags(cellId);
    this._applyHideInput(wrap);

    if (kind === 'markdown') {
      out.style.cursor = 'pointer';
      out.title = 'Click to edit';
      out.addEventListener('click', () => this._editMarkdown(wrap));
      ta.addEventListener('blur', () => { if (ta.value.trim() !== '') this._renderMarkdown(wrap); });
      if (source.trim() !== '') this._renderMarkdown(wrap);
    } else {
      // Code cell: the textarea is a resting display of the source; clicking it
      // opens the real ICL editor (paredit, indent, highlighting, completion).
      ta.readOnly = true;
      ta.style.cursor = 'text';
      ta.title = 'Click to edit in the ICL editor';
      ta.addEventListener('mousedown', (e) => { e.preventDefault(); this._startIclEdit(wrap); });
    }

    if (afterEl && afterEl.nextSibling) {
      this._cellsEl.insertBefore(wrap, afterEl.nextSibling);
    } else if (afterEl) {
      this._cellsEl.appendChild(wrap);
    } else {
      this._cellsEl.appendChild(wrap);
    }
    return wrap;
  }

  _addCell(kind) {
    const active = document.activeElement;
    const afterEl = active && active.closest ? active.closest('[data-cell-id]') : null;
    const w = this._appendCell(kind, '', [], afterEl);
    if (kind === 'markdown') {
      const ta = w.querySelector('textarea');
      if (ta) ta.focus();
    } else {
      this._startIclEdit(w);   // open the ICL editor for a fresh code cell
    }
    this._markDirty();
  }

  _removeCell(wrap) {
    notebookCells.delete(wrap.dataset.cellId);
    wrap.remove();
    this._markDirty();
  }

  _clearCellOutput(wrap) {
    const e = notebookCells.get(wrap.dataset.cellId);
    if (!e) return;
    e.outputEl.innerHTML = ''; e.outputEl.style.display = 'none';
    if (e.execEl) { e.execEl.textContent = ''; e.execEl.style.color = ''; }
  }

  _clearAllOutputs() {
    for (const w of this._cellsEl.querySelectorAll('[data-cell-id]')) this._clearCellOutput(w);
  }

  _moveCell(wrap, dir) {
    const sib = dir < 0 ? wrap.previousElementSibling : wrap.nextElementSibling;
    if (!(sib && sib.dataset && sib.dataset.cellId)) return;
    if (dir < 0) this._cellsEl.insertBefore(wrap, sib);
    else this._cellsEl.insertBefore(sib, wrap);
    this._markDirty();
  }

  _duplicateCell(wrap) {
    const e = notebookCells.get(wrap.dataset.cellId);
    this._appendCell(wrap.dataset.kind, e.textarea.value, [], wrap);
    this._markDirty();
  }

  _cells() { return Array.from(this._cellsEl.querySelectorAll('[data-cell-id]')); }

  // Select a cell for command mode (highlight + focus the panel for keys).
  _selectCell(wrap) {
    if (!wrap) return;
    for (const w of this._cells()) w.style.outline = '';
    wrap.style.outline = '2px solid var(--accent, #4098ff)';
    this._selected = wrap;
    wrap.scrollIntoView({ block: 'nearest' });
    this._element.focus();
  }

  // Change a cell between :code and :markdown by rebuilding it in place.
  _setCellKind(wrap, kind) {
    if (wrap.dataset.kind === kind) return;
    const src = notebookCells.get(wrap.dataset.cellId).textarea.value;
    const w = this._appendCell(kind, src, [], wrap);
    this._removeCell(wrap);
    this._selectCell(w);
    this._markDirty();
  }

  // Command-mode keyboard shortcuts (active only when no editor is focused).
  _onKeyDown(e) {
    if ((e.ctrlKey || e.metaKey) && (e.key === 'f' || e.key === 'F')) {
      e.preventDefault(); this._openFind(); return;
    }
    const editing = this._cells().some(w => notebookCells.get(w.dataset.cellId)?.term)
      || (document.activeElement && document.activeElement.tagName === 'TEXTAREA'
          && document.activeElement.style.display !== 'none');
    if (editing) return;
    const sel = this._selected;
    const list = this._cells();
    const idx = sel ? list.indexOf(sel) : -1;
    const k = e.key;
    if (k === 'd') {
      if (this._dPending) {                       // dd -> delete
        this._dPending = false;
        if (sel) { const nx = sel.nextElementSibling || sel.previousElementSibling; this._removeCell(sel); this._selectCell((nx && nx.dataset && nx.dataset.cellId) ? nx : this._cells()[0]); }
        e.preventDefault();
      } else { this._dPending = true; setTimeout(() => { this._dPending = false; }, 600); }
      return;
    }
    this._dPending = false;
    switch (k) {
      case 'Enter':
        if (sel) { e.preventDefault(); if (sel.dataset.kind === 'code') this._startIclEdit(sel); else this._editMarkdown(sel); }
        break;
      case 'ArrowDown': case 'j':
        if (idx >= 0 && idx < list.length - 1) { this._selectCell(list[idx + 1]); e.preventDefault(); }
        break;
      case 'ArrowUp': case 'k':
        if (idx > 0) { this._selectCell(list[idx - 1]); e.preventDefault(); }
        break;
      case 'a': { const w = this._appendCell('code', '', [], sel); if (sel) this._cellsEl.insertBefore(w, sel); this._selectCell(w); this._markDirty(); e.preventDefault(); break; }
      case 'b': { const w = this._appendCell('code', '', [], sel); this._selectCell(w); this._markDirty(); e.preventDefault(); break; }
      case 'm': if (sel) { this._setCellKind(sel, 'markdown'); e.preventDefault(); } break;
      case 'y': if (sel) { this._setCellKind(sel, 'code'); e.preventDefault(); } break;
    }
  }

  // Toggle a table of contents built from markdown-cell headings.
  _toggleToc() {
    const existing = this._element.querySelector('.nb-toc');
    if (existing) { existing.remove(); return; }
    const toc = document.createElement('div');
    toc.className = 'nb-toc';
    toc.style.cssText = 'padding:8px 12px;border-bottom:1px solid var(--border);background:var(--bg-secondary);font-family:system-ui,sans-serif;font-size:13px;';
    const title = document.createElement('div');
    title.textContent = 'Contents'; title.style.cssText = 'font-weight:bold;margin-bottom:4px;';
    toc.appendChild(title);
    let found = false;
    for (const w of this._cellsEl.querySelectorAll('[data-cell-id]')) {
      if (w.dataset.kind !== 'markdown') continue;
      for (const line of (w.querySelector('textarea').value || '').split('\n')) {
        const m = line.match(/^(#{1,6})\s+(.*)$/);
        if (!m) continue;
        found = true;
        const a = document.createElement('a');
        a.textContent = m[2]; a.href = '#';
        a.style.cssText = 'display:block;padding-left:' + ((m[1].length - 1) * 14) + 'px;color:var(--fg-primary);text-decoration:none;cursor:pointer;';
        a.onclick = (e) => { e.preventDefault(); w.scrollIntoView({ behavior: 'smooth', block: 'start' }); };
        toc.appendChild(a);
      }
    }
    if (!found) {
      const n = document.createElement('div');
      n.textContent = '(no markdown headings)'; n.style.color = 'var(--fg-secondary)';
      toc.appendChild(n);
    }
    this._element.insertBefore(toc, this._cellsEl);
  }

  // Show the rendered markdown, hide the editor (Jupyter-style).
  _renderMarkdown(wrap) {
    const entry = notebookCells.get(wrap.dataset.cellId);
    nbRenderOutputs(entry.outputEl, [{ kind: 'markdown', payload: entry.textarea.value }]);
    entry.textarea.style.display = 'none';
    entry.outputEl.style.borderTop = 'none';
  }

  // Reveal the editor for a markdown cell.
  _editMarkdown(wrap) {
    const entry = notebookCells.get(wrap.dataset.cellId);
    entry.textarea.style.display = '';
    entry.outputEl.style.display = 'none';
    entry.textarea.focus();
  }

  _runCell(wrap) {
    const cellId = wrap.dataset.cellId;
    const kind = wrap.dataset.kind;
    if (kind === 'markdown') { this._renderMarkdown(wrap); return; }
    const entry = notebookCells.get(cellId);
    if (entry && entry.term) {
      // Being edited in the ICL editor: submit it (same path as Shift-Enter).
      ws.send(JSON.stringify({ type: 'cell-key', cellId, data: '\x1b[13;2u' }));
      return;
    }
    // Mark the cell running/queued until its result arrives.
    if (entry && entry.execEl) { entry.execEl.textContent = '[*]'; entry.execEl.style.color = 'var(--accent, #4098ff)'; }
    ws.send(JSON.stringify({ type: 'run-cell', cellId, kind, source: wrap.querySelector('textarea').value }));
  }

  _runAll() {
    for (const w of this._cellsEl.querySelectorAll('[data-cell-id]')) this._runCell(w);
  }

  // Run every cell above the given one (exclusive).
  _runAbove(wrap) {
    const cs = this._cells(), i = cs.indexOf(wrap);
    for (let k = 0; k < i; k++) this._runCell(cs[k]);
  }
  // Run the given cell and every cell below it (inclusive).
  _runBelow(wrap) {
    const cs = this._cells(), i = cs.indexOf(wrap);
    if (i < 0) return;
    for (let k = i; k < cs.length; k++) this._runCell(cs[k]);
  }
  // Restart the backend, then run the selected cell and everything below.
  _restartRunBelow(wrap) {
    this._pendingRunBelowId = wrap && wrap.dataset ? wrap.dataset.cellId : null;
    ws.send(JSON.stringify({ type: 'notebook-restart', runBelow: true }));
  }
  // Selected cell, or the first cell as a sensible default.
  _selectedOrFirst() { return this._selected || this._cells()[0] || null; }

  // ── Find & replace across cells ──
  _ensureFindBar() {
    if (this._findBar) return this._findBar;
    const bar = document.createElement('div');
    bar.style.cssText = 'display:none;gap:6px;align-items:center;padding:6px 8px;' +
      'border-bottom:1px solid var(--border);position:sticky;top:0;background:var(--bg-secondary);z-index:2;';
    const find = document.createElement('input'); find.placeholder = 'Find';
    const repl = document.createElement('input'); repl.placeholder = 'Replace';
    find.style.cssText = repl.style.cssText = 'flex:0 0 180px;padding:2px 6px;';
    const count = document.createElement('span');
    count.style.cssText = 'color:var(--fg-secondary);min-width:70px;font:11px monospace;';
    const refresh = () => {
      this._findRecompute(find.value);
      count.textContent = find.value
        ? (this._findMatches.length ? ((this._findIdx + 1) + '/' + this._findMatches.length + ' cells') : 'no matches')
        : '';
    };
    find.addEventListener('input', () => { this._findIdx = 0; refresh(); });
    find.addEventListener('keydown', (e) => {
      if (e.key === 'Enter') { e.preventDefault(); this._findNext(e.shiftKey ? -1 : 1); refresh(); }
      else if (e.key === 'Escape') { e.preventDefault(); this._closeFind(); }
    });
    bar.appendChild(find);
    bar.appendChild(this._button('◀', () => { this._findNext(-1); refresh(); }, 'Previous match'));
    bar.appendChild(this._button('▶', () => { this._findNext(1); refresh(); }, 'Next match'));
    bar.appendChild(count);
    bar.appendChild(repl);
    bar.appendChild(this._button('Replace all', () => { count.textContent = this._findReplaceAll(find.value, repl.value) + ' replaced'; }, 'Replace all matches across cells'));
    bar.appendChild(this._button('✕', () => this._closeFind(), 'Close'));
    this._findInput = find;
    this._element.insertBefore(bar, this._cellsEl);
    this._findBar = bar; this._findMatches = []; this._findIdx = 0;
    return bar;
  }
  _openFind() { const b = this._ensureFindBar(); b.style.display = 'flex'; this._findInput.focus(); this._findInput.select(); }
  _closeFind() { if (this._findBar) this._findBar.style.display = 'none'; this._element.focus(); }
  _findRecompute(q) {
    this._findMatches = [];
    if (!q) return;
    for (const w of this._cells()) {
      const e = notebookCells.get(w.dataset.cellId);
      if (e && e.textarea && e.textarea.value.includes(q)) this._findMatches.push(w);
    }
    if (this._findIdx >= this._findMatches.length) this._findIdx = 0;
  }
  _findScroll() {
    const w = this._findMatches[this._findIdx];
    if (!w) return;
    w.scrollIntoView({ block: 'center' });
    w.style.outline = '2px solid var(--accent)';
    setTimeout(() => { w.style.outline = ''; }, 700);
  }
  _findNext(dir) {
    if (!this._findMatches.length) this._findRecompute(this._findInput.value);
    if (!this._findMatches.length) return;
    this._findIdx = (this._findIdx + dir + this._findMatches.length) % this._findMatches.length;
    this._findScroll();
  }
  _findReplaceAll(q, r) {
    if (!q) return 0;
    let n = 0;
    for (const w of this._cells()) {
      const e = notebookCells.get(w.dataset.cellId);
      if (!e || !e.textarea || !e.textarea.value.includes(q)) continue;
      const parts = e.textarea.value.split(q);
      n += parts.length - 1;
      e.textarea.value = parts.join(r);
      // If the ICL editor is open on this cell it has the stale text; close it
      // so the (now-updated) resting source shows.
      if (e.term) { try { e.term.dispose(); } catch (x) {} e.term = null; }
      if (e.termDiv) { e.termDiv.remove(); e.termDiv = null; }
      e.textarea.style.display = (e.tags || []).includes('hide-input') ? 'none' : '';
      if (w.dataset.kind === 'markdown') this._renderMarkdown(w);
    }
    if (n) this._markDirty();
    this._findRecompute(q);
    return n;
  }

  _save() {
    const path = window.prompt('Save notebook to path:', this._path || 'notebook.iclnb');
    if (!path) return;
    this._path = path;
    // Title the notebook after the file's base name.
    this._title = path.replace(/^.*\//, '').replace(/\.iclnb$/i, '') || 'Untitled';
    const panel = dockviewApi && dockviewApi.getPanel('notebook');
    if (panel && panel.api && panel.api.setTitle) panel.api.setTitle('Notebook: ' + this._title);
    this._writeNotebook(path);
  }

  // Export to a loadable jupytext-style .lisp (server writes the file).
  _exportLisp() {
    const def = (this._path || 'notebook').replace(/\.iclnb$/i, '') + '.lisp';
    const path = window.prompt('Export to .lisp path:', def);
    if (!path) return;
    const cells = [];
    for (const w of this._cellsEl.querySelectorAll('[data-cell-id]'))
      cells.push({ kind: w.dataset.kind, source: w.querySelector('textarea').value });
    ws.send(JSON.stringify({ type: 'export-notebook', path, format: 'lisp', cells }));
  }

  // Export to a self-contained HTML file (downloaded), capturing the
  // rendered outputs currently in the DOM.
  _exportHtml() {
    const t = this._title || 'Notebook';
    const parts = ['<!doctype html><html><head><meta charset="utf-8"><title>' + t + '</title>',
      '<style>body{font-family:system-ui,sans-serif;max-width:900px;margin:2em auto;padding:0 1em;line-height:1.5;}',
      'pre{background:#f5f5f5;padding:8px;border-radius:4px;overflow:auto;}',
      'table{border-collapse:collapse;}td,th{border:1px solid #ccc;padding:3px 8px;}',
      '.cell{margin:1.2em 0;}.out{margin-top:.4em;}</style></head><body>',
      '<h1>' + nbEscapeHtml(t) + '</h1>'];
    for (const w of this._cellsEl.querySelectorAll('[data-cell-id]')) {
      const e = notebookCells.get(w.dataset.cellId);
      if (w.dataset.kind === 'markdown') {
        parts.push('<div class="cell">' + nbRenderMarkdown(e.textarea.value) + '</div>');
      } else {
        parts.push('<div class="cell"><pre><code>' + nbEscapeHtml(e.textarea.value) + '</code></pre>');
        if (e.outputEl && e.outputEl.firstChild) parts.push('<div class="out">' + e.outputEl.innerHTML + '</div>');
        parts.push('</div>');
      }
    }
    parts.push('</body></html>');
    const blob = new Blob([parts.join('\n')], { type: 'text/html' });
    const a = document.createElement('a');
    a.href = URL.createObjectURL(blob);
    a.download = t + '.html';
    a.click();
    setTimeout(() => URL.revokeObjectURL(a.href), 2000);
  }

  // Export a reveal.js slide deck. Slide breaks come from cell tags:
  //   slide/subslide -> new slide, fragment -> incremental reveal within a
  //   slide, notes -> speaker notes, skip -> omit. An untagged notebook makes
  //   one slide per cell.
  _exportSlides() {
    const t = this._title || 'Notebook';
    const cells = Array.from(this._cellsEl.querySelectorAll('[data-cell-id]')).map(w => {
      const e = notebookCells.get(w.dataset.cellId);
      return { kind: w.dataset.kind, e, tags: (e.tags || []) };
    });
    const SLIDE_TAGS = ['slide', 'subslide', 'fragment', 'notes', 'skip'];
    const anyTagged = cells.some(c => c.tags.some(x => SLIDE_TAGS.includes(x)));
    // Slides are output-forward (a presentation, not a code listing): show the
    // cell's output first; include the code only when a cell opts in with a
    // "show-input" tag, or when it has no output (and isn't hide-input).
    const cellHtml = (c) => {
      if (c.kind === 'markdown') return nbRenderMarkdown(c.e.textarea.value);
      const out = (c.e.outputEl && c.e.outputEl.firstChild)
        ? '<div class="nb-out">' + c.e.outputEl.innerHTML + '</div>' : '';
      const showCode = c.tags.includes('show-input') ||
        (!out && !c.tags.includes('hide-input'));
      const code = showCode
        ? '<pre><code>' + nbEscapeHtml(c.e.textarea.value) + '</code></pre>' : '';
      return out + code;
    };
    const slides = []; let cur = null;
    const start = (html) => { cur = { body: [html], notes: [] }; slides.push(cur); };
    for (const c of cells) {
      const st = SLIDE_TAGS.find(x => c.tags.includes(x)) || (anyTagged ? null : 'slide');
      if (st === 'skip') continue;
      if (st === 'notes') { if (!cur) start(''); cur.notes.push(cellHtml(c)); continue; }
      if (st === 'slide' || st === 'subslide' || !cur) { start(cellHtml(c)); continue; }
      if (st === 'fragment') { cur.body.push('<div class="fragment">' + cellHtml(c) + '</div>'); continue; }
      cur.body.push(cellHtml(c));
    }
    const sections = slides.map(s =>
      '<section>' + s.body.join('\n') +
      (s.notes.length ? '<aside class="notes">' + s.notes.join('\n') + '</aside>' : '') +
      '</section>').join('\n');
    const cdn = 'https://cdn.jsdelivr.net/npm/reveal.js@5/dist';
    const html = '<!doctype html><html><head><meta charset="utf-8"><title>' + nbEscapeHtml(t) + '</title>' +
      '<link rel="stylesheet" href="' + cdn + '/reveal.css">' +
      '<link rel="stylesheet" href="' + cdn + '/theme/white.css">' +
      '<style>.reveal pre{width:100%;font-size:0.6em;}.reveal section img{max-height:60vh;}' +
      '.reveal table{font-size:0.6em;}</style></head>' +
      '<body><div class="reveal"><div class="slides">' + sections + '</div></div>' +
      '<script src="' + cdn + '/reveal.js"></script>' +
      '<script>Reveal.initialize({hash:true});</script></body></html>';
    nbDownload(t + '.slides.html', html, 'text/html');
  }
}
