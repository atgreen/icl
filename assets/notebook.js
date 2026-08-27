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
  if (msg.submitted && nbPanel) {
    nbPanel._runCell(e.wrap);
    const action = e.postRun; e.postRun = null;
    if (action === 'advance') nbPanel._advanceFrom(e.wrap);
    else if (action === 'insert') nbPanel._insertAndEditAfter(e.wrap);
    // 'inplace' (or none): stay put
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
      box.style.left = (si.start * cw) + 'px'; box.style.top = (row * ch) + 'px';
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
}

// ── Minimal markdown → HTML (headings, emphasis, code, lists, links) ──

function nbEscapeHtml(s) {
  return s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
}

function nbMarkdownInline(t) {
  return nbEscapeHtml(t)
    .replace(/`([^`]+)`/g, '<code style="background:var(--bg-tertiary);padding:1px 4px;border-radius:3px;">$1</code>')
    .replace(/\*\*([^*]+)\*\*/g, '<strong>$1</strong>')
    .replace(/(^|[^*])\*([^*]+)\*/g, '$1<em>$2</em>')
    .replace(/\[([^\]]+)\]\(([^)]+)\)/g, '<a href="$2" target="_blank" rel="noopener">$1</a>');
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
  let html = '', inList = false, inCode = false, code = '';
  const closeList = () => { if (inList) { html += '</ul>'; inList = false; } };
  const cell = (tag, text) =>
    '<' + tag + ' style="border:1px solid var(--border);padding:3px 8px;' +
    (tag === 'th' ? 'background:var(--bg-tertiary);text-align:left;' : '') + '">' +
    nbMarkdownInline(text) + '</' + tag + '>';
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    if (line.trim().startsWith('```')) {
      if (inCode) {
        html += '<pre style="background:var(--bg-tertiary);padding:8px;border-radius:4px;overflow:auto;"><code>' + nbEscapeHtml(code) + '</code></pre>';
        code = ''; inCode = false;
      } else { closeList(); inCode = true; }
      continue;
    }
    if (inCode) { code += line + '\n'; continue; }
    // GFM table: a header row followed by a separator row.
    if (line.includes('|') && nbIsTableSep(lines[i + 1])) {
      closeList();
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
    if (h) { closeList(); const n = h[1].length; html += '<h' + n + '>' + nbMarkdownInline(h[2]) + '</h' + n + '>'; continue; }
    if (/^\s*[-*]\s+/.test(line)) { if (!inList) { html += '<ul>'; inList = true; } html += '<li>' + nbMarkdownInline(line.replace(/^\s*[-*]\s+/, '')) + '</li>'; continue; }
    if (/^\s*---+\s*$/.test(line)) { closeList(); html += '<hr>'; continue; }
    if (line.trim() === '') { closeList(); continue; }
    closeList();
    html += '<p>' + nbMarkdownInline(line) + '</p>';
  }
  closeList();
  if (inCode) html += '<pre><code>' + nbEscapeHtml(code) + '</code></pre>';
  return html;
}

// ── Output rendering (blob kinds mirror the server taxonomy) ──

const NB_RICH_KINDS = new Set(['hash-table', 'vega-lite', 'mermaid', 'json', 'svg', 'html', 'image']);

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
      case 'mermaid': renderMermaid(div, o.payload); break;
      case 'json': renderJson(div, o.payload); break;
      case 'svg': div.innerHTML = o.payload; break;
      case 'html': {
        const f = document.createElement('iframe');
        f.style.cssText = 'width:100%;height:300px;border:none;background:white;';
        f.sandbox = 'allow-same-origin'; f.srcdoc = o.payload;
        div.appendChild(f); break;
      }
      case 'image': renderImage(div, o.payload); break;
    }
  } catch (e) { div.textContent = 'render error: ' + e; }
  outEl.appendChild(div);
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
    let cells = p.cells && p.cells.length ? p.cells : [{ kind: 'code', source: '', outputs: [] }];
    this._build(cells);
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

  _button(label, fn) {
    const b = document.createElement('button');
    b.type = 'button';
    b.textContent = label;
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
    bar.appendChild(this._button('+ Code', () => this._addCell('code')));
    bar.appendChild(this._button('+ Markdown', () => this._addCell('markdown')));
    bar.appendChild(this._button('Run all', () => this._runAll()));
    bar.appendChild(this._button('⏹ Interrupt', () => ws.send(JSON.stringify({ type: 'notebook-interrupt' }))));
    bar.appendChild(this._button('⟳ Restart', () => ws.send(JSON.stringify({ type: 'notebook-restart' }))));
    bar.appendChild(this._button('⟳▶ Run all', () => ws.send(JSON.stringify({ type: 'notebook-restart', runAll: true }))));
    bar.appendChild(this._button('Clear outputs', () => this._clearAllOutputs()));
    bar.appendChild(this._button('Save', () => this._save()));
    this._element.appendChild(bar);

    this._cellsEl = document.createElement('div');
    this._cellsEl.style.cssText = 'flex:1;padding:8px;';
    this._element.appendChild(this._cellsEl);

    for (const c of cells) this._appendCell(c.kind || 'code', c.source || '', c.outputs || []);
  }

  _appendCell(kind, source, outputs, afterEl) {
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
    head.appendChild(exec);
    head.appendChild(kindLabel);
    head.appendChild(this._button('Run', () => this._runCell(wrap)));
    head.appendChild(this._button('▾', (function () {
      const o = notebookCells.get(cellId).outputEl;
      o.dataset.collapsed = o.dataset.collapsed === '1' ? '' : '1';
      o.style.display = (o.dataset.collapsed === '1' || !o.firstChild) ? 'none' : 'block';
    })));
    head.appendChild(this._button('∅', () => this._clearCellOutput(wrap)));
    head.appendChild(this._button('✕', () => this._removeCell(wrap)));

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
    notebookCells.set(cellId, { outputEl: out, execEl: exec, textarea: ta, wrap });

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
  }

  _removeCell(wrap) {
    notebookCells.delete(wrap.dataset.cellId);
    wrap.remove();
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

  _save() {
    const path = window.prompt('Save notebook to path:', this._path || 'notebook.iclnb');
    if (!path) return;
    this._path = path;
    // Title the notebook after the file's base name.
    this._title = path.replace(/^.*\//, '').replace(/\.iclnb$/i, '') || 'Untitled';
    const panel = dockviewApi && dockviewApi.getPanel('notebook');
    if (panel && panel.api && panel.api.setTitle) panel.api.setTitle('Notebook: ' + this._title);
    const cells = [];
    for (const w of this._cellsEl.querySelectorAll('[data-cell-id]')) {
      cells.push({ kind: w.dataset.kind, source: w.querySelector('textarea').value });
    }
    ws.send(JSON.stringify({ type: 'save-notebook', path, title: this._title, cells }));
  }
}
