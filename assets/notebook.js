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

// cellId -> { outputEl, execEl } for routing cell-result messages.
const notebookCells = new Map();
let notebookCellSeq = 0;
function nbNewCellId() { return 'nbcell-' + (++notebookCellSeq); }

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
  if (entry.execEl) entry.execEl.textContent = msg.execCount ? ('[' + msg.execCount + ']') : '';
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
      case 'vega-lite': div.style.minHeight = '260px'; renderVegaLite(div, o.payload); break;
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
  const rich = outputs.find(o => NB_RICH_KINDS.has(o.kind));
  for (const o of outputs) {
    if (o === rich) { nbRenderRich(outEl, o); }
    else if (rich && o.kind === 'value') { nbRenderValueToggle(outEl, o.payload); }
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
    const p = params.params || {};
    this._path = p.path || null;
    this._title = p.title || 'Untitled';
    let cells = p.cells && p.cells.length ? p.cells : [{ kind: 'code', source: '', outputs: [] }];
    this._build(cells);
  }

  _button(label, fn) {
    const b = document.createElement('button');
    b.textContent = label;
    b.style.cssText =
      'font:12px sans-serif;padding:3px 8px;cursor:pointer;border:1px solid var(--border);' +
      'border-radius:4px;background:var(--bg-tertiary);color:var(--fg-primary);';
    b.onclick = fn;
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
      'font:11px monospace;color:var(--fg-secondary);';
    const exec = document.createElement('span');
    exec.style.cssText = 'min-width:34px;';
    exec.textContent = '';
    const kindLabel = document.createElement('span');
    kindLabel.textContent = kind;
    kindLabel.style.cssText = 'flex:1;';
    head.appendChild(exec);
    head.appendChild(kindLabel);
    head.appendChild(this._button('Run', () => this._runCell(wrap)));
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

    // output
    const out = document.createElement('div');
    out.style.cssText = 'border-top:1px solid var(--border);display:none;';
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
    const ta = w.querySelector('textarea');
    if (ta) ta.focus();
  }

  _removeCell(wrap) {
    notebookCells.delete(wrap.dataset.cellId);
    wrap.remove();
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
    const source = wrap.querySelector('textarea').value;
    if (kind === 'markdown') { this._renderMarkdown(wrap); return; }
    ws.send(JSON.stringify({ type: 'run-cell', cellId, kind, source }));
  }

  _runAll() {
    for (const w of this._cellsEl.querySelectorAll('[data-cell-id]')) this._runCell(w);
  }

  _save() {
    const path = window.prompt('Save notebook to path:', this._path || 'notebook.iclnb');
    if (!path) return;
    this._path = path;
    const cells = [];
    for (const w of this._cellsEl.querySelectorAll('[data-cell-id]')) {
      cells.push({ kind: w.dataset.kind, source: w.querySelector('textarea').value });
    }
    ws.send(JSON.stringify({ type: 'save-notebook', path, title: this._title, cells }));
  }
}
