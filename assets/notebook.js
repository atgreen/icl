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
  dockviewApi.addPanel({
    id: panelId,
    component: 'notebook',
    title: msg.title ? ('Notebook: ' + msg.title) : 'Notebook',
    params: { title: msg.title, path: msg.path, cells: msg.cells || [] },
    position: { referencePanel: 'terminal', direction: 'right' }
  });
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

// ── Output rendering (blob kinds mirror the server taxonomy) ──

function nbRenderOutputs(outEl, outputs) {
  outEl.innerHTML = '';
  if (!outputs || outputs.length === 0) { outEl.style.display = 'none'; return; }
  outEl.style.display = 'block';
  for (const o of outputs) {
    const div = document.createElement('div');
    div.className = 'nb-out nb-out-' + o.kind;
    const base = 'white-space:pre-wrap;font-family:monospace;font-size:13px;padding:4px 8px;';
    switch (o.kind) {
      case 'value':
        div.style.cssText = base + 'color:var(--fg-primary);';
        div.textContent = o.payload;
        break;
      case 'stdout':
        div.style.cssText = base + 'color:var(--fg-secondary);';
        div.textContent = o.payload;
        break;
      case 'error':
        div.style.cssText = base + 'color:#e06c75;';
        div.textContent = o.payload;
        break;
      case 'markdown':
        div.style.cssText = 'padding:4px 8px;color:var(--fg-primary);white-space:pre-wrap;';
        div.textContent = o.payload;  // rich markdown (tuition) comes later
        break;
      default:
        div.style.cssText = base + 'color:var(--fg-secondary);';
        div.textContent = o.payload;
    }
    outEl.appendChild(div);
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

  _runCell(wrap) {
    const cellId = wrap.dataset.cellId;
    const kind = wrap.dataset.kind;
    const source = wrap.querySelector('textarea').value;
    const entry = notebookCells.get(cellId);
    if (kind === 'markdown') {
      // Rendered locally for now; rich markdown via the backend comes later.
      nbRenderOutputs(entry.outputEl, [{ kind: 'markdown', payload: source }]);
      return;
    }
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
