// Monaco editor initialization for coverage panel
// Runs inside iframe, isolated from main window's AMD loader

let editor = null;
let decorations = [];
let hoverRequestId = 0;
const pendingHoverRequests = new Map();

require.config({ paths: { 'vs': '/assets/monaco/vs' }});

require(['vs/editor/editor.main'], function() {
  // Register Common Lisp language
  monaco.languages.register({ id: 'lisp' });

  // Configure word pattern for Lisp symbols (includes -, *, +, /, etc.)
  monaco.languages.setLanguageConfiguration('lisp', {
    wordPattern: /[a-zA-Z_\-\+\*\/\?\!\=\<\>\&\%\$\@\#\^\.\:][a-zA-Z0-9_\-\+\*\/\?\!\=\<\>\&\%\$\@\#\^\.\:]*|[0-9]+/g,
    brackets: [['(', ')'], ['[', ']']],
    autoClosingPairs: [
      { open: '(', close: ')' },
      { open: '[', close: ']' },
      { open: '"', close: '"' }
    ],
    comments: { lineComment: ';' }
  });

  // Basic Lisp syntax highlighting
  monaco.languages.setMonarchTokensProvider('lisp', {
    defaultToken: '',
    keywords: [
      'defun', 'defmacro', 'defvar', 'defparameter', 'defconstant', 'defclass',
      'defmethod', 'defgeneric', 'defstruct', 'defpackage', 'deftype',
      'lambda', 'let', 'let*', 'flet', 'labels', 'macrolet', 'symbol-macrolet',
      'if', 'when', 'unless', 'cond', 'case', 'ecase', 'typecase', 'etypecase',
      'progn', 'prog1', 'prog2', 'block', 'return', 'return-from',
      'loop', 'do', 'do*', 'dolist', 'dotimes', 'tagbody', 'go',
      'catch', 'throw', 'unwind-protect', 'handler-case', 'handler-bind',
      'restart-case', 'restart-bind', 'with-simple-restart',
      'multiple-value-bind', 'multiple-value-call', 'multiple-value-prog1',
      'setq', 'setf', 'psetq', 'psetf', 'incf', 'decf', 'push', 'pop',
      'and', 'or', 'not', 'the', 'declare', 'locally', 'eval-when',
      'quote', 'function', 'in-package', 'use-package', 'require', 'provide',
      'nil', 't'
    ],
    operators: ['eq', 'eql', 'equal', 'equalp', 'null', 'atom', 'listp', 'consp',
                'car', 'cdr', 'cons', 'list', 'append', 'apply', 'funcall',
                'mapcar', 'mapc', 'mapcan', 'format', 'print', 'princ', 'prin1'],
    tokenizer: {
      root: [
        [/;.*$/, 'comment'],
        [/#\|/, 'comment', '@blockComment'],
        [/"(?:[^"\\]|\\.)*"/, 'string'],
        [/#\\[a-zA-Z]+|#\\./, 'string.char'],
        [/'[a-zA-Z_\-\+\*\/\?\!\=\<\>\&\%\$\@\#\^\.\:][a-zA-Z0-9_\-\+\*\/\?\!\=\<\>\&\%\$\@\#\^\.\:]*/, 'variable.quoted'],
        [/:[a-zA-Z_\-\+\*\/\?\!\=\<\>\&\%\$\@\#\^\.\:][a-zA-Z0-9_\-\+\*\/\?\!\=\<\>\&\%\$\@\#\^\.\:]*/, 'constant'],
        [/&[a-zA-Z_\-\+\*\/\?\!\=\<\>\&\%\$\@\#\^\.\:][a-zA-Z0-9_\-\+\*\/\?\!\=\<\>\&\%\$\@\#\^\.\:]*/, 'variable.parameter'],
        [/\*[a-zA-Z_\-\+\*\/\?\!\=\<\>\&\%\$\@\#\^\.\:][a-zA-Z0-9_\-\+\*\/\?\!\=\<\>\&\%\$\@\#\^\.\:]*\*/, 'variable.special'],
        [/[a-zA-Z_\-\+\*\/\?\!\=\<\>\&\%\$\@\#\^\.\:][a-zA-Z0-9_\-\+\*\/\?\!\=\<\>\&\%\$\@\#\^\.\:]*/, {
          cases: {
            '@keywords': 'keyword',
            '@operators': 'predefined',
            '@default': 'identifier'
          }
        }],
        [/[0-9]+(\.[0-9]+)?([eE][\-+]?[0-9]+)?/, 'number'],
        [/#[xXoObB][0-9a-fA-F]+/, 'number.hex'],
        [/[()\[\]]/, 'delimiter.parenthesis'],
        [/`|,@|,|'/, 'delimiter.quote']
      ],
      blockComment: [
        [/[^|#]+/, 'comment'],
        [/\|#/, 'comment', '@pop'],
        [/[|#]/, 'comment']
      ]
    }
  });

  // Notify parent that Monaco is ready
  window.parent.postMessage({ type: 'monaco-ready' }, '*');

  // Listen for commands from parent
  window.addEventListener('message', function(e) {
    if (!e.data) return;

    if (e.data.type === 'set-content') {
      const isDark = e.data.theme === 'dark';
      if (!editor) {
        editor = monaco.editor.create(document.getElementById('container'), {
          value: e.data.content,
          language: 'lisp',
          theme: isDark ? 'vs-dark' : 'vs',
          readOnly: true,
          minimap: { enabled: true },
          scrollBeyondLastLine: false,
          lineNumbers: 'on',
          glyphMargin: true,
          folding: true,
          renderLineHighlight: 'none',
          automaticLayout: true,
          fontSize: 13,
          fontFamily: "'SF Mono', Monaco, 'Cascadia Code', Consolas, monospace"
        });

        // Add context menu action to send top-level form to REPL
        editor.addAction({
          id: 'copy-toplevel-to-repl',
          label: 'Send Top-Level Form to REPL',
          keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter],
          contextMenuGroupId: 'navigation',
          contextMenuOrder: 1.5,
          run: function(ed) {
            const position = ed.getPosition();
            const model = ed.getModel();
            const toplevelForm = findEnclosingToplevelForm(model, position);
            if (toplevelForm) {
              window.parent.postMessage({
                type: 'copy-to-repl',
                code: toplevelForm
              }, '*');
            }
          }
        });

        // Add Find action to context menu (Ctrl+F may be intercepted by browser)
        editor.addAction({
          id: 'find-in-file',
          label: 'Find...',
          keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyF],
          contextMenuGroupId: 'navigation',
          contextMenuOrder: 1.6,
          run: function(ed) {
            ed.getAction('actions.find').run();
          }
        });

        // Remove default clipboard actions from context menu
        editor.addAction({
          id: 'editor.action.clipboardCopyAction',
          label: '',
          contextMenuGroupId: null,
          run: function() {}
        });

        // Handle clicks on symbols - update Packages/Symbols/Symbol Info panels
        editor.onMouseUp(function(e) {
          if (e.target.position) {
            const word = getSymbolAtPosition(editor.getModel(), e.target.position);
            if (word) {
              window.parent.postMessage({
                type: 'symbol-click',
                symbol: word.word
              }, '*');
            }
          }
        });
      } else {
        editor.setValue(e.data.content);
        monaco.editor.setTheme(isDark ? 'vs-dark' : 'vs');
      }

      // Apply coverage decorations with precise column positions
      if (e.data.annotations) {
        applyCoverageDecorations(e.data.annotations);
      }

      // Jump to specific line if provided (for ,source command)
      if (e.data.line && editor) {
        const lineNum = parseInt(e.data.line, 10);
        if (lineNum > 0) {
          editor.revealLineInCenter(lineNum);
          editor.setPosition({ lineNumber: lineNum, column: 1 });
          // Highlight the line
          decorations = editor.deltaDecorations(decorations, [{
            range: new monaco.Range(lineNum, 1, lineNum, 1),
            options: {
              isWholeLine: true,
              className: 'line-highlight'
            }
          }]);
        }
      }
    } else if (e.data.type === 'set-theme') {
      // Theme change from parent window
      if (editor) {
        const isDark = e.data.theme === 'dark';
        monaco.editor.setTheme(isDark ? 'vs-dark' : 'vs');
      }
    } else if (e.data.type === 'hover-response') {
      // Documentation response from parent
      const resolver = pendingHoverRequests.get(e.data.requestId);
      if (resolver) {
        pendingHoverRequests.delete(e.data.requestId);
        resolver(e.data.documentation);
      }
    }
  });

  // Register hover provider for Lisp symbols
  monaco.languages.registerHoverProvider('lisp', {
    provideHover: function(model, position) {
      const word = getSymbolAtPosition(model, position);
      if (!word) return null;

      return new Promise((resolve) => {
        const requestId = ++hoverRequestId;

        // Set up timeout to avoid hanging
        const timeout = setTimeout(() => {
          pendingHoverRequests.delete(requestId);
          resolve(null);
        }, 3000);

        pendingHoverRequests.set(requestId, (doc) => {
          clearTimeout(timeout);
          if (doc) {
            resolve({
              contents: [{ value: '```lisp\n' + doc + '\n```' }]
            });
          } else {
            resolve(null);
          }
        });

        // Request documentation from parent
        window.parent.postMessage({
          type: 'get-hover-doc',
          requestId: requestId,
          symbol: word.word
        }, '*');
      });
    }
  });
});

function applyCoverageDecorations(annotations) {
  if (!editor) return;

  const newDecorations = [];
  const glyphLines = {};  // Track which lines need glyph markers

  // Group branch annotations by position to determine combined state
  const branchGroups = {};
  const exprAnnotations = [];

  annotations.forEach(ann => {
    const state = ann.state;
    if (state === 'then-taken' || state === 'then-not-taken' ||
        state === 'else-taken' || state === 'else-not-taken') {
      // Branch annotation - group by position
      const key = `${ann.startLine}:${ann.startCol}:${ann.endLine}:${ann.endCol}`;
      if (!branchGroups[key]) {
        branchGroups[key] = { ann: ann, thenTaken: null, elseTaken: null };
      }
      if (state === 'then-taken') branchGroups[key].thenTaken = true;
      else if (state === 'then-not-taken') branchGroups[key].thenTaken = false;
      else if (state === 'else-taken') branchGroups[key].elseTaken = true;
      else if (state === 'else-not-taken') branchGroups[key].elseTaken = false;
    } else {
      // Expression annotation
      exprAnnotations.push(ann);
    }
  });

  // Process expression annotations
  exprAnnotations.forEach(ann => {
    let className;
    if (ann.state === 'executed') {
      className = 'coverage-executed';
    } else if (ann.state === 'not-executed') {
      className = 'coverage-not-executed';
    } else {
      return;
    }
    addDecoration(ann, className);
  });

  // Process branch annotations with combined state
  Object.values(branchGroups).forEach(group => {
    const { ann, thenTaken, elseTaken } = group;
    let className;

    if (thenTaken === true && elseTaken === true) {
      // Both branches taken - fully covered
      className = 'coverage-executed';
    } else if (thenTaken === false && elseTaken === false) {
      // Neither branch taken - condition never evaluated
      className = 'coverage-neither-branch';
    } else if ((thenTaken === true && elseTaken === false) ||
               (thenTaken === false && elseTaken === true)) {
      // One branch taken - partial coverage
      className = 'coverage-partial';
    } else {
      // Only have one branch info (shouldn't happen normally)
      className = (thenTaken === true || elseTaken === true)
        ? 'coverage-executed' : 'coverage-not-executed';
    }
    addDecoration(ann, className);
  });

  function addDecoration(ann, className) {
    // Track glyph state for each line
    for (let line = ann.startLine; line <= ann.endLine; line++) {
      if (!glyphLines[line]) {
        glyphLines[line] = { executed: false, notExecuted: false, partial: false };
      }
      if (className === 'coverage-executed') {
        glyphLines[line].executed = true;
      } else if (className === 'coverage-partial' || className === 'coverage-neither-branch') {
        glyphLines[line].partial = true;
      } else {
        glyphLines[line].notExecuted = true;
      }
    }

    // Create inline decoration with exact column positions
    newDecorations.push({
      range: new monaco.Range(ann.startLine, ann.startCol, ann.endLine, ann.endCol),
      options: {
        inlineClassName: className + '-inline'
      }
    });
  }

  // Add glyph margin decorations for lines (showing overall line status)
  Object.keys(glyphLines).forEach(lineNum => {
    const lineState = glyphLines[lineNum];
    let glyphClassName;

    // Priority: partial/neither > mixed executed+not > not-executed > executed
    if (lineState.partial) {
      glyphClassName = 'coverage-glyph-partial';
    } else if (lineState.executed && lineState.notExecuted) {
      glyphClassName = 'coverage-glyph-partial';
    } else if (lineState.notExecuted) {
      glyphClassName = 'coverage-glyph-not-executed';
    } else if (lineState.executed) {
      glyphClassName = 'coverage-glyph-executed';
    }

    if (glyphClassName) {
      newDecorations.push({
        range: new monaco.Range(parseInt(lineNum), 1, parseInt(lineNum), 1),
        options: {
          glyphMarginClassName: glyphClassName
        }
      });
    }
  });

  decorations = editor.deltaDecorations(decorations, newDecorations);
}

// Find the enclosing top-level form (starts at column 1 with open paren)
function findEnclosingToplevelForm(model, position) {
  const lineCount = model.getLineCount();
  let startLine = null;

  // Search backwards for a line starting with '(' at column 1
  for (let line = position.lineNumber; line >= 1; line--) {
    const lineContent = model.getLineContent(line);
    if (lineContent.length > 0 && lineContent[0] === '(') {
      startLine = line;
      break;
    }
  }

  if (startLine === null) return null;

  // Now find the matching close paren
  let depth = 0;
  let endLine = startLine;
  let endCol = 1;
  let inString = false;
  let escape = false;

  for (let line = startLine; line <= lineCount; line++) {
    const lineContent = model.getLineContent(line);
    for (let col = (line === startLine ? 0 : 0); col < lineContent.length; col++) {
      const ch = lineContent[col];

      if (escape) {
        escape = false;
        continue;
      }

      if (ch === '\\') {
        escape = true;
        continue;
      }

      if (ch === '"') {
        inString = !inString;
        continue;
      }

      if (inString) continue;

      // Skip line comments
      if (ch === ';') break;

      if (ch === '(') {
        depth++;
      } else if (ch === ')') {
        depth--;
        if (depth === 0) {
          endLine = line;
          endCol = col + 2;  // 1-based, after the paren
          // Extract the form
          const range = new monaco.Range(startLine, 1, endLine, endCol);
          return model.getValueInRange(range);
        }
      }
    }
  }

  return null;  // Unbalanced parens
}

// Extract Lisp symbol at position (handles special chars like -, *, +, etc.)
function getSymbolAtPosition(model, position) {
  const line = model.getLineContent(position.lineNumber);
  const col = position.column - 1;  // 0-based

  if (col >= line.length) return null;

  // Lisp symbol characters (excluding whitespace, parens, quotes, etc.)
  const isSymbolChar = (ch) => {
    if (!ch) return false;
    // Not whitespace, parens, quotes, semicolon, comma, backtick
    return !/[\s()\[\]{}"';,`]/.test(ch);
  };

  // Check if we're on a symbol character
  if (!isSymbolChar(line[col])) return null;

  // Find start of symbol
  let start = col;
  while (start > 0 && isSymbolChar(line[start - 1])) {
    start--;
  }

  // Find end of symbol
  let end = col;
  while (end < line.length - 1 && isSymbolChar(line[end + 1])) {
    end++;
  }

  const word = line.substring(start, end + 1);

  // Skip if it looks like a number
  if (/^-?\d+\.?\d*$/.test(word)) return null;

  // Skip very short symbols (likely noise)
  if (word.length < 2) return null;

  return {
    word: word,
    startColumn: start + 1,
    endColumn: end + 2
  };
}
