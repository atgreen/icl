// Monaco editor initialization script
// This runs in an isolated iframe to avoid AMD loader conflicts with regulex.js
require.config({ paths: { 'vs': '/assets/monaco/vs' }});
require(['vs/editor/editor.main'], function() {
  window.parent.postMessage({ type: 'monaco-loaded' }, '*');
});
