// Monaco initialization - must run BEFORE regulex.js loads
// Configure Monaco and start loading the editor module immediately
// before regulex.js overwrites AMD globals with its bundled almond.js
if (typeof require !== 'undefined' && require.config) {
  require.config({ paths: { 'vs': '/assets/monaco/vs' }});
  // Start loading Monaco editor immediately - this registers the pending load
  // with Monaco's require before regulex overwrites it
  window._monacoLoadPromise = new Promise(function(resolve) {
    require(['vs/editor/editor.main'], function() {
      window._monacoLoaded = true;
      resolve();
    });
  });
}
