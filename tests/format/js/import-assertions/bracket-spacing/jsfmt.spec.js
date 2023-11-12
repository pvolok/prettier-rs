run_spec(import.meta, ["babel"], {
  bracketSpacing: false,
  errors: {
    acorn: [
      "dynamic-import.js",
      "static-import.js",
      "re-export.js",
      "empty.js",
    ],
    espree: [
      "dynamic-import.js",
      "static-import.js",
      "re-export.js",
      "empty.js",
    ],
    meriyah: [
      "dynamic-import.js",
      "static-import.js",
      "re-export.js",
      "empty.js",
    ],
  },
});
