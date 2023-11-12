run_spec(import.meta, ["babel", "typescript"], {
  errors: {
    acorn: [
      "dynamic-import.js",
      "empty.js",
      "multi-types.js",
      "static-import.js",
      "re-export.js",
      "without-from.js",
      "non-type.js",
    ],
    espree: [
      "dynamic-import.js",
      "empty.js",
      "multi-types.js",
      "static-import.js",
      "re-export.js",
      "without-from.js",
      "non-type.js",
    ],
    meriyah: [
      "dynamic-import.js",
      "empty.js",
      "multi-types.js",
      "static-import.js",
      "re-export.js",
      "without-from.js",
      "non-type.js",
    ],
    typescript: [
      "empty.js",
      "multi-types.js",
      "static-import.js",
      "re-export.js",
      "without-from.js",
      "non-type.js",
    ],
  },
});
