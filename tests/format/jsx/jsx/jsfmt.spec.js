run_spec(import.meta, ["flow", "babel", "typescript"], {
  singleQuote: false,
  jsxSingleQuote: false,
});
run_spec(import.meta, ["flow", "babel", "typescript"], {
  singleQuote: false,
  jsxSingleQuote: true,
});
run_spec(import.meta, ["flow", "babel", "typescript"], {
  singleQuote: true,
  jsxSingleQuote: false,
});
run_spec(import.meta, ["flow", "babel", "typescript"], {
  singleQuote: true,
  jsxSingleQuote: true,
});
