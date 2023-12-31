"use strict";

function createPlugin({ name, print, finalNewLine = true }) {
  return {
    languages: [
      {
        name,
        parsers: [name],
        extensions: [`.${name}`],
      },
    ],
    parsers: {
      [name]: {
        parse: (text) => ({ value: text }),
        astFormat: name,
      },
    },
    printers: {
      [name]: {
        print(path, options) {
          return (
            print(path.getValue().value, options) + (finalNewLine ? "\n" : "")
          );
        },
      },
    },
  };
}

module.exports = createPlugin;
