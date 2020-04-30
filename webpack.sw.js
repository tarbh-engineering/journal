const { resolve } = require("path");

const publicFolder = resolve("./public");

module.exports = {
  mode: "production",
  target: "webworker",
  entry: "./src/sw.js",
  output: {
    path: publicFolder,
    filename: "sw.js",
  },
};
