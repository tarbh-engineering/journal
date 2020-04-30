const { resolve } = require("path");

const publicFolder = resolve("./test");

module.exports = {
  mode: "development",
  entry: "./test/index.js",
  devServer: {
    contentBase: publicFolder,
    port: 1234,
  },
  output: {
    path: publicFolder,
    filename: "bundle.js",
  },
};
