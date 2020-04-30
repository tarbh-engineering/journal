const { resolve } = require("path");
const webpack = require("webpack");

const { ENV, HASURA_ENDPOINT } = process.env;

const publicFolder = resolve("./public");

const production = ENV === "production";

const webpackLoader = {
  loader: "elm-webpack-loader",
  options: {
    debug: false,
    optimize: production,
    cwd: __dirname,
  },
};

module.exports = {
  mode: production ? "production" : "development",
  entry: "./src/index.js",
  output: {
    // publicPath only necessary due to HMR bug:
    // https://github.com/webpack/webpack-dev-server/issues/1385#issuecomment-482166140
    publicPath: "/",
    path: publicFolder,
    filename: "bundle.js",
  },
  devServer: {
    contentBase: publicFolder,
    proxy: {
      "/graphql": HASURA_ENDPOINT + "/v1",
      changeOrigin: true,
    },
    port: 7777,
    historyApiFallback: true,
    hotOnly: true,
  },
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: production
          ? [webpackLoader]
          : [{ loader: "elm-hot-webpack-loader" }, webpackLoader],
      },
      {
        test: /\.(woff(2)?|otf)$/,
        use: [
          {
            loader: "base64-inline-loader",
            options: {},
          },
        ],
      },
      {
        test: /\.css$/,
        use: ["style-loader", "css-loader"],
      },
    ],
  },
  plugins: [new webpack.NoEmitOnErrorsPlugin()],
};
