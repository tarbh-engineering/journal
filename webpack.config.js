const { resolve } = require("path");
const webpack = require("webpack");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const InlineChunkHtmlPlugin = require("react-dev-utils/InlineChunkHtmlPlugin");

const {
  ENV,
  HASURA_ENDPOINT,
  STRIPE_PROJECT_ID,
  STRIPE_ANNUAL,
  STRIPE_CHARGE,
} = process.env;

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
      ...(production
        ? [
            {
              test: /\.js$/,
              exclude: /(node_modules)/,
              use: {
                loader: "babel-loader",
                options: {
                  presets: ["@babel/preset-env"],
                },
              },
            },
          ]
        : []),
    ],
  },
  plugins: [
    new webpack.DefinePlugin({
      STRIPE_PROJECT_ID: JSON.stringify(STRIPE_PROJECT_ID),
      STRIPE_ANNUAL: JSON.stringify(STRIPE_ANNUAL),
      STRIPE_CHARGE,
    }),
    new webpack.NoEmitOnErrorsPlugin(),
    new HtmlWebpackPlugin({
      minify: false,
      cache: false,
      inject: true,
      template: "./src/index.html",
    }),
    new InlineChunkHtmlPlugin(HtmlWebpackPlugin, [/.(js)$/]),
  ],
};
