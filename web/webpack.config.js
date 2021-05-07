const CopyWebpackPlugin = require("copy-webpack-plugin");
const path = require('path');

module.exports = {
  entry: "./bootstrap.js",
  output: {
    publicPath: '/monkey-rs/',
    path: path.resolve(__dirname, "dist"),
    filename: "bootstrap.js",
  },
  mode: process.env.NODE_ENV === 'production' ? 'production' : 'development',
  plugins: [
    new CopyWebpackPlugin(['index.html'])
  ],
};
