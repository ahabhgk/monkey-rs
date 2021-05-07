const CopyWebpackPlugin = require("copy-webpack-plugin");
const path = require('path');

module.exports = {
  entry: "./bootstrap.js",
  output: {
    publicPath: process.env.NODE_ENV === 'production' ? '/monkey-rs/' : '/',
    path: path.resolve(__dirname, "dist"),
    filename: "bootstrap.js",
  },
  mode: process.env.NODE_ENV === 'production' ? 'production' : 'development',
  plugins: [
    new CopyWebpackPlugin(['index.html'])
  ],
  devServer: {
    writeToDisk: true,
  },
};
