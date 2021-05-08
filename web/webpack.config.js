const CopyWebpackPlugin = require("copy-webpack-plugin");
const path = require('path');
const isProduction = process.env.NODE_ENV === 'production'

module.exports = {
  entry: "./bootstrap.js",
  output: {
    publicPath: isProduction ? '/monkey-rs/' : '/',
    path: path.resolve(__dirname, "dist"),
    filename: "bootstrap.js",
  },
  mode: isProduction ? 'production' : 'development',
  plugins: [
    new CopyWebpackPlugin(['index.html'])
  ],
  devServer: {
    writeToDisk: true,
  },
};
