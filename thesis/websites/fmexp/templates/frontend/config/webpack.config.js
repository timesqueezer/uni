const path = require('path')
const webpack = require('webpack')

const { CleanWebpackPlugin } = require('clean-webpack-plugin')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const { VueLoaderPlugin } = require('vue-loader')


module.exports = {
  mode: 'development',
  entry: './src/index.js',
  resolve: {
    extensions: ['.js', '.vue'],
    alias: {
      '@': path.resolve(__dirname, '.')
    }
  },
  output: {
    path: path.resolve(__dirname, '../dist'),
    filename: '[name].bundle.js',
    publicPath: '/dist',
  },
  devtool: 'inline-source-map',
  plugins: [
    new HtmlWebpackPlugin({ template: './src/index.html' }),
    new CleanWebpackPlugin(),
    new VueLoaderPlugin(),
    new webpack.DefinePlugin({
      __VUE_OPTIONS_API__: true,
      __VUE_PROD_DEVTOOLS__: false,
    }),
  ],
  module: {
    rules: [
      { test: /\.js$/, exclude: /node_modules/, use: 'babel-loader' },
      { test: /\.vue$/, use: 'vue-loader' },
      { test: /\.(?:ico|gif|png|jpg|jpeg)$/i, type: 'asset/resource' },
      { test: /\.(woff(2)?|eot|ttf|otf|svg|)$/, type: 'asset/inline' },
      { test: /\.(sass|scss|css)$/, use: ['style-loader', 'css-loader', 'sass-loader'] },
    ],
  },
}
