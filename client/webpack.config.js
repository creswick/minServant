module.exports = {
  context: __dirname + "/src",
  entry: "./index.js",
  output: {
    path: "../static/",
    filename: "bundle.js"
  },
  module: {
    loaders: [
      { test: /\.css$/,
        loader: "style!css" },
      { // use jsx-loader for all *.jsx files
        test: /\.js$/,
        loader: 'jsx-loader?insertPragma=React.DOM&harmony'
      }
    ]
  }
};
