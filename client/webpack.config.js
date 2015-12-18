module.exports = {
  context: __dirname + "/src",
  entry: "./index.jsx",
  output: {
    path: "../static/",
    filename: "bundle.js"
  },
  module: {
    loaders: [
      { test: /\.css$/,           loader: "style!css" },
      // use jsx-loader for all *.jsx files
      { test: /\.jsx$/,            loader: 'jsx-loader?insertPragma=React.DOM&harmony' },

      // loaders for bootstrap fonts:
      { test: /\.(woff|woff2)$/,  loader: "url-loader?limit=10000&mimetype=application/font-woff" },
      { test: /\.ttf$/,           loader: "file-loader" },
      { test: /\.eot$/,           loader: "file-loader" },
      { test: /\.svg$/,           loader: "file-loader" }
    ]
  }
};
