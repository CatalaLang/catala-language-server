const path = require('path');

module.exports = {
  entry: {
    extension: './src/extension.ts',
    ui: {
      import: './src/uiEntryPoint.ts',
      library: {
        name: 'Ui',
        type: 'window', //Ugh... we could do better; investigate
      },
    },
  },
  devtool: 'inline-source-map',
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: 'ts-loader',
        exclude: /node_modules/,
      },
      {
        test: /\.css$/i,
        use: ['style-loader', 'css-loader'],
      },
      {
        test: /\.(woff|woff2|eot|ttf|otf)$/i,
        type: 'asset/resource',
      },
    ],
  },
  resolve: {
    extensions: ['.tsx', '.ts', '.js'],
  },
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: '[name].js',
    libraryTarget: 'commonjs2',
    devtoolModuleFilenameTemplate: '../[resource-path]',
  },
  externals: {
    vscode: 'commonjs vscode',
  },
  //target: 'webworker',
  // we use the node target as we invoke external commands (catala compiler);
  // we might want to look for an alternative solution
  target: 'node',
};
