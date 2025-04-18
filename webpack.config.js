const path = require('path');

const commonConfig = {
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
        generator: {
          filename: '[name][ext]',
        },
      },
    ],
  },
  resolve: {
    extensions: ['.tsx', '.ts', '.js'],
  },
  output: {
    path: path.resolve(__dirname, 'dist'),
    devtoolModuleFilenameTemplate: '../[resource-path]',
  },
};

const extensionConfig = {
  ...commonConfig,
  target: 'node',
  entry: {
    extension: './src/extension.ts',
  },
  output: {
    ...commonConfig.output,
    filename: 'extension.js',
    libraryTarget: 'commonjs2',
  },
  externals: {
    vscode: 'commonjs vscode',
  },
};

const uiConfig = {
  ...commonConfig,
  target: 'web',
  entry: {
    ui: {
      import: './src/uiEntryPoint.ts',
      library: {
        name: 'Ui',
        type: 'window',
      },
    },
  },
  output: {
    ...commonConfig.output,
    filename: 'ui.js',
  },
};

module.exports = [extensionConfig, uiConfig];
