// More info on Webpack's Node API here: https://webpack.github.io/docs/node.js-api.html
// Allowing console calls below since this is a build file.
/*eslint-disable no-console */
import webpack from 'webpack';
import webpackConfigBuilder from '../webpack.config';
import chalk from 'chalk';
import { argv as args } from 'yargs';

const inSilentMode = args.s; // set to true when -s is passed on the command
const inWatchMode = args.watch; // set to true when --watch is passed on the command

process.env.NODE_ENV = 'production'; // this assures React is built in prod mode and that the Babel dev config doesn't apply.

const webpackConfig = webpackConfigBuilder(process.env.NODE_ENV);

const compiler = webpack(webpackConfig);

if (!inWatchMode) {
  compiler.run(webpackCompilationHandler);
}
else {
  compiler.watch({// watch options:
    aggregateTimeout: 300, // wait so long for more changes
    poll: true // use polling instead of native watchers
    // pass a number to set the polling interval
  }, webpackCompilationHandler);
}


function webpackCompilationHandler(err, stats) {

  if (!inSilentMode) {
    console.log(chalk.bold.blue('Generating minified bundle for production use via Webpack...'));
  }

  if (err) { // so a fatal error occurred. Stop here.
    console.error('Fatal Error Occurred', chalk.bold.red(err));

    return 1;
  }

  const jsonStats = stats.toJson();

  if (jsonStats.hasErrors) {
    console.error('JSON Stats has errors');
    return jsonStats.errors.map(error => console.log(chalk.red(error)));
  }

  if (jsonStats.hasWarnings && !inSilentMode) {
    console.warn(chalk.bold.yellow('Webpack generated the following warnings: '));
    jsonStats.warnings.map(warning => console.log(chalk.yellow(warning)));
  }

  if (!inSilentMode) {
    console.log(`Webpack stats: ${stats}`);
  }

  if (err) {
    console.error(chalk.red.bold('Errors prevented the build from completing successfully'), err);
  }
  else {
    console.log(chalk.green.bold('Your app has been compiled in production mode and written to /dist. It\'s ready to roll!'));
  }

  return 0;
}
