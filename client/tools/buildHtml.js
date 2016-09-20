// This script copies src/index.html into dist/index.html

// Allowing console calls below since this is a build file.
/*eslint-disable no-console */

import fs from 'fs';
import chalk from 'chalk';
import cheerio from 'cheerio';

fs.readFile('src/index.html', 'utf8', (err, markup) => {
  if (err) {
    return console.error('Errors occurred reading the source index.html file', err);
  }

  const $ = cheerio.load(markup);

  $('head').prepend('<link rel="stylesheet" href="styles.css">');

  fs.writeFile('dist/index.html', $.html(), 'utf8', function (err) {
    if (err) {
      return console.error('Errors occurred writing the index.html file to /dist', err);
    }
    else {
      console.log(chalk.green('index.html written to /dist'));
    }
  });
});

