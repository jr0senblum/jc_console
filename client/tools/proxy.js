/*eslint no-console: [1, { allow: ["info", "warn", "error"] }] */

let httpProxy = require('http-proxy');
let chalk = require('chalk');
let scriptName = require('path').basename(__filename);

function makeProxy(pathKey, url, opts) {
  let proxy = {
    url: url,
    opts: opts || {},
    pathKey: pathKey,
    server: httpProxy.createProxyServer({
      target: 'http://10.6.81.152:8080'
    })
  };

  proxy.server.on('error', function (err, req, res) {
    error(chalk.red('ERROR on proxy ' + pathKey), proxy);
    res.writeHead(500, {
      'Content-Type': 'text/plain'
    });
    res.end(JSON.stringify(err));
    error(chalk.red('[Proxy]'), err);
  });

  return proxy;
}

var proxy = makeProxy('/api', 'http://10.6.81.152:8080', {});

function applicationProxyMiddleware(req, res, next) {
  res.setHeader('Access-Control-Allow-Origin', '*');

  debug('Incoming Request', chalk.cyan(req.url));

  let apiProxyPath = new RegExp('^\/(api\/)+\\b\/?', 'i');

  if (req.url.match(apiProxyPath)) {

    let finalUrl = req.url.replace(/^\/(api\/)+/i, '/');

    debug(chalk.white('Using Proxy'), 'for', chalk.cyan(req.url), 'as', chalk.cyan(finalUrl));

    req.url = finalUrl;

    proxy.server.web(req, res);

    return;
  }


  debug(chalk.white('Saw unproxied request to'), chalk.cyan(req.url));

  next();
}

// Private var to avoid repeating this text over and over

function info(...args) {
  console.info('[' + chalk.gray(scriptName) + ']', ...args);
}

function debug(...args) {
  info(...args);
}

function error(...args) {
  console.error('[' + chalk.gray(scriptName) + ']', ...args);
}

export default function http() {
  return applicationProxyMiddleware;

}
