const express = require("express");
const httpProxy = require("http-proxy");
const server = "http://localhost:5000";
const client = "http://localhost:3000";

const app = express();
const apiProxy = httpProxy.createProxyServer();

app.all("/api/*", function (req, res) {
  apiProxy.web(req, res, { target: server });
});

app.all("/*", function (req, res) {
  apiProxy.web(req, res, { target: client });
});

app.listen(8000);
console.log("proxy server on port 8000");
