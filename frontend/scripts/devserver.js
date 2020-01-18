const proxy = require('http-proxy-middleware')
const Bundler = require('parcel')
const express = require('express')

let bundler = new Bundler('./src/index.html', {cache: false})
let app = express()

app.use(
  '/api',
  proxy({
    target: 'http://localhost:8190'
  })
)

app.use(bundler.middleware())

app.listen(1234)
