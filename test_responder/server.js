var http = require('express');
var app = http();
app.get('/',function(req, res) {
  var body = 'Home response';
  res.setHeader('Content-Type','text/plain');
  res.setHeader('Content-Length',body.length);
  res.end(body);
});
app.post('/',function(req,res){
  var body = 'Home response';
  res.setHeader('Content-Type','text/plain');
  res.setHeader('Content-Length',body.length);
  res.end(body);
});
app.put('/',function(req,res){
  var body = 'Home response';
  res.setHeader('Content-Type','text/plain');
  res.setHeader('Content-Length',body.length);
  res.end(body);
});
app.delete('/',function(req,res){
  var body = 'Home response';
  res.setHeader('Content-Type','text/plain');
  res.setHeader('Content-Length',body.length);
  res.end(body);
});
app.get('/bad_request',function(req, res) {
  res.setHeader('Content-Type','text/plain');
  res.send(400,'bad request');
});
app.post('/bad_request',function(req,res){
  res.setHeader('Content-Type','text/plain');
  res.send(400,'bad request');
});
app.put('/bad_request',function(req,res){
  res.setHeader('Content-Type','text/plain');
  res.send(400,'bad request');
});
app.delete('/bad_request',function(req,res){
  res.setHeader('Content-Type','text/plain');
  res.send(400,'bad request');
});
app.get('/unauthorized',function(req, res) {
  res.setHeader('Content-Type','text/plain');
  res.send(401,'unauthorized');
});
app.post('/unauthorized',function(req,res){
  res.setHeader('Content-Type','text/plain');
  res.send(401,'unauthorized');
});
app.put('/unauthorized',function(req,res){
  res.setHeader('Content-Type','text/plain');
  res.send(401,'unauthorized');
});
app.delete('/unauthorized',function(req,res){
  res.setHeader('Content-Type','text/plain');
  res.send(401,'unauthorized');
});
app.get('/forbidden',function(req, res) {
  res.setHeader('Content-Type','text/plain');
  res.send(403,'forbidden');
});
app.post('/forbidden',function(req,res){
  res.setHeader('Content-Type','text/plain');
  res.send(403,'forbidden');
});
app.put('/forbidden',function(req,res){
  res.setHeader('Content-Type','text/plain');
  res.send(403,'forbidden');
});
app.delete('/forbidden',function(req,res){
  res.setHeader('Content-Type','text/plain');
  res.send(403,'forbidden');
});
app.get('/internal_server_error',function(req, res) {
  res.setHeader('Content-Type','text/plain');
  res.send(500);
});
app.post('/internal_server_error',function(req,res){
  res.setHeader('Content-Type','text/plain');
  res.send(500);
});
app.put('/internal_server_error',function(req,res){
  res.setHeader('Content-Type','text/plain');
  res.send(500);
});
app.delete('/internal_server_error',function(req,res){
  res.setHeader('Content-Type','text/plain');
  res.send(500);
});
app.listen(1337);
