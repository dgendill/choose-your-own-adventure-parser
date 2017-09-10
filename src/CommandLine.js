'use strict';

var standalone = require('standalone-html');
var standalone = standalone.api;

exports.createStandaloneHtmlImpl = function(from, to) {
  return function(success, fail) {
    standalone(from, to, "", function(err) {
      if (err) fail(err);
      success();
    });
  }  
}