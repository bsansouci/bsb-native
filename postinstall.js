var yauzl = require("yauzl");
var fs = require("fs");
var exec = require("child_process").exec;
var path = require("path");
var https = require('https');

var isWin = process.platform === "win32";
var isOSX = process.platform === "darwin";
function mkdirp(path, cb){
  if (isWin){
    exec("mkdir " + path.replace(/\//g, '\\').replace(/ /g, '\\ '), cb);
  } else {
    exec("mkdir -p " + path, cb);
  }
};

var zipFilename;
if (isWin) {
  zipFilename = "bsb-native-win-v2.1.1.zip";
} else if (isOSX) {
  zipFilename = "bsb-native-osx-v2.1.1.zip";
} else {
  console.error("No linux pre-built binaries yet! Coming soon (Go ping @bsansouci on Discord so he hurries up)");
  return;
}

https.get('https://github.com/bsansouci/bsb-native/releases/download/v2.1.1.007/' + zipFilename, function(res) {
  if (res.statusCode === 302) {
    https.get(res.headers.location, function(res) {
      handleResponse(res);
    });
    return;
  }
  handleResponse(res);
}).on("error", (err) => {
  console.error("Error: " + err.message);
});

function handleResponse(res) {
  res.setEncoding('binary');

  var len = parseInt(res.headers['content-length'], 10);
  var fileStream = fs.createWriteStream(zipFilename, {encoding: "binary"});
  var downloaded = 0;
  
  // A chunk of data has been recieved.
  res.on('data', (chunk) => {
    downloaded += chunk.length;
    fileStream.write(chunk);
    process.stdout.write("Downloading " + (100.0 * downloaded / len).toFixed(2) + "% " + (downloaded / 1000) + " kb\r");
  });
 
  // The whole response has been received. Print out the result.
  res.on('end', () => {
    fileStream.end();

    yauzl.open(zipFilename, {lazyEntries: true}, function(err, zipfile) {
      if (err) throw err;
      var i = 0;
      zipfile.readEntry();
      zipfile.on("entry", function(entry) {
        if (/\/$/.test(entry.fileName)) {
          // Directory file names end with '/'.
          // Note that entires for directories themselves are optional.
          // An entry's fileName implicitly requires its parent directories to exist.
          fs.mkdir(entry.fileName, () => zipfile.readEntry());
        } else {
          // Mode roughly transcribes to the unix permissions.
          // See https://github.com/thejoshwolfe/yauzl/issues/57#issuecomment-301847099          
          var mode = entry.externalFileAttributes >>> 16;
          process.stdout.write("Unzipped " + i + " of "+zipfile.entryCount+" files                  \r");
          zipfile.openReadStream(entry, function(err, readStream) {
            if (err) throw err;
            readStream.on("end", function() {
              i++;
              zipfile.readEntry();
            });
            var writeStream = fs.createWriteStream(entry.fileName, {mode}).on('error', (e) => {
              if (e.errno === -4058 || e.code === 'ENOENT'){
                mkdirp(path.dirname(entry.fileName), (e) => {
                  readStream.pipe(fs.createWriteStream(entry.fileName, {mode}));
                });
              } else {
                console.log(e);
                throw e
              }
            });
            readStream.pipe(writeStream);
          });
        }
      });
    })
  });
}
