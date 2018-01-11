var yauzl = require("yauzl");
var fs = require("fs");
var exec = require("child_process").exec;
var path = require("path");
var https = require('https');

var isWin = process.platform === "win32";
var isOSX = process.platform === "darwin";
var isLinux = process.platform === "linux";
function mkdirp(path, cb){
  if (isWin){
    exec("mkdir " + path.replace(/\//g, '\\').replace(/ /g, '\\ '), cb);
  } else {
    exec("mkdir -p " + path, cb);
  }
};

var zipFilename;
if (isWin) {
  zipFilename = "bsb-native-win-2.1.1.zip";
} else if (isOSX) {
  zipFilename = "bsb-native-osx-2.1.1.zip";
} else if (isLinux) {
  zipFilename = "bsb-native-linux-2.1.1.zip";
} else {
  console.error("No pre-built binaries for " + process.platform + " yet! Please open an issue on bsansouci/bsb-native!");
  return;
}

https.get('https://github.com/bsansouci/bsb-native/releases/download/2.1.1/' + zipFilename, function(res) {
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
var encoding =  'binary';

function handleResponse(res) {
  res.setEncoding('binary');

  var len = parseInt(res.headers['content-length'], 10);
  var fileStream = fs.createWriteStream(zipFilename, { encoding });
  var downloaded = 0;
  
  // A chunk of data has been recieved.
  res.on('data', (chunk) => {
    downloaded += chunk.length;
    fileStream.write(chunk);
    process.stdout.write("Downloading " + (100.0 * downloaded / len).toFixed(2) + "% " + (downloaded / 1000) + " kb\r");
  });
  
  fileStream.on('finish', function() {
    yauzl.open(zipFilename, {lazyEntries: true}, function(err, zipfile) {
      if (err) throw err;
      var i = 0;
      zipfile.readEntry();
      zipfile.once("close", function(entry) {
        fs.unlink(zipFilename, () => console.log("Unzip successful."));
      });
      zipfile.on("entry", function(entry) {
        if (/\/$/.test(entry.fileName)) {
          // Directory file names end with '/'.
          // Note that entires for directories themselves are optional.
          // An entry's fileName implicitly requires its parent directories to exist.
          fs.mkdir(entry.fileName, () => zipfile.readEntry());
        } else {
          // Mode roughly translates to unix permissions.
          // See https://github.com/thejoshwolfe/yauzl/issues/57#issuecomment-301847099          
          var mode = entry.externalFileAttributes >>> 16;
          process.stdout.write("Unzipped " + i + " of "+zipfile.entryCount+" files                  \r");
          zipfile.openReadStream(entry, function(err, readStream) {
            if (err) throw err;
            readStream.on("end", function() {
              i++;
              zipfile.readEntry();
            });
            var writeStream = fs.createWriteStream(entry.fileName, {mode, encoding}).on('error', (e) => {
              if (e.errno === -4058 || e.code === 'ENOENT'){
                mkdirp(path.dirname(entry.fileName), (e) => {
                  readStream.pipe(fs.createWriteStream(entry.fileName, {mode, encoding}));
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

  // The whole response has been received. Print out the result.
  res.on('end', function() {
    fileStream.end();
  });
}
