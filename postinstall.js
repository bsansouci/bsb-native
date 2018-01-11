var yauzl = require("yauzl");
var fs = require("fs");
var exec = require("child_process").exec;
var path = require("path");
var https = require('https');

var isWin = process.platform === "win32";
var isOSX = process.platform === "darwin";
var isLinux = process.platform === "linux";
function mkdirp(dir, cb) {
  if (dir === ".") return cb();
  fs.stat(dir, function(err) {
    if (err == null) return cb(); // already exists

    var parent = path.dirname(dir);
    mkdirp(parent, function() {
      // process.stdout.write(dir.replace(/\/$/, "") + "/\n");
      fs.mkdir(dir, cb);
    });
  });
}

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
        fs.unlink(zipFilename, () => console.log("Unzip successful.                          "));
      });
      zipfile.on("entry", function(entry) {
        if (/\/$/.test(entry.fileName)) {
          // directory file names end with '/'
          mkdirp(entry.fileName, function() {
            if (err) throw err;
            zipfile.readEntry();
          });
        } else {
          process.stdout.write("Unzipped " + i + " of "+zipfile.entryCount+" files                  \r");
          mkdirp(path.dirname(entry.fileName), function() {
            zipfile.openReadStream(entry, function(err, readStream) {
              if (err) throw err;
              readStream.on("end", function() {
                i++;
                zipfile.readEntry();
              });
              // Mode roughly translates to unix permissions.
              // See https://github.com/thejoshwolfe/yauzl/issues/57#issuecomment-301847099          
              var mode = entry.externalFileAttributes >>> 16;
              var writeStream = fs.createWriteStream(entry.fileName, {mode, encoding})
              readStream.pipe(writeStream);
            });
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
