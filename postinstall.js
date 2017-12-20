var yauzl = require("yauzl");
var fs = require("fs");
var exec = require("child_process").exec;
var path = require("path");
var https = require('https');

function mkdirp(path, cb){
  console.log(path);
  if (process.platform === "win32"){
    exec("mkdir " + path.replace(/\//g, '\\').replace(/ /g, '\\ '), cb);
  } else {
    exec("mkdir -p " + path, cb);
  }
};

var zipFilename;
var isWin = process.platform === "win32";
var isOSX = process.platform === "darwin";
if (isWin) {
  zipFilename = "bsb-native-win-v2.1.1.zip";
} else if (isOSX) {
  zipFilename = "bsb-native-osx-v2.1.1.zip";
} else {
  console.error("No linux pre-built binaries yet! Coming soon (Go ping @bsansouci on Discord so he hurries up)");
  return;
}

https.get('https://github.com/bsansouci/bsb-native/releases/download/v2.1.1.007/' + zipFilename, (resp) => {
  var len = parseInt(resp.headers['content-length'], 10);
  
  var fileStream = fs.createWriteStream(zipFilename)
 
  // A chunk of data has been recieved.
  resp.on('data', (chunk) => {
    fileStream.write(chunk);
    process.stdout.write("Downloading " + (100.0 * downloaded / len).toFixed(2) + "% " + downloaded + " bytes\r");
  });
 
  // The whole response has been received. Print out the result.
  resp.on('end', () => {
    file.end();

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
          // file entry

          process.stdout.write("Unzipped " + i + " of 3385 files\r");
          zipfile.openReadStream(entry, function(err, readStream) {
            if (err) throw err;
            readStream.on("end", function() {
              i++;
              zipfile.readEntry();
            });
            var writeStream = fs.createWriteStream(entry.fileName).on('error', (e) => {
              if (e.errno === -4058){
                mkdirp(path.dirname(entry.fileName), (e) => {
                  readStream.pipe(fs.createWriteStream(entry.fileName));
                });
              } else {
                throw e
              }
            });
            readStream.pipe(writeStream);
          });
        }
      });
    });

  });
 
}).on("error", (err) => {
  console.error("Error: " + err.message);
});
