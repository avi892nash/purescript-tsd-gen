const { execSync } = require('child_process');
const os = require('os');
const fs = require('fs');
const path = require('path');

const platform = os.platform();
const arch = os.arch();
const version = "0.0.0"; // Make sure to update this with your version

const binDir = path.join(__dirname, 'bin');
if (!fs.existsSync(binDir)){
    fs.mkdirSync(binDir);
}

let binaryUrl;
if (platform === 'win32') {
  binaryUrl = `https://github.com/avi892nash/purescript-tsd-gen/purs-tsd-gen/releases/download/v${version}/purs-tsd-gen-win.exe`;
} else if (platform === 'darwin' && arch === 'arm64') {
  binaryUrl = `https://github.com/avi892nash/purescript-tsd-gen/purs-tsd-gen/releases/download/v${version}/purs-tsd-gen-macos-arm64`;
} else if (platform === 'darwin' && arch === 'x64') {
  binaryUrl = `https://github.com/avi892nash/purescript-tsd-gen/purs-tsd-gen/releases/download/v${version}/purs-tsd-gen-macos-x86_64`;
} else if (platform === 'linux') {
  binaryUrl = `https://github.com/avi892nash/purescript-tsd-gen/purs-tsd-gen/releases/download/v${version}/purs-tsd-gen-linux`;
} else {
  console.error(`Unsupported platform: ${platform} ${arch}`);
  process.exit(1);
}

execSync(`curl -L ${binaryUrl} -o ${path.join(binDir, 'purs-tsd-gen')}`);
fs.chmodSync(path.join(binDir, 'purs-tsd-gen'), '755');
console.log('purs-tsd-gen installed successfully!');