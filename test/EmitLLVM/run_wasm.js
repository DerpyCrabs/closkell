const fs = require('fs');
var source = fs.readFileSync('./main.wasm');
var typedArray = new Uint8Array(source);
const env = {
    memoryBase: 0,
    tableBase: 0,
    memory: new WebAssembly.Memory({
      initial: 256
    }),
    table: new WebAssembly.Table({
      initial: 0,
      element: 'anyfunc'
    })
  }

WebAssembly.instantiate(typedArray, {
}).then(result => {
  console.log(result.instance.exports.main().toString());
}).catch(e => {
  // error caught
  console.error(e);
});