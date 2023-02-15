// import the parser class from ./Parser
let {Parser} = require('./Parser');

let readline = require('readline');
let reader = readline.createInterface({
    input: process.stdin,
    output: process.stdout
}); 

// creating an array to store the input
let lines = [];

reader.on('line', function (cmd) {
    lines.push(cmd);
});

reader.on('close', function () {
    // create new parser object
    const parser = new Parser(lines);
    parser.parse();
    process.exit(0);
});