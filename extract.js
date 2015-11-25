
var bc = require('babel-core');
    util = require('util');

var requestedFile = process.argv[2];
    requestedWord = process.argv[3];
    source = bc.transformFileSync(requestedFile, {code: false});
    scope = source.ast.program._scopeInfo;

var data = scope.bindings[requestedWord];
    imported = source.metadata.modules.imports;

if (data) {
    console.log(JSON.stringify({
        identifier: data.identifier,
        declaredType: data.path.type,
        imported: imported,
        status: 'success'
    }));
}
console.log(JSON.stringify({status: 'notfound'}));
