(window["webpackJsonp"] = window["webpackJsonp"] || []).push([[0],{

/***/ "../pkg/monkey_rs.js":
/*!***************************!*\
  !*** ../pkg/monkey_rs.js ***!
  \***************************/
/*! exports provided: run */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
eval("__webpack_require__.r(__webpack_exports__);\n/* harmony import */ var _monkey_rs_bg_wasm__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./monkey_rs_bg.wasm */ \"../pkg/monkey_rs_bg.wasm\");\n/* harmony import */ var _monkey_rs_bg_js__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ./monkey_rs_bg.js */ \"../pkg/monkey_rs_bg.js\");\n/* harmony reexport (safe) */ __webpack_require__.d(__webpack_exports__, \"run\", function() { return _monkey_rs_bg_js__WEBPACK_IMPORTED_MODULE_1__[\"run\"]; });\n\n\n\n\n//# sourceURL=webpack:///../pkg/monkey_rs.js?");

/***/ }),

/***/ "../pkg/monkey_rs_bg.js":
/*!******************************!*\
  !*** ../pkg/monkey_rs_bg.js ***!
  \******************************/
/*! exports provided: run */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
eval("__webpack_require__.r(__webpack_exports__);\n/* WEBPACK VAR INJECTION */(function(module) {/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, \"run\", function() { return run; });\n/* harmony import */ var _monkey_rs_bg_wasm__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./monkey_rs_bg.wasm */ \"../pkg/monkey_rs_bg.wasm\");\n\n\nlet WASM_VECTOR_LEN = 0;\n\nlet cachegetUint8Memory0 = null;\nfunction getUint8Memory0() {\n    if (cachegetUint8Memory0 === null || cachegetUint8Memory0.buffer !== _monkey_rs_bg_wasm__WEBPACK_IMPORTED_MODULE_0__[\"memory\"].buffer) {\n        cachegetUint8Memory0 = new Uint8Array(_monkey_rs_bg_wasm__WEBPACK_IMPORTED_MODULE_0__[\"memory\"].buffer);\n    }\n    return cachegetUint8Memory0;\n}\n\nconst lTextEncoder = typeof TextEncoder === 'undefined' ? (0, module.require)('util').TextEncoder : TextEncoder;\n\nlet cachedTextEncoder = new lTextEncoder('utf-8');\n\nconst encodeString = (typeof cachedTextEncoder.encodeInto === 'function'\n    ? function (arg, view) {\n    return cachedTextEncoder.encodeInto(arg, view);\n}\n    : function (arg, view) {\n    const buf = cachedTextEncoder.encode(arg);\n    view.set(buf);\n    return {\n        read: arg.length,\n        written: buf.length\n    };\n});\n\nfunction passStringToWasm0(arg, malloc, realloc) {\n\n    if (realloc === undefined) {\n        const buf = cachedTextEncoder.encode(arg);\n        const ptr = malloc(buf.length);\n        getUint8Memory0().subarray(ptr, ptr + buf.length).set(buf);\n        WASM_VECTOR_LEN = buf.length;\n        return ptr;\n    }\n\n    let len = arg.length;\n    let ptr = malloc(len);\n\n    const mem = getUint8Memory0();\n\n    let offset = 0;\n\n    for (; offset < len; offset++) {\n        const code = arg.charCodeAt(offset);\n        if (code > 0x7F) break;\n        mem[ptr + offset] = code;\n    }\n\n    if (offset !== len) {\n        if (offset !== 0) {\n            arg = arg.slice(offset);\n        }\n        ptr = realloc(ptr, len, len = offset + arg.length * 3);\n        const view = getUint8Memory0().subarray(ptr + offset, ptr + len);\n        const ret = encodeString(arg, view);\n\n        offset += ret.written;\n    }\n\n    WASM_VECTOR_LEN = offset;\n    return ptr;\n}\n\nlet cachegetInt32Memory0 = null;\nfunction getInt32Memory0() {\n    if (cachegetInt32Memory0 === null || cachegetInt32Memory0.buffer !== _monkey_rs_bg_wasm__WEBPACK_IMPORTED_MODULE_0__[\"memory\"].buffer) {\n        cachegetInt32Memory0 = new Int32Array(_monkey_rs_bg_wasm__WEBPACK_IMPORTED_MODULE_0__[\"memory\"].buffer);\n    }\n    return cachegetInt32Memory0;\n}\n\nconst lTextDecoder = typeof TextDecoder === 'undefined' ? (0, module.require)('util').TextDecoder : TextDecoder;\n\nlet cachedTextDecoder = new lTextDecoder('utf-8', { ignoreBOM: true, fatal: true });\n\ncachedTextDecoder.decode();\n\nfunction getStringFromWasm0(ptr, len) {\n    return cachedTextDecoder.decode(getUint8Memory0().subarray(ptr, ptr + len));\n}\n/**\n* @param {string} code\n* @returns {string}\n*/\nfunction run(code) {\n    try {\n        const retptr = _monkey_rs_bg_wasm__WEBPACK_IMPORTED_MODULE_0__[\"__wbindgen_add_to_stack_pointer\"](-16);\n        var ptr0 = passStringToWasm0(code, _monkey_rs_bg_wasm__WEBPACK_IMPORTED_MODULE_0__[\"__wbindgen_malloc\"], _monkey_rs_bg_wasm__WEBPACK_IMPORTED_MODULE_0__[\"__wbindgen_realloc\"]);\n        var len0 = WASM_VECTOR_LEN;\n        _monkey_rs_bg_wasm__WEBPACK_IMPORTED_MODULE_0__[\"run\"](retptr, ptr0, len0);\n        var r0 = getInt32Memory0()[retptr / 4 + 0];\n        var r1 = getInt32Memory0()[retptr / 4 + 1];\n        return getStringFromWasm0(r0, r1);\n    } finally {\n        _monkey_rs_bg_wasm__WEBPACK_IMPORTED_MODULE_0__[\"__wbindgen_add_to_stack_pointer\"](16);\n        _monkey_rs_bg_wasm__WEBPACK_IMPORTED_MODULE_0__[\"__wbindgen_free\"](r0, r1);\n    }\n}\n\n\n/* WEBPACK VAR INJECTION */}.call(this, __webpack_require__(/*! ./../web/node_modules/webpack/buildin/harmony-module.js */ \"./node_modules/webpack/buildin/harmony-module.js\")(module)))\n\n//# sourceURL=webpack:///../pkg/monkey_rs_bg.js?");

/***/ }),

/***/ "../pkg/monkey_rs_bg.wasm":
/*!********************************!*\
  !*** ../pkg/monkey_rs_bg.wasm ***!
  \********************************/
/*! exports provided: memory, run, __wbindgen_add_to_stack_pointer, __wbindgen_malloc, __wbindgen_realloc, __wbindgen_free */
/***/ (function(module, exports, __webpack_require__) {

eval("\"use strict\";\n// Instantiate WebAssembly module\nvar wasmExports = __webpack_require__.w[module.i];\n__webpack_require__.r(exports);\n// export exports from WebAssembly module\nfor(var name in wasmExports) if(name != \"__webpack_init__\") exports[name] = wasmExports[name];\n// exec imports from WebAssembly module (for esm order)\n\n\n// exec wasm module\nwasmExports[\"__webpack_init__\"]()\n\n//# sourceURL=webpack:///../pkg/monkey_rs_bg.wasm?");

/***/ }),

/***/ "./index.js":
/*!******************!*\
  !*** ./index.js ***!
  \******************/
/*! no exports provided */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
eval("__webpack_require__.r(__webpack_exports__);\n/* harmony import */ var _pkg__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../pkg */ \"../pkg/monkey_rs.js\");\n\n\nconst $ = (selectors) => document.querySelector(selectors)\n\nconst $input = $('.input')\nconst $output = $('.output')\nconst $run = $('.run')\nconst $select = $('.select')\n\nconst snippets = {\n  '--Choose a code snippet--': '',\n\n  'Hello World!': `\"Hello\" + \" \" + \"World!\"`,\n\n  'Computing integers': `(10 + 2) * 30 + 5`,\n\n  'Conditionals':\n`if (true) {\n  \"Hello\"\n} else {\n  \"unreachable\"\n}`,\n\n  'Var binding':\n`let x = 10;\nlet y = 15;\nx + y;`,\n\n  'Array':\n`let arr = [\"one\", \"two\", \"three\"];\nlet arr = push(arr, \"four\");\narr`,\n\n  'Hash':\n`let hash = { \"name\": \"Jimmy\", \"age\": 72, \"band\": \"Led Zeppelin\" };\nhash`,\n\n  'Function':\n`let factorial = fn(n) {\n  if (n == 0) {\n    1\n  } else {\n    n * factorial(n - 1)\n  }\n}\nfactorial(10)`,\n\n  'Closures':\n`let newAdder = fn(x) {\n  fn(y) { x + y };\n};\nlet addTwo = newAdder(2);\naddTwo(2);`,\n\n  'Reduce & Map':\n`let reduce = fn(arr, initial, f) {\n  let iter = fn(arr, result) {\n    if (len(arr) == 0) {\n      result\n    } else {\n      iter(rest(arr), f(result, first(arr)))\n    }\n  }\n  iter(arr, initial);\n};\nlet map = fn(arr, f) {\n  rest(reduce(arr, [0], fn(acc, cur) { push(acc, f(cur)) }))\n}\nlet arr = [1, 2, 3, 4];\nlet double = fn(x) { x * 2 };\nmap(arr, double)`,\n}\n\n$select.innerHTML = Object.keys(snippets)\n  .map((snip) => `<option value=\"${snip}\">${snip}</option>`)\n  .join('')\n\n$select.addEventListener('change', e => {\n  const code = snippets[$select.value]\n  $input.value = code\n})\n\n$run.addEventListener('click', e => {\n  const code = $input.value\n  const output = _pkg__WEBPACK_IMPORTED_MODULE_0__[\"run\"](code)\n  $output.textContent = output\n})\n\n\n//# sourceURL=webpack:///./index.js?");

/***/ }),

/***/ "./node_modules/webpack/buildin/harmony-module.js":
/*!*******************************************!*\
  !*** (webpack)/buildin/harmony-module.js ***!
  \*******************************************/
/*! no static exports found */
/***/ (function(module, exports) {

eval("module.exports = function(originalModule) {\n\tif (!originalModule.webpackPolyfill) {\n\t\tvar module = Object.create(originalModule);\n\t\t// module.parent = undefined by default\n\t\tif (!module.children) module.children = [];\n\t\tObject.defineProperty(module, \"loaded\", {\n\t\t\tenumerable: true,\n\t\t\tget: function() {\n\t\t\t\treturn module.l;\n\t\t\t}\n\t\t});\n\t\tObject.defineProperty(module, \"id\", {\n\t\t\tenumerable: true,\n\t\t\tget: function() {\n\t\t\t\treturn module.i;\n\t\t\t}\n\t\t});\n\t\tObject.defineProperty(module, \"exports\", {\n\t\t\tenumerable: true\n\t\t});\n\t\tmodule.webpackPolyfill = 1;\n\t}\n\treturn module;\n};\n\n\n//# sourceURL=webpack:///(webpack)/buildin/harmony-module.js?");

/***/ })

}]);