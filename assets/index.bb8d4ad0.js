const e={};import{d as n,r as t,c as o,a,w as r,v as s,t as i,F as l,o as c,b as u}from"./vendor.96dae543.js";let f;!function(e=".",n="__import__"){try{self[n]=new Function("u","return import(u)")}catch(t){const o=new URL(e,location),a=e=>{URL.revokeObjectURL(e.src),e.remove()};self[n]=e=>new Promise(((t,r)=>{const s=new URL(e,o);if(self[n].moduleMap[s])return t(self[n].moduleMap[s]);const i=new Blob([`import * as m from '${s}';`,`${n}.moduleMap['${s}']=m;`],{type:"text/javascript"}),l=Object.assign(document.createElement("script"),{type:"module",src:URL.createObjectURL(i),onerror(){r(new Error(`Failed to import: ${e}`)),a(l)},onload(){t(self[n].moduleMap[s]),a(l)}});document.head.appendChild(l)})),self[n].moduleMap={}}}("/monkey-rs/assets/");let m=0,d=null;function b(){return null!==d&&d.buffer===f.memory.buffer||(d=new Uint8Array(f.memory.buffer)),d}let p=new TextEncoder("utf-8");const w="function"==typeof p.encodeInto?function(e,n){return p.encodeInto(e,n)}:function(e,n){const t=p.encode(e);return n.set(t),{read:e.length,written:t.length}};let y=null;function _(){return null!==y&&y.buffer===f.memory.buffer||(y=new Int32Array(f.memory.buffer)),y}let g=new TextDecoder("utf-8",{ignoreBOM:!0,fatal:!0});function v(e){try{const i=f.__wbindgen_add_to_stack_pointer(-16);var n=function(e,n,t){if(void 0===t){const t=p.encode(e),o=n(t.length);return b().subarray(o,o+t.length).set(t),m=t.length,o}let o=e.length,a=n(o);const r=b();let s=0;for(;s<o;s++){const n=e.charCodeAt(s);if(n>127)break;r[a+s]=n}if(s!==o){0!==s&&(e=e.slice(s)),a=t(a,o,o=s+3*e.length);const n=b().subarray(a+s,a+o);s+=w(e,n).written}return m=s,a}(e,f.__wbindgen_malloc,f.__wbindgen_realloc),t=m;f.run(i,n,t);var o=_()[i/4+0],a=_()[i/4+1];return r=o,s=a,g.decode(b().subarray(r,r+s))}finally{f.__wbindgen_add_to_stack_pointer(16),f.__wbindgen_free(o,a)}var r,s}async function h(n){void 0===n&&(n=new URL("monkey_rs_bg.wasm",e.url));("string"==typeof n||"function"==typeof Request&&n instanceof Request||"function"==typeof URL&&n instanceof URL)&&(n=fetch(n));const{instance:t,module:o}=await async function(e,n){if("function"==typeof Response&&e instanceof Response){if("function"==typeof WebAssembly.instantiateStreaming)try{return await WebAssembly.instantiateStreaming(e,n)}catch(t){if("application/wasm"==e.headers.get("Content-Type"))throw t;console.warn("`WebAssembly.instantiateStreaming` failed because your server does not serve wasm with `application/wasm` MIME type. Falling back to `WebAssembly.instantiate` which is slower. Original error:\n",t)}const o=await e.arrayBuffer();return await WebAssembly.instantiate(o,n)}{const t=await WebAssembly.instantiate(e,n);return t instanceof WebAssembly.Instance?{instance:t,module:e}:t}}(await n,{});return f=t.exports,h.__wbindgen_wasm_module=o,f}g.decode(),u(n({expose:[],setup(e){h();const n=t(""),u=t(""),f=()=>{u.value=v(n.value)};return(e,t)=>(c(),o(l,null,[a("div",null,[r(a("textarea",{"onUpdate:modelValue":t[1]||(t[1]=e=>n.value=e),rows:20,cols:100},null,512),[[s,n.value]])]),a("button",{onClick:f},"run"),a("div",null,i(u.value),1)],64))}})).mount("#app");
