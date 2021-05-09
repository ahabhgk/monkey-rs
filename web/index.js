import * as wasm from "../pkg";

const $ = (selectors) => document.querySelector(selectors)

const $input = $('.input')
const $output = $('.output')
const $run = $('.run')
const $select = $('.select')

const snippets = {
  '--Choose a code snippet--': '',

  'Hello World!': `"Hello" + " " + "World!"`,

  'Computing integers': `(10 + 2) * 30 + 5`,

  'Conditionals':
`if (true) {
  "Hello"
} else {
  "unreachable"
}`,

  'Var binding':
`let x = 10;
let y = 15;
x + y;`,

  'Array':
`let arr = ["one", "two", "three"];
let arr = push(arr, "four");
arr`,

  'Hash':
`let hash = { "name": "Jimmy", "age": 72, "band": "Led Zeppelin" };
hash`,

  'Function':
`let factorial = fn(n) {
  if (n == 0) {
    1
  } else {
    n * factorial(n - 1)
  }
}
factorial(10)`,

  'Closures':
`let newAdder = fn(x) {
  fn(y) { x + y };
};
let addTwo = newAdder(2);
addTwo(2);`,

  'Reduce & Map':
`let reduce = fn(arr, initial, f) {
  let iter = fn(arr, result) {
    if (len(arr) == 0) {
      result
    } else {
      iter(rest(arr), f(result, first(arr)))
    }
  }
  iter(arr, initial);
};
let map = fn(arr, f) {
  rest(reduce(arr, [0], fn(acc, cur) { push(acc, f(cur)) }))
}
let arr = [1, 2, 3, 4];
let double = fn(x) { x * 2 };
map(arr, double)`,

  'Cons in SICP':
`let cons = fn(first, second) { fn(pick) { pick(first, second) } }
let car = fn(pair) { pair(fn(first, second) { first }) }
let cdr = fn(pair) { pair(fn(first, second) { second }) }
let pair = cons(1, 2)
{ "car": car(pair), "cdr": cdr(pair) }`,
}

$select.innerHTML = Object.keys(snippets)
  .map((snip) => `<option value="${snip}">${snip}</option>`)
  .join('')

$select.addEventListener('change', e => {
  const code = snippets[$select.value]
  $input.value = code
  
  // dispatch an input event to update the hash
  const event = new Event('input')
  $input.dispatchEvent(event)
})

$run.addEventListener('click', e => {
  const code = $input.value
  const output = wasm.run(code)
  $output.textContent = output
})

$input.addEventListener('input', e => {
  const snapshot = getSnapshot($input.value)
  window.location.hash = snapshot
})

function getSnapshot(code) {
  return encodeURIComponent(JSON.stringify({ code }))
}
