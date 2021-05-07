import * as wasm from "../pkg/monkey_rs";

const $ = (selectors) => document.querySelector(selectors)

const $input = $('.input')
const $output = $('.output')
const $run = $('.run')

$run.addEventListener('click', e => {
  const code = $input.value
  const output = wasm.run(code)
  $output.textContent = output
})
