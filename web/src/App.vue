<template>
  <div>
    <textarea v-model="code" :rows="20" :cols="100"></textarea>
  </div>
  <button @click="onRunCode">run</button>
  <div>{{ output }}</div>
</template>

<script setup lang="ts">
import { ref } from 'vue'
import init, { run } from '../../pkg/monkey_rs'

init()

const code = ref('')
const output = ref('')
const onRunCode = () => {
  output.value = run(code.value)
}
</script>
