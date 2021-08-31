<template>
    <div>
        <button class="w-8 h-8 rounded-full bg-gray-300" @click="zoomIn">
            +
        </button>
        <button class="w-8 h-8 rounded-full bg-gray-300" @click="zoomOut">
            -
        </button>
    </div>
</template>

<script setup>
import { onMounted, computed } from "vue"
const root = document.documentElement

function zoomIn() {
  const currentZoom = parseInt(
            root.style.getPropertyValue("--cell-size")?.replace("px", ""),
            10
        )
    console.log(root.style.getPropertyValue("--cell-size"), currentZoom)
    root.style.setProperty("--cell-size", `${currentZoom + 15}px`)
}

function zoomOut() {
    const currentZoom = parseInt(
            root.style.getPropertyValue("--cell-size")?.replace("px", ""),
            10
        )
    console.log(root.style.getPropertyValue("--cell-size"), currentZoom)
    root.style.setProperty("--cell-size", `${currentZoom - 15}px`)
}

onMounted(() => {
    root.style.setProperty("--cell-size", "85px")
})
</script>
