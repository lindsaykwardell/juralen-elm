<template>
    <div>
        <button class="w-8 h-8 rounded-lg bg-gray-300 mr-2 text-xl" @click="zoomIn">
            +
        </button>
        <button class="w-8 h-8 rounded-lg bg-gray-300 ml-2 text-xl" @click="zoomOut">
            -
        </button>
    </div>
</template>

<script setup>
import { onMounted, computed } from "vue"
const root = document.documentElement

function getCurrentZoom() {
    return parseInt(
        root.style.getPropertyValue("--cell-size")?.replace("px", ""),
        10
    )
}

function zoomIn() {
    const currentZoom = getCurrentZoom()

    updateZoom(currentZoom + 15)
}

function zoomOut() {
    const currentZoom = getCurrentZoom()

    updateZoom(currentZoom - 15)
}

function updateZoom(zoom) {
    localStorage.setItem("zoom", zoom)
    root.style.setProperty("--cell-size", `${zoom}px`)
}

onMounted(() => {
    const zoom = localStorage.getItem("zoom") || 85
    updateZoom(zoom)
})
</script>

<style lang="postcss">
@tailwind base;
@tailwind components;
@tailwind utilities;
</style>
