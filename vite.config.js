import { defineConfig } from "vite"
import elmPlugin from "vite-plugin-elm"
import vue from "@vitejs/plugin-vue"

export default defineConfig({
    plugins: [elmPlugin(), vue({ customElement: true })],
})
