import { defineConfig } from "vite"
import elmPlugin from "vite-plugin-elm"
import vue from "@vitejs/plugin-vue"
import { VitePWA } from "vite-plugin-pwa"

export default defineConfig({
    plugins: [elmPlugin(), vue({ customElement: true }), VitePWA({
        includeAssets: ['/img/**'],
        manifest: {
            name: 'Juralen',
            short_name: 'Juralen',
            description: 'Turn-Based Strategy Game',
            theme_color: '#333'
          }
    })],
})
