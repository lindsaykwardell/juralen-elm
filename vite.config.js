import { defineConfig } from "vite"
import elmPlugin from "vite-plugin-elm"
import vue from "@vitejs/plugin-vue"
import { VitePWA } from "vite-plugin-pwa"

export default defineConfig({
    plugins: [
        elmPlugin(),
        vue(),
        VitePWA({
            registerType: "autoUpdate",
            includeAssets: [
                "/img/**",
                "favicon.svg",
                "favicon.ico",
                "robots.txt",
                "apple-touch-icon.png",
            ],
            manifest: {
                name: "Juralen",
                short_name: "Juralen",
                description: "Turn-Based Strategy Game",
                theme_color: "#333",
                icons: [
                    {
                        src: "pwa-192x192.png",
                        sizes: "192x192",
                        type: "image/png",
                    },
                    {
                        src: "pwa-512x512.png",
                        sizes: "512x512",
                        type: "image/png",
                    },
                    {
                        src: "pwa-512x512.png",
                        sizes: "512x512",
                        type: "image/png",
                        purpose: "any maskable",
                    },
                ],
            },
        }),
    ],
    // server: {
    //     hmr: {
    //         overlay: false
    //     }
    // }
})
