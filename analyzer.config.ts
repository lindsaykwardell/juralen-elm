import { defineConfig } from "vite"
import vue from "@vitejs/plugin-vue"
import elmPlugin from "vite-plugin-elm"
import path from "path"
import typescript from "@rollup/plugin-typescript"

const resolvePath = (str: string) => path.resolve(__dirname, str)

// https://vitejs.dev/config/
export default defineConfig({
    publicDir: false,
    plugins: [vue(), elmPlugin()],
    build: {
        outDir: "./src/analyzer",
        sourcemap: false,
        lib: {
            entry: path.resolve(__dirname, "src/Game/Analyzer/analyzer.ts"),
            name: "juralen-analyzer",
            fileName: (format) => `juralen-analyzer.${format}.js`,
        },
        rollupOptions: {
            // make sure to externalize deps that shouldn't be bundled
            // into your library
            external: ["vue"],
            output: {
                // Provide global variables to use in the UMD build
                // for externalized deps
                globals: {
                    vue: "Vue",
                },
            },
            plugins: [
                typescript({
                    target: "es2020",
                    rootDir: resolvePath("./src/Game/Analyzer"),
                    declaration: true,
                    declarationDir: resolvePath("./src/analyzer"),
                    exclude: resolvePath("./node_modules/**"),
                    allowSyntheticDefaultImports: true,
                }),
            ],
        },
    },
})
