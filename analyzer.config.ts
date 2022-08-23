import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";
const path = require("path");
import typescript from "@rollup/plugin-typescript";

const resolvePath = (str: string) => path.resolve(__dirname, str);

// https://vitejs.dev/config/
export default defineConfig({
  publicDir: false,
  plugins: [elmPlugin()],
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
      external: [],
      output: {
        // Provide global variables to use in the UMD build
        // for externalized deps
        globals: {},
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
});
