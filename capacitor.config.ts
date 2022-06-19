import { CapacitorConfig } from "@capacitor/cli"

const config: CapacitorConfig = {
    appId: "com.lindsaykwardell.juralen",
    appName: "Juralen",
    webDir: "./dist",
    bundledWebRuntime: false,
    plugins: {
        SplashScreen: {
            launchShowDuration: 0,
        },
    },
}

export default config
