/*
 ** TailwindCSS Configuration File
 **
 ** Docs: https://tailwindcss.com/docs/configuration
 ** Default: https://github.com/tailwindcss/tailwindcss/blob/master/stubs/defaultConfig.stub.js
 */
const { colors, fontFamily } = require("tailwindcss/defaultTheme")

module.exports = {
    mode: 'jit',
    theme: {
        colors: {
            ...colors,
            "player-red": "rgb(220, 53, 69)",
            "player-blue": "rgb(0, 123, 255)",
            "player-orange": "rgb(255, 120, 0)",
            "player-yellow": "rgb(255, 239, 0)",
            "player-green": "rgb(0, 255, 28)",
            "player-teal": "rgb(0, 255, 243)",
            "player-purple": "rgb(151, 0, 255)",
            "player-gray": "rgb(88, 88, 88)",
            "terrain-mountain": "rgb(170, 170, 170)",
            "terrain-forest": "rgb(72, 133, 79)",
            "terrain-plains": "rgb(155, 118, 83)",
        },
        fontFamily: {
            ...fontFamily,
            stoke: "'Stoke', serif"
        },
    },
    variants: {},
    plugins: [],
    purge: {
        // Learn more on https://tailwindcss.com/docs/controlling-file-size/#removing-unused-css
        enabled: process.env.NODE_ENV === "production",
        content: ["src/**/*.elm"],
        options: {
            whitelist: [],
        },
    },
    future: {
        removeDeprecatedGapUtilities: true,
        purgeLayersByDefault: true,
    },
}
