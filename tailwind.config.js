module.exports = {
    content: ["src/**/*.elm", "src/**/*.vue"],
    theme: {
        extend: {
            colors: {
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
                stoke: "'Stoke', serif",
                vt323: ["VT323", "monospace"],
            },
        },
    },
    plugins: [],
}
