mod analyzer;

use wasm_bindgen::prelude::*;

// #[wasm_bindgen]
// extern {
//     fn alert(s: &str);
// }

#[wasm_bindgen]
extern {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

#[wasm_bindgen]
pub fn main() {
    log("Hello, world!");
}

#[wasm_bindgen]
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[wasm_bindgen]
pub fn analyze(json: &str) {
    log(json);
    let game: analyzer::Game = serde_json::from_str(json).unwrap();
    // // Return the game as a JSON string
    let game_json = serde_json::to_string(&game).unwrap();
    log(&game_json);
}