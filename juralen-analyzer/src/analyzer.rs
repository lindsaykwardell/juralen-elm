use serde::{Deserialize, Serialize};

#[derive(Deserialize, Serialize, Debug)]
pub struct Game {
  #[serde(rename = "nextId")]
  next_id: u32,
  turn: u32,
  grid: Vec<Vec<Cell>>,
  #[serde(rename = "selectedCell")]
  selected_cell: String,
  #[serde(rename = "selectedUnits")]
  selected_units: Vec<u32>,
  players: Vec<Player>,
  #[serde(rename = "activePlayer")]
  active_player: u32,
  units: Vec<Unit>,
  scenario: Scenario,
  // combat: Option<Combat>,
  #[serde(rename = "ai_speed")]
  ai_speed: f32
}

#[derive(Deserialize, Serialize, Debug)]
pub struct Cell {
  #[serde(rename = "cellType")]
  cell_type: String,
  #[serde(rename = "controlledBy")]
  controlled_by: Option<u32>,
  #[serde(rename = "defBonus")]
  def_bonus: u32,
  structure: String,
  farms: u32,
  towers: u32,
  loc: Loc
}

#[derive(Deserialize, Serialize, Debug)]
pub enum Structure {
  Town,
  Citadel,
  None
}

#[derive(Deserialize, Serialize, Debug)]
pub enum Loc {
  Loc(u32, u32)
}

#[derive(Deserialize, Serialize, Debug)]
pub struct Player {
  id: u32,
  name: String,
  resources: Resources,
  #[serde(rename = "isHuman")]
  is_human: bool,
  analyzer: String,
  color: String,
  #[serde(rename = "techTree")]
  tech_tree: TechTree
}

#[derive(Deserialize, Serialize, Debug)]
pub struct Resources {
  actions: f32,
  gold: u32
}

#[derive(Deserialize, Serialize, Debug)]
pub enum AnalyzerMode {
  Default,
  Aggressive,
  Defensive,
  Passive,
  Expansionist
}

#[derive(Deserialize, Serialize, Debug)]
pub struct TechTree {
  #[serde(rename = "levelOne")]
  level_one: Option<String>,
  #[serde(rename = "levelTwo")]
  level_two: Option<String>,
  #[serde(rename = "levelThree")]
  level_three: Option<String>,
  #[serde(rename = "levelFour")]
  level_four: Option<String>
}

#[derive(Deserialize, Serialize, Debug)]
pub struct Unit {
  id: u32,
  #[serde(rename = "unitType")]
  unit_type: String,
  #[serde(rename = "movesLeft")]
  moves_left: u32,
  #[serde(rename = "maxMoves")]
  max_moves: u32,
  attack: u32,
  health: u32,
  #[serde(rename = "maxHealth")]
  max_health: u32,
  range: u32,
  #[serde(rename = "controlledBy")]
  controlled_by: u32,
  loc: Loc,
  level: Level
}

#[derive(Deserialize, Serialize, Debug)]
pub enum UnitType {
  Soldier,
  Warrior,
  Archer,
  Knight,
  Rogue,
  Wizard,
  Priest
  // Archer,
  // Pikeman,
  // Swordsman,
  // Cavalry,
  // Monk,
  // Wizard,
  // Healer,
  // Scout,
  // None
}

#[derive(Deserialize, Serialize, Debug)]
pub struct Level {
  level: u32,
  xp: u32,
  #[serde(rename = "toNextLevel")]
  to_next_level: u32
}

#[derive(Deserialize, Serialize, Debug)]
pub struct Scenario {
  #[serde(rename = "scenarioType")]
  scenario_type: String,
  #[serde(rename = "maxX")]
  max_x: u32,
  #[serde(rename = "maxY")]
  max_y: u32,
  #[serde(rename = "currentX")]
  current_x: u32,
  #[serde(rename = "currentY")]
  current_y: u32,
  finished: bool,
  #[serde(rename = "playerCount")]
  player_count: u32,
  #[serde(rename = "newPlayers")]
  new_players: Vec<NewPlayer>,
  players: Vec<Player>,
  units: Vec<Unit>,
  grid: Vec<Vec<Cell>>,
  #[serde(rename = "nextId")]
  next_id: u32,
  #[serde(rename = "activePlayerId")]
  active_player_id: u32
}

#[derive(Deserialize, Serialize, Debug)]
pub struct NewPlayer {
  id: u32,
  name: String,
  color: String,
  #[serde(rename = "isHuman")]
  is_human: bool,
  analyzer: AnalyzerMode
}