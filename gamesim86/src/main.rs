mod game_simulator;

use std::path::Path;
use game_simulator::GameSimulator;

#[show_image::main]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let game = GameSimulator::new()?;
    sim8086::simulate(Path::new("gamesim86/sna86/sna86"), game, true, None)?;

    Ok(())
}