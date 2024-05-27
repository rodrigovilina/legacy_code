#[derive(Default, Debug)]
pub struct Unit {
  pub pos: (usize, usize),
}

#[derive(Default, Debug)]
pub struct Game {
  pub unit: Unit,
}

pub fn clear_terminal() {
  print!("\x1B[2J\x1B[1;1H");
}
