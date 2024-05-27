use {
  super::Draw,
  crate::client::{
    backend::{position::Position, screen::Screen, size::Size},
    frontend::Frontend,
  },
  sdl2::{pixels::Color, rect::Rect},
};

impl Draw for Screen {
  fn draw(&self, frontend: &mut Frontend) {
    match self {
      Self::Empty => {}
      Self::Main { player, map: _ } => {
        // self.draw_map(frontend, map);
        // self.draw_player(frontend, player);
        let pos: Position = player.pos();
        let size: Size = player.size();

        frontend.canvas.set_draw_color(Color::RGB(255, 210, 0));
        frontend
          .canvas
          .fill_rect(Rect::new(
            pos.x() as i32,
            pos.y() as i32,
            size.x() as u32,
            size.y() as u32,
          ))
          .unwrap();
      }
    }
  }
}
