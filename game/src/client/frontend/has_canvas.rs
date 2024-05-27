use {super::Frontend, sdl2::pixels::Color};

pub trait HasCanvas {
  fn present_canvas(&mut self);
  fn clear_canvas(&mut self, color: Color);
  fn clear_canvas_black(&mut self);
}

impl HasCanvas for Frontend {
  fn present_canvas(&mut self) {
    self.canvas.present();
  }

  fn clear_canvas(&mut self, color: Color) {
    self.canvas.set_draw_color(color);
    self.canvas.clear();
  }

  fn clear_canvas_black(&mut self) {
    self.clear_canvas(Color::RGB(0, 0, 0));
  }
}
