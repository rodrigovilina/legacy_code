use {
  super::{keyboard::Keyboard, mouse::Mouse},
  sdl2::{pixels::Color, render::Canvas, video::Window as SDLWindow, EventPump},
};

pub struct Window {
  pub canvas: Canvas<SDLWindow>,
  pub event_pump: EventPump,
  pub mouse: Mouse,
  pub keyboard: Keyboard,
  position: (i32, i32),
  size: (u32, u32),
  title: String,
}

impl Window {
  pub fn new() -> Result<Self, String> {
    let sdl_context = sdl2::init()?;
    let canvas: Canvas<sdl2::video::Window> = sdl_context
      .video()?
      .window("rust-sdl2 demo: Video", 800, 600)
      .position_centered()
      .resizable()
      .opengl()
      .build()
      .map_err(|e| e.to_string())?
      .into_canvas()
      .build()
      .map_err(|e| e.to_string())?;
    let event_pump: EventPump = sdl_context.event_pump()?;
    let mouse: Mouse = Mouse::new(&event_pump);
    let keyboard: Keyboard = Keyboard::new();
    let position: (i32, i32) = (0, 0);
    let size: (u32, u32) = (0, 0);
    let title: String = String::new();

    let window: Window = Window {
      canvas,
      event_pump,
      mouse,
      keyboard,
      position,
      size,
      title,
    };
    Ok(window)
  }

  pub fn set_draw_color(&mut self, color: Color) -> () {
    self.canvas.set_draw_color(color);
  }

  pub fn clear(&mut self) -> () {
    self.canvas.clear();
  }

  pub fn clear_white(&mut self) -> () {
    self.canvas.set_draw_color(Color::RGBA(255, 255, 255, 255));
    self.canvas.clear();
  }

  pub fn present(&mut self) -> () {
    self.canvas.present();
  }

  pub fn update_title(&mut self, frame: u128) -> Result<(), String> {
    let video_window: &mut SDLWindow = self.canvas.window_mut();
    self.position = video_window.position();
    self.size = video_window.size();
    let title: String = format!(
      "P({}, {}) S({}x{}) F({})",
      self.position.0, self.position.1, self.size.0, self.size.1, frame
    );
    self.title = title;

    video_window
      .set_title(&self.title)
      .map_err(|e| e.to_string())
  }
}
