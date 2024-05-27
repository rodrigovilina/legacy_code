pub mod draw;
mod frame_counter;
mod handle_events;
pub mod has_canvas;
pub mod keyboard;
pub mod sleep;

use {
  self::keyboard::Keyboard,
  sdl2::{
    pixels::Color,
    render::Canvas,
    video::{Window, WindowBuildError},
    EventPump, IntegerOrSdlError, Sdl, VideoSubsystem,
  },
};

pub struct Frontend {
  canvas: Canvas<Window>,
  pub event_pump: EventPump,
  frame: usize,
  keyboard: Keyboard,
}

impl Frontend {
  pub fn new() -> Result<Self, String> {
    let sdl_context: Sdl = sdl2::init()?;
    let video_subsystem: VideoSubsystem = sdl_context.video()?;
    let window: Window = video_subsystem
      .window("rust-sdl2 demo: Events", 800, 600)
      .position_centered()
      .resizable()
      .build()
      .map_err(|e: WindowBuildError| e.to_string())?;

    let mut canvas: Canvas<Window> = window
      .into_canvas()
      .build()
      .map_err(|e: IntegerOrSdlError| e.to_string())?;

    canvas.set_draw_color(Color::RGB(255, 0, 0));
    canvas.clear();
    canvas.present();

    let event_pump: EventPump = sdl_context.event_pump()?;

    Ok(Self {
      canvas,
      event_pump,
      frame: 0,
      keyboard: Keyboard::new(),
    })
  }
}
