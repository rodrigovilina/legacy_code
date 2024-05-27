use sdl2::{
  event::Event, pixels::Color, render::Canvas, video::Window, EventPump, Sdl,
};

pub struct Client {
  pub canvas: Canvas<Window>,
  pub event_pump: EventPump,
}

const GAME_WIDTH: u32 = 800;
const GAME_HEIGHT: u32 = 600;
const GAME_TITLE: &str = "Pong";
const WHITE: Color = Color { r: 255, g: 255, b: 255, a: 0xFF };

impl Client {
  pub fn new() -> Result<Client, String> {
    let sdl: Sdl = sdl2::init()?;
    let mut canvas: Canvas<Window> = sdl
      .video()?
      .window(GAME_TITLE, GAME_WIDTH as u32, GAME_HEIGHT as u32)
      .position_centered()
      .build()
      .map_err(|e| e.to_string())?
      .into_canvas()
      .present_vsync()
      .build()
      .map_err(|e| e.to_string())?;
    let event_pump: EventPump = sdl.event_pump()?;

    canvas.set_draw_color(WHITE);
    canvas.clear();
    canvas.present();

    Ok(Client { canvas, event_pump })
  }

  pub fn events(&mut self) -> Vec<Event> {
    self.event_pump.poll_iter().take_while(event_filter()).collect()
  }

  fn event_filter() -> impl FnMut(&Event) -> bool {
    |event| match event {
      Event::Quit { .. }
      | Event::KeyDown { keycode: Some(Keycode::Escape), .. } => false,
      _ => true,
    }
  }
}
