use {
  super::SyncViewer,
  crate::Server,
  eyro_entities::player::Player,
  eyro_repositories::Repository,
  eyro_values::{
    player_id::PlayerId,
    position::Positionable,
    size::{Size, Sizeable},
  },
  sdl2::{
    event::Event,
    keyboard::Keycode,
    pixels::Color,
    rect::Rect,
    render::Canvas,
    video::{Window, WindowBuildError},
    EventPump, IntegerOrSdlError,
  },
};

const DEBUG: bool = true;

pub struct PlayerSyncViewer {
  player_id: PlayerId,
  frame: u64,
  canvas: Canvas<Window>,
  event_pump: EventPump,
}

impl PlayerSyncViewer {
  #[allow(dead_code)]
  pub fn new(player_id: PlayerId) -> Result<Self, String> {
    let sdl_context = sdl2::init()?;
    let event_pump = sdl_context.event_pump()?;
    let video_subsystem = sdl_context.video()?;
    let window = video_subsystem
      .window("Game", 800, 600)
      .position_centered()
      .build()
      .map_err(|e: WindowBuildError| e.to_string())?;

    let canvas = window
      .into_canvas()
      .build()
      .map_err(|e: IntegerOrSdlError| e.to_string())?;

    Ok(Self {
      player_id,
      frame: 0,
      canvas,
      event_pump,
    })
  }

  fn clear_canvas_black(&mut self) {
    self.canvas.set_draw_color(Color::RGB(0, 0, 0));
    self.canvas.clear();
  }

  fn handle_event(event: Event) -> Option<()> {
    match &event {
      Event::MouseMotion { .. } => {}
      e => {
        if DEBUG {
          println!("{:?}", e);
        }
      }
    }
    match event {
      Event::Quit { .. }
      | Event::KeyDown {
        keycode: Some(Keycode::Escape),
        ..
      } => {
        return None;
      }
      _ => {}
    }
    Some(())
  }

  fn present_canvas(&mut self) {
    self.canvas.present();
  }

  fn handle_events(&mut self) -> Option<()> {
    for event in self.event_pump.poll_iter() {
      Self::handle_event(event)?
    }
    Some(())
  }
}

impl SyncViewer for PlayerSyncViewer {
  fn tick(&mut self) -> Option<()> {
    self.frame += 1;
    self.handle_events()
  }

  fn draw(&mut self, server: &Server) {
    self.clear_canvas_black();
    let player: &Player = server.players().find(self.player_id).unwrap();
    let players: Vec<&Player> = server.players().where_map(player.map_id());

    for player in players {
      let size: Size = player.size();

      self.canvas.set_draw_color(Color::RGB(0, 255, 0));
      self
        .canvas
        .fill_rect(Rect::new(
          player.position().x() as i32,
          player.position().y() as i32,
          size.width() as u32,
          size.height() as u32,
        ))
        .unwrap();
    }
    self.present_canvas();
  }
}
