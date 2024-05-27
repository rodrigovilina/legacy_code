use {
  super::SyncViewer,
  eyro_values::{
    map_id::MapId,
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
    EventPump, IntegerOrSdlError, Sdl, VideoSubsystem,
  },
};

const DEBUG: bool = true;

pub struct MapsSyncViewer {
  frame: u64,
  event_pump: EventPump,
  map_viewers: Vec<MapViewer>,
}

pub struct MapViewer {
  map_id: MapId,
  canvas: Canvas<Window>,
}

impl MapViewer {
  #[allow(dead_code)]
  fn new(map_id: MapId, video_subsystem: &VideoSubsystem) -> Self {
    let window: Window = video_subsystem
      .window("rust-sdl2 demo: Events", 800, 600)
      .position_centered()
      .resizable()
      .build()
      .map_err(|e: WindowBuildError| e.to_string())
      .unwrap();

    let mut canvas: Canvas<Window> = window
      .into_canvas()
      .build()
      .map_err(|e: IntegerOrSdlError| e.to_string())
      .unwrap();

    canvas.set_draw_color(Color::RGB(255, 0, 0));
    canvas.clear();
    canvas.present();

    Self { map_id, canvas }
  }
}

impl MapsSyncViewer {
  #[allow(dead_code)]
  pub fn new(map_ids: Vec<MapId>) -> Self {
    let sdl_context: Sdl = sdl2::init().unwrap();
    let event_pump: EventPump = sdl_context.event_pump().unwrap();
    let video_subsystem: VideoSubsystem = sdl_context.video().unwrap();

    let map_viewers: Vec<MapViewer> = map_ids
      .into_iter()
      .map(|map_id| MapViewer::new(map_id, &video_subsystem))
      .collect();

    Self {
      frame: 0,
      event_pump,
      map_viewers,
    }
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

  fn clear_canvas_black(&mut self) {
    for viewer in self.map_viewers.iter_mut() {
      viewer.canvas.set_draw_color(Color::RGB(0, 0, 0));
      viewer.canvas.clear();
    }
  }

  fn present_canvas(&mut self) {
    for viewer in self.map_viewers.iter_mut() {
      viewer.canvas.present();
    }
  }
}

impl SyncViewer for MapsSyncViewer {
  fn tick(&mut self) -> Option<()> {
    self.frame += 1;

    for event in self.event_pump.poll_iter() {
      Self::handle_event(event)?
    }

    Some(())
  }

  fn draw(&mut self, server: &crate::Server) {
    self.clear_canvas_black();
    for viewer in self.map_viewers.iter_mut() {
      for player in server.players().where_map(viewer.map_id) {
        let size: Size = player.size();

        viewer.canvas.set_draw_color(Color::RGB(0, 255, 0));
        viewer
          .canvas
          .fill_rect(Rect::new(
            player.position().x() as i32,
            player.position().y() as i32,
            size.width() as u32,
            size.height() as u32,
          ))
          .unwrap();
      }
    }
    self.present_canvas();
  }
}
