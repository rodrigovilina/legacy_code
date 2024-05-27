mod canvas_and_event_pump;
mod draw;
mod packets;
mod receive_packets;
mod tick;

use {
  eyro_repositories::player::PlayerRepository,
  eyro_values::{map_id::MapId, size::Size},
  sdl2::{event::Event, keyboard::Keycode, render::Canvas, video::Window, EventPump},
};

const DEBUG: bool = true;

pub enum MapClient {
  Disconnected {
    map_id: MapId,
  },
  Connected {
    map_id: MapId,
    map_size: Size,
    frame: u64,
    canvas: Canvas<Window>,
    event_pump: EventPump,
    players: PlayerRepository,
  },
}

impl MapClient {
  pub fn disconnected(map_id: MapId) -> Self {
    Self::Disconnected { map_id }
  }

  pub fn connected(
    map_id: MapId,
    map_size: Size,
    players: PlayerRepository,
  ) -> Result<Self, String> {
    let (canvas, event_pump) = canvas_and_event_pump::canvas_and_event_pump(map_size);

    Ok(Self::Connected {
      map_id,
      frame: 0,
      canvas,
      event_pump,
      map_size,
      players,
    })
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
}
