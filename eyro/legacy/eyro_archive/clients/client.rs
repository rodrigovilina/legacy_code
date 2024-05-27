use {
  super::SyncViewer,
  crate::{
    values::{Keyboard, Mouse},
    Server,
  },
  eyro_entities::{bullet::Bullet, monster::Monster, player::Player},
  eyro_packets::{
    player_bullet_packet::PlayerBulletPacket, player_movement_packet::PlayerMovementPacket, Packets,
  },
  eyro_repositories::Repository,
  eyro_values::{
    horizontal::Horizontal,
    hp_bar::HasHpBar,
    player_id::PlayerId,
    position::Positionable,
    size::{Size, Sizeable},
    vertical::Vertical,
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
  std::collections::HashMap,
};

const DEBUG: bool = false;

pub struct ClientSyncViewer {
  player_id: PlayerId,
  frame: u64,
  canvas: Canvas<Window>,
  event_pump: EventPump,
  keyboard: Keyboard,
  mouse: Mouse,
}

impl ClientSyncViewer {
  pub fn new(player_id: PlayerId) -> Result<Self, String> {
    let sdl_context: Sdl = sdl2::init()?;
    let event_pump: EventPump = sdl_context.event_pump()?;
    let video_subsystem: VideoSubsystem = sdl_context.video()?;
    let window: Window = video_subsystem
      .window("Game", 1400, 800)
      .position_centered()
      .build()
      .map_err(|e: WindowBuildError| e.to_string())?;

    let canvas: Canvas<Window> = window
      .into_canvas()
      .build()
      .map_err(|e: IntegerOrSdlError| e.to_string())?;

    let keyboard: Keyboard = Keyboard::new();
    let mouse: Mouse = Mouse::new();

    Ok(Self {
      player_id,
      frame: 0,
      canvas,
      event_pump,
      keyboard,
      mouse,
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

  fn clear_canvas_black(&mut self) {
    self.canvas.set_draw_color(Color::RGB(0, 0, 0));
    self.canvas.clear();
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

impl SyncViewer for ClientSyncViewer {
  fn tick(&mut self) -> Option<()> {
    self.frame += 1;
    self.handle_events()?;
    self.mouse.update(&self.event_pump);
    self.keyboard.update(&self.event_pump);
    Some(())
  }

  fn draw(&mut self, server: &Server) {
    self.clear_canvas_black();

    let map = server
      .maps()
      .find(server.players().find(self.player_id).unwrap().map_id())
      .unwrap();

    self.canvas.set_draw_color(Color::RGB(255, 255, 255));
    self
      .canvas
      .draw_rect(Rect::new(
        0,
        0,
        map.size().width() as u32,
        map.size().height() as u32,
      ))
      .unwrap();

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

    let monsters: Vec<&Monster> = server.monsters().where_map(player.map_id());

    for monster in monsters {
      let size: Size = monster.size();

      self.canvas.set_draw_color(Color::RGB(255, 0, 0));
      self
        .canvas
        .fill_rect(Rect::new(
          monster.position().x() as i32,
          monster.position().y() as i32,
          size.width() as u32,
          size.height() as u32,
        ))
        .unwrap();

      // draw hp bar above monster, as a rectangle composed of two rectangles
      // the first with green for current hp
      // and the second with red for missing hp
      self.canvas.set_draw_color(Color::RGB(0, 200, 100));
      self
        .canvas
        .fill_rect(Rect::new(
          monster.position().x() as i32,
          monster.position().y() as i32 - 20,
          (size.width() as f64 * monster.current_hp_ratio()) as u32,
          10,
        ))
        .unwrap();
      self.canvas.set_draw_color(Color::RGB(200, 0, 100));
      let var_name = size.width() as f64 * monster.current_hp_ratio();
      self
        .canvas
        .fill_rect(Rect::new(
          var_name as i32 + monster.position().x() as i32,
          monster.position().y() as i32 - 20,
          size.width() as u32 - var_name as u32,
          10,
        ))
        .unwrap();
    }

    // draw mouse
    self.canvas.set_draw_color(Color::RGB(0, 0, 255));
    self
      .canvas
      .fill_rect(Rect::new(self.mouse.x() - 5, self.mouse.y() - 5, 10, 10))
      .unwrap();

    // draw bullets
    let bullets: Vec<&Bullet> = server.bullets().where_map(player.map_id());
    for bullet in bullets {
      let size: Size = Size::new(10, 10);

      self.canvas.set_draw_color(Color::RGB(0, 0, 255));
      self
        .canvas
        .fill_rect(Rect::new(
          bullet.position().x() as i32,
          bullet.position().y() as i32,
          size.width() as u32,
          size.height() as u32,
        ))
        .unwrap();
    }

    // draw line between player and mouse
    let player: &Player = server.players().find(self.player_id).unwrap();
    self.canvas.set_draw_color(Color::RGB(255, 255, 255));
    self
      .canvas
      .draw_line(
        (player.position().x() as i32, player.position().y() as i32),
        (self.mouse.x(), self.mouse.y()),
      )
      .unwrap();
    self.present_canvas();
  }

  fn events(&self) -> Packets {
    let horizontal: Horizontal = match (
      self.keyboard.pressed_keys.contains(&Keycode::A),
      self.keyboard.pressed_keys.contains(&Keycode::D),
    ) {
      (true, true) => Horizontal::None,
      (true, false) => Horizontal::Left,
      (false, true) => Horizontal::Right,
      (false, false) => Horizontal::None,
    };
    let vertical: Vertical = match (
      self.keyboard.pressed_keys.contains(&Keycode::W),
      self.keyboard.pressed_keys.contains(&Keycode::S),
    ) {
      (true, true) => Vertical::None,
      (true, false) => Vertical::Up,
      (false, true) => Vertical::Down,
      (false, false) => Vertical::None,
    };
    let player_movement_input: PlayerMovementPacket =
      PlayerMovementPacket::new(horizontal, vertical);

    let player_movements: HashMap<u64, PlayerMovementPacket> =
      HashMap::from([(self.player_id.into(), player_movement_input)]);

    let player_bullets: Vec<PlayerBulletPacket> = if self.mouse.released_left() {
      vec![PlayerBulletPacket::new(
        self.mouse.x() as usize,
        self.mouse.y() as usize,
        self.player_id,
      )]
    } else {
      vec![]
    };

    Packets::new(player_movements, player_bullets)
  }
}
