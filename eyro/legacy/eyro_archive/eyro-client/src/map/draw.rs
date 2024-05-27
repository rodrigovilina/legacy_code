use {
  super::MapClient,
  crate::draw::Draw,
  eyro_values::{
    position::Positionable,
    size::{Size, Sizeable},
  },
  sdl2::{pixels::Color, rect::Rect},
};

impl Draw for MapClient {
  fn draw(&mut self) {
    match self {
      MapClient::Disconnected { .. } => {}
      MapClient::Connected {
        map_id,
        map_size,
        canvas,
        players,
        ..
      } => {
        canvas.set_draw_color(Color::RGB(0, 0, 0));
        canvas.clear();

        let size: Size = *map_size;

        canvas.set_draw_color(Color::RGB(0, 0, 255));
        canvas
          .draw_rect(Rect::new(0, 0, size.width() as u32, size.height() as u32))
          .unwrap();

        for player in players.where_map(*map_id) {
          let size: Size = player.size();

          canvas.set_draw_color(Color::RGB(0, 255, 0));
          canvas
            .fill_rect(Rect::new(
              player.position().x() as i32,
              player.position().y() as i32,
              size.width() as u32,
              size.height() as u32,
            ))
            .unwrap();
        }

        canvas.present();
      }
    }
  }
}
