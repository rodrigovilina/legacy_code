use std::{time::Duration, thread::sleep};
use crate::{client::Client, server::Server};
use sdl2::{event::Event, keyboard::Keycode};

pub fn game_loop(mut client: Client, mut server: Server) -> Result<(), String> {
  print!("\x1B[2J\x1B[1;1H");
  println!("server: {:#?}", server);
  if server.exit_game() {
    return Ok(());
  }
  let events: Vec<Event> = client.events();
  server = server.process_events(events);
  if server.exit_game() {
    return Ok(());
  }
  //  game = game.next();
  if server.exit_game() {
    return Ok(());
  }
  //  draw_game(&mut client, game.clone())?;
  //  client.canvas.present();
  sleep(Duration::from_millis(100));
  game_loop(client, server)
}
