use {
  super::{
    backend::{game_input::GameInput, screen::Screen},
    Client,
  },
  crate::client::frontend::{
    draw::Draw, has_canvas::HasCanvas, keyboard::has_keyboard::HasKeyboard, sleep::Sleep,
  },
};

impl Client {
  pub fn main_loop(&mut self) -> Option<()> {
    #[allow(unused_assignments)]
    let mut screen: Screen = self.back.present();

    loop {
      self.front.increase_frame_counter();
      self.front.handle_events()?;
      self.front.update_keyboard();
      self.tick_backend();
      screen = self.back.present();
      self.front.clear_canvas_black();
      screen.draw(&mut self.front);
      self.front.present_canvas();
      self.front.sleep();
    }
  }

  fn tick_backend(&mut self) {
    let game_input: GameInput = self.front.present_keyboard();
    self.back = self.back.clone().tick(game_input);
  }
}
