use {
  super::Frontend,
  sdl2::{event::Event, keyboard::Keycode},
};

const DEBUG: bool = false;

impl Frontend {
  // return None to exit the main loop.
  pub fn handle_events(&mut self) -> Option<()> {
    for event in self.event_pump.poll_iter() {
      handle_event(event)?
    }
    Some(())
  }
}

// return None to exit the main loop.
fn handle_event(event: Event) -> Option<()> {
  match event {
    Event::Quit { .. }
    | Event::KeyDown {
      keycode: Some(Keycode::Escape),
      ..
    } => return None,
    // skip mouse motion intentionally because of the verbose it might cause.
    Event::MouseMotion { .. } => {}
    e => {
      if DEBUG {
        println!("{:?}", e);
      }
    }
  }
  Some(())
}
