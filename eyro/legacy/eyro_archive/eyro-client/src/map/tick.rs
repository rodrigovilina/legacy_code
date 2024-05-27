use {super::MapClient, crate::tick::Tick};

impl Tick for MapClient {
  fn tick(&mut self) -> Option<()> {
    match self {
      MapClient::Disconnected { .. } => Some(()),
      MapClient::Connected {
        frame, event_pump, ..
      } => {
        *frame += 1;

        for event in event_pump.poll_iter() {
          Self::handle_event(event)?
        }

        Some(())
      }
    }
  }
}
