use {
  eyro_values::size::Size,
  sdl2::{pixels::Color, render::Canvas, video::Window, EventPump, Sdl, VideoSubsystem},
};

pub fn canvas_and_event_pump(size: Size) -> (Canvas<Window>, EventPump) {
  let sdl_context: Sdl = sdl2::init().unwrap();
  let event_pump: EventPump = sdl_context.event_pump().unwrap();
  let video_subsystem: VideoSubsystem = sdl_context.video().unwrap();
  let window: Window = video_subsystem
    .window("Eyro Map Client", size.width() as u32, size.height() as u32)
    .position_centered()
    .resizable()
    .build()
    .unwrap();
  let mut canvas: Canvas<Window> = window.into_canvas().build().unwrap();

  canvas.set_draw_color(Color::RGB(0, 0, 0));
  canvas.clear();
  canvas.present();

  (canvas, event_pump)
}
