use {
  crate::server::Server,
  eyro_entities::{bullet::Bullet, player::Player},
  eyro_packets::clients_to_server_packets::ClientsToServerPackets,
  eyro_repositories::Repository,
  eyro_values::{position::Positionable, velocity::Velocity},
};

pub trait CreateBullets {
  fn create_bullets(&mut self, packets: ClientsToServerPackets);
}

impl CreateBullets for Server {
  fn create_bullets(&mut self, packets: ClientsToServerPackets) {
    for bullet_packet in packets.player_bullets() {
      let player: &Player = self.players().find(bullet_packet.1.player_id()).unwrap();

      let direction_x: i64 = bullet_packet.1.x() as i64 - player.left();
      let direction_y: i64 = bullet_packet.1.y() as i64 - player.top();

      let direction: Velocity = Velocity::new(direction_x, direction_y).normalize();

      let bullet: Bullet = Bullet::new(
        self.bullets().next_id(),
        player.id(),
        player.map_id(),
        player.position(),
        direction,
        player.attack(),
      );

      self.bullets_mut().push(bullet);
    }
  }
}
