use {
  crate::server::Server,
  eyro_entities::player::Player,
  eyro_packets::{
    clients_to_server_packets::ClientsToServerPackets, player_movement_packet::PlayerMovementPacket,
  },
  eyro_repositories::player::PlayerRepository,
  std::mem,
};

pub trait MovePlayers {
  fn move_players(&mut self, packets: &ClientsToServerPackets);
}

impl MovePlayers for Server {
  fn move_players(&mut self, packets: &ClientsToServerPackets) {
    update(self.players_mut(), packets);
  }
}

pub fn update(repo: &mut PlayerRepository, packets: &ClientsToServerPackets) {
  let players: Vec<Player> = mem::take::<Vec<Player>>(repo.players.as_mut())
    .into_iter()
    .map(|player: Player| {
      let player_movement: Option<&PlayerMovementPacket> = packets.get_player_movement(player.id());
      if let Some(player_movement) = player_movement {
        player.update(player_movement.acc())
      } else {
        player
      }
    })
    .collect();
  repo.players = players;
}
