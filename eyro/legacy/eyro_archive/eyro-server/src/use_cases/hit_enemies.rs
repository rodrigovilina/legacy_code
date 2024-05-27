use {
  crate::server::Server,
  eyro_entities::{bullet::Bullet, monster::Monster},
  eyro_repositories::Repository,
  eyro_values::{
    attack::HasAttack, current_hp::CurrentHp, hp_bar::HasHpBar, map_id::MapId, r#static::Spatial,
  },
};

impl Server {
  pub fn hit_enemies(&mut self) {
    for (bullet, monster) in self.collitions() {
      self.bullets_mut().destroy(bullet.id());
      let maybe_monster = self.monsters_mut().find_mut(monster.id());
      if let Some(val) = maybe_monster {
        val.damage(bullet.attack());
        if val.current_hp() == CurrentHp::new(0) {
          self.monsters_mut().kill(monster.id());
          self.monster_spawns_mut().kill(monster.id())
        }
      }
    }
  }

  fn collitions(&self) -> Vec<(Bullet, Monster)> {
    self
      .maps()
      .iter()
      .flat_map(|map| self.map_collitions(map.id()))
      .filter(|(bullet, monster)| bullet.collides_with(monster))
      .collect()
  }

  fn map_collitions(&self, map_id: MapId) -> Vec<(Bullet, Monster)> {
    let bullets: Vec<&Bullet> = self.bullets().where_map(map_id);
    let monsters: Vec<&Monster> = self.monsters().where_map(map_id);

    cartesian_product(bullets, monsters)
  }
}

fn cartesian_product(bullets: Vec<&Bullet>, monsters: Vec<&Monster>) -> Vec<(Bullet, Monster)> {
  bullets
    .into_iter()
    .flat_map(|bullet| {
      monsters
        .iter()
        .cloned()
        .map(move |monster| (bullet.clone(), monster.clone()))
    })
    .collect()
}
