use {
  crate::server::Server, eyro_repositories::Repository, eyro_values::position::PositionableMut,
};

impl Server {
  pub fn move_bullets(&mut self) {
    for bullet in self.bullets_mut().iter_mut() {
      bullet.update_posistion();
    }
  }
}
