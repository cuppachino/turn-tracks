use bevy::{ecs::system::Command, prelude::*};
use rand::prelude::*;

#[derive(Resource, Reflect, Deref, DerefMut)]
#[reflect(Resource, Default)]
pub struct GlobalRandom(#[reflect(ignore)] pub StdRng);

impl Default for GlobalRandom {
    fn default() -> Self {
        Self(StdRng::from_seed([1; 32]))
    }
}

pub struct GlobalRandomPlugin;

impl Plugin for GlobalRandomPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<GlobalRandom>();
    }
}
