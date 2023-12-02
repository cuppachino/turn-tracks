use bevy::{
    core_pipeline::experimental::taa::TemporalAntiAliasPlugin,
    pbr::wireframe::{WireframeConfig, WireframePlugin},
    prelude::*,
    render::{
        settings::{RenderCreation, WgpuFeatures, WgpuSettings},
        RenderPlugin,
    },
};
use bevy_inspector_egui::quick::WorldInspectorPlugin;

mod gameflow;
mod global_random;
mod target_rules;
mod util;

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins.set(RenderPlugin {
                render_creation: RenderCreation::Automatic(WgpuSettings {
                    // WARN this is a native only feature. It will not work with webgl or webgpu
                    features: WgpuFeatures::POLYGON_MODE_LINE | WgpuFeatures::PUSH_CONSTANTS,
                    ..default()
                }),
            }),
            WireframePlugin,
            TemporalAntiAliasPlugin,
        ))
        .insert_resource(WireframeConfig {
            global: true,
            default_color: Color::WHITE,
        })
        .insert_resource(Msaa::Sample4)
        .add_plugins((util::FpsPlugin, WorldInspectorPlugin::default()))
        .add_plugins(global_random::GlobalRandomPlugin)
        .add_plugins(gameflow::GameFlowPlugin)
        .run();
}
