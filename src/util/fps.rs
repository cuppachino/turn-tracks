use bevy::{
    diagnostic::{DiagnosticsStore, FrameTimeDiagnosticsPlugin},
    prelude::*,
};

#[derive(Component)]
pub struct FpsText;

pub fn setup_ui(mut commands: Commands) {
    commands
        .spawn((
            NodeBundle {
                style: Style {
                    position_type: PositionType::Absolute,
                    right: Val::Px(10.0),
                    top: Val::Px(10.0),
                    ..default()
                },
                ..default()
            },
            Name::new("__fps_display"),
        ))
        .with_children(|build| {
            let font_size = 20.0;
            build.spawn((
                FpsText,
                TextBundle::from_sections([
                    TextSection::new(
                        "FPS: ",
                        TextStyle {
                            font_size,
                            color: Color::GOLD.with_a(0.5),
                            ..default()
                        },
                    ),
                    TextSection::from_style(TextStyle {
                        font_size,
                        color: Color::GOLD.with_a(0.8),
                        ..default()
                    }),
                ]),
            ));
        });
}

pub fn update_fps_text(
    diagnostics: Res<DiagnosticsStore>,
    mut query: Query<&mut Text, With<FpsText>>,
) {
    for mut text in &mut query {
        if let Some(fps) = diagnostics.get(FrameTimeDiagnosticsPlugin::FPS) {
            if let Some(value) = fps.smoothed() {
                // Update the value of the second section
                text.sections[1].value = format!("{value:.2}");
            }
        }
    }
}

pub struct FpsPlugin;

impl Plugin for FpsPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(FrameTimeDiagnosticsPlugin::default())
            .add_systems(Startup, setup_ui)
            .add_systems(Update, update_fps_text);
    }
}
