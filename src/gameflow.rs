#![allow(dead_code)]

use bevy::{
    ecs::query::{QueryIter, ReadOnlyWorldQuery, WorldQuery},
    prelude::*,
    ui::FocusPolicy,
    utils::{StableHashMap, StableHashSet},
};

pub trait Queryable: Send + Sync + 'static {}
impl<T> Queryable for T where T: Send + Sync + 'static {}

#[derive(Clone, Eq, PartialEq, Debug, Reflect)]
pub struct PhasePermissions {
    /// If true, the player can "go back" to the previous phase.
    pub cancellable: bool,
}

impl Default for PhasePermissions {
    fn default() -> Self {
        Self { cancellable: false }
    }
}

#[derive(States, Clone, Eq, PartialEq, Debug, Reflect)]
pub enum ActionPhase {
    ChooseAction(PhasePermissions),
    ChooseTargets(PhasePermissions),
    ChooseLane(PhasePermissions),
}

impl std::hash::Hash for ActionPhase {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::ChooseAction(_) => "ChooseAction".hash(state),
            Self::ChooseTargets(_) => "ChooseTarget".hash(state),
            Self::ChooseLane(_) => "ChooseLane".hash(state),
        }
    }
}

impl Default for ActionPhase {
    fn default() -> Self {
        Self::ChooseAction(PhasePermissions::default())
    }
}

/// Each entity gets a turn phase.
#[derive(States, Clone, Eq, PartialEq, Debug, Reflect)]
pub enum TurnPhase {
    /// - Consume buffs/debuff tokens, if any.
    /// - Play animations, write to dialogue, etc.
    Start,
    /// Entities choose what they want to do.
    Action(ActionPhase),
    /// Push choices to the appropriate lane.
    End,
}

impl std::hash::Hash for TurnPhase {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Start => "Start".hash(state),
            Self::Action(action) => action.hash(state),
            Self::End => "End".hash(state),
        }
    }
}

impl Default for TurnPhase {
    fn default() -> Self {
        Self::Start
    }
}

#[derive(States, Clone, Eq, PartialEq, Debug, Default, Reflect)]
pub enum EnactLoop {
    /// Progress all lanes. Determine which entity has initiative, and progress them to their turn.
    /// If all entities have completed their turn, transition from [`GameFlow::Enact`] -> [`GameFlow::End`].
    #[default]
    Eval,
    /// Let the entity take their turn.
    Turn(TurnPhase),
    /// Consume actions, applying any effects, in order.
    Act,
    /// Cleanup all lanes, removing dead entities, etc.
    Cleanup,
}

impl std::hash::Hash for EnactLoop {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Eval => "Eval".hash(state),
            Self::Turn(turn) => turn.hash(state),
            Self::Act => "Act".hash(state),
            Self::Cleanup => "Cleanup".hash(state),
        }
    }
}

#[derive(States, Clone, Eq, PartialEq, Debug, Hash, Default, Reflect)]
pub enum GameFlow {
    /// The beginning of the game. Initialize all entities / board.
    Enter,
    /// Play animations, write to dialogue, etc.
    ///
    /// E.g. "ROUND 1"
    #[default]
    Start,

    /// Wait for each entity to complete one turn (or die).
    Enact(EnactLoop),

    /// Play animations, write to dialogue, etc.
    /// E.g. "ROUND RATING: S+"
    /// E.g. "Unit leveled up!"
    End,

    /// Check win conditions.
    Eval,

    /// The game is over. Do whatever with the results.
    Exit,
}

// ---- Components ----

/// This component is used to determine which entity needs to take their turn.
#[derive(Component, Debug, Reflect)]
#[component(storage = "SparseSet")]
pub struct Initiative;

/// Marks an entities for extraction at the next `GameFlow::Enter`.
#[derive(Component, Reflect)]
pub struct Combatant;

/// Marks which party a combatant belongs to.
#[derive(Component, Debug, Hash, Eq, PartialEq, Reflect, Clone, Copy)]
pub enum Party {
    Ally,
    Enemy,
}

/// Mark an entity as a Player.
#[derive(Component, Reflect, Default)]
pub struct Player;

/// Marks an entity as a CPU enemy of the player.
#[derive(Component, Reflect, Default)]
pub struct Enemy;

/// If present, this determines the order in which entities are
/// spawned on the board and the order in which party members
/// replace their dead comrades.
///
/// Skip adding this to an entity to have "random" encounters.
#[derive(Component, Debug, Hash, Eq, PartialEq, Ord, PartialOrd, Deref, DerefMut, Reflect)]
pub struct PartyPosition(pub usize);

impl From<usize> for PartyPosition {
    fn from(index: usize) -> Self {
        Self(index)
    }
}

#[derive(Component, Debug, Reflect)]
pub struct GameRoundUiNode;

/// Init the UI for the game round.
fn spawn_round_ui(mut commands: Commands) {
    commands
        .spawn(NodeBundle {
            style: Style {
                position_type: PositionType::Absolute,
                align_self: AlignSelf::Center,
                justify_self: JustifySelf::Center,
                top: Val::Px(0.0),
                left: Val::Px(0.0),
                ..default()
            },
            focus_policy: FocusPolicy::Pass,
            ..default()
        })
        .with_children(|parent| {
            parent.spawn((
                GameRoundUiNode,
                TextBundle::from_sections(vec![TextSection {
                    value: "ROUND".to_string(),
                    style: TextStyle {
                        color: Color::YELLOW,
                        ..default()
                    },
                }]),
            ));
        });
}

// ---- Resources ----

/// One round = one turn for each entity.
/// Only after ALL entities have completed their turn, may the round progress.
#[derive(Resource, Deref, DerefMut, Eq, PartialEq, PartialOrd, Ord, Debug, Reflect)]
pub struct GameRound(pub u32);

impl Default for GameRound {
    fn default() -> Self {
        Self(1)
    }
}

impl GameRound {
    pub fn increment(&mut self) {
        self.0 += 1;
    }

    pub fn reset(&mut self) {
        self.0 = 0;
    }
}

#[derive(Resource, Debug, Default, Reflect)]
pub enum WinCondition {
    /// A party must defeat all rivals.
    ///
    /// This is the default condition.
    #[default]
    Clear,
    /// A party must survive for a certain number of rounds.
    Survive(u32),
}

#[derive(Debug)]
pub enum WinConditions {
    /// Only a single condition must be met to win.
    ///
    /// This is the default condition.
    Single(WinCondition),
    /// The first met condition wins.
    Any(Vec<WinCondition>),
    /// All conditions must be met to win.
    All(Vec<WinCondition>),
}

impl Default for WinConditions {
    fn default() -> Self {
        Self::Single(WinCondition::Clear)
    }
}

#[derive(Resource, Debug, Default, Reflect, Deref, DerefMut)]
pub struct GameFlowWinConditions {
    #[deref]
    #[reflect(ignore)]
    pub party_conditions: StableHashMap<Party, WinConditions>,
}

/// Entities extracted from the world at [`GameFlow::Enter`].
#[derive(Resource, Deref, DerefMut, Debug, Default, Reflect)]
pub struct GameFlowCombatants {
    /// [`Combatant`] entities sorted into [`Party`] buckets and ordered by [`PartyPosition`].
    #[deref]
    #[reflect(ignore)]
    pub parties: StableHashMap<Party, Vec<Entity>>,
}

impl GameFlowCombatants {
    /// Load entities from a query into `self`.
    pub fn load_from_query<'s, 'w, F: ReadOnlyWorldQuery>(
        &mut self,
        iter: QueryIter<'s, 'w, (Entity, &Party, Option<&PartyPosition>), F>,
    ) {
        for (entity, party, position) in iter {
            let party = self.parties.entry(*party).or_default();
            if let Some(index) = position {
                party.insert(**index, entity)
            } else {
                party.push(entity)
            }
        }
    }
}

// ----

fn register_types(app: &mut App) {
    app.register_type::<Player>().register_type::<Enemy>();

    app.register_type::<Initiative>()
        .register_type::<Combatant>()
        .register_type::<Party>()
        .register_type::<PartyPosition>()
        .register_type::<GameRound>()
        .register_type::<GameRoundUiNode>()
        .register_type::<GameFlowCombatants>()
        .register_type::<GameFlow>()
        .register_type::<EnactLoop>()
        .register_type::<TurnPhase>()
        .register_type::<PhasePermissions>()
        .register_type::<ActionPhase>();
}

// ----

use crate::target_rules::{Rule, TargetRule};

#[derive(Debug, Reflect, Deref, DerefMut)]
pub struct Contained<T> {
    #[deref]
    pub value: T,
    pub max: T,
}

impl<T> Contained<T> {
    pub fn new(value: T, max: T) -> Self {
        Self { value, max }
    }

    pub fn new_empty(max: T) -> Self
    where
        T: Default,
    {
        Self::new(T::default(), max)
    }

    pub fn new_full(value: T) -> Self
    where
        T: Copy,
    {
        Self::new(value, value)
    }
}

impl<T: core::cmp::Ord + Copy> Contained<T> {
    pub fn new_clamped(value: T, max: T) -> Self {
        let value = value.min(max);
        Self { value, max }
    }
}

/// Mitigates damage before health.
#[derive(Component, Debug, Reflect, Deref, DerefMut)]
pub struct Shield(f32);

/// Mitigates physical damage after shield.
#[derive(Component, Debug, Reflect, Deref, DerefMut)]
pub struct Defense(f32);

/// Mitigates magical damage after shield.
#[derive(Component, Debug, Reflect, Deref, DerefMut)]
pub struct Resistance(f32);

/// Once depleted, the unit is considered "dead".
#[derive(Component, Debug, Reflect, Deref, DerefMut)]
pub struct Health(pub Contained<f32>);
impl Health {
    pub fn new(max: f32) -> Self {
        Self(Contained::new_full(max))
    }
}

/// The casting speed of abilities.
#[derive(Component, Debug, Reflect, Deref, DerefMut)]
pub struct Speed(f32);

/// Earned resource used to cast better abilities.
#[derive(Component, Debug, Reflect, Deref, DerefMut)]
pub struct Focus(pub Contained<u32>);
impl Focus {
    pub fn new(max: u32) -> Self {
        Self(Contained::new_empty(max))
    }
}

/// Secondary unit resource required to cast arcane abilities.
#[derive(Component, Debug, Reflect, Deref, DerefMut)]
pub struct Mana(pub Contained<u32>);
impl Mana {
    pub fn new(value: u32, max: u32) -> Self {
        Self(Contained::new(value, max))
    }
}

/// The strength of magical abilities.
#[derive(Component, Debug, Reflect, Deref, DerefMut)]
pub struct Arcane(f32);

/// The strength of physical abilities.
#[derive(Component, Debug, Reflect, Deref, DerefMut)]
pub struct Power(f32);

#[derive(Debug)]
pub enum Defensive {
    /// Consumed before health.
    Shield(f32),
    /// Mitigates physical damage after shield.
    Defense(f32),
    /// Mitigates magical damage after shield.
    Resistance(f32),
    /// Consumed after shield and mitigated by defense.
    Health(f32),
}

#[derive(Debug)]
pub enum Offensive {
    /// Strength of magical abilities.
    Arcane(f32),
    /// Strength of physical abilities.
    Power(f32),
    /// The casting speed of abilities.
    Speed(f32),
}

#[derive(Debug)]
pub enum Stat {
    Offense(Offensive),
    Defense(Defensive),
    Focus(f32),
    Speed(f32),
}

#[derive(Debug)]
pub struct StatusEffectMeta {
    pub name: String,
    pub description: String,
}

#[derive(Debug)]
pub enum StatusEffect {
    Debuff {
        meta: StatusEffectMeta,
        stats: Vec<Stat>,
        turns_remaining: u32,
    },
    Buff {
        meta: StatusEffectMeta,
        stats: Vec<Stat>,
        stacks: u32,
        max_stacks: u32,
    },
}

/// A collection of patches to apply to the target and the source caster.
#[derive(Debug)]
pub struct Effect {
    pub target: Entity,
    /// Effects applied to the target.
    ///
    /// Effects are applied negatively to the target. For example,
    /// if sending a `Stat::Offense(Offense::Power(10.0))` to the
    /// target, the target will lose 10 power.
    pub target_stat_effects: Vec<Stat>,
    /// Status effects applied to the target.
    pub target_status_effects: Vec<StatusEffect>,

    /// Effects applied to the caster.
    ///
    /// Effects are applied positively to the caster. For example,
    /// if sending a `Stat::Offense(Offense::Power(10.0))` to the
    /// caster, the caster will gain 10 power.
    pub source_stat_effects: Vec<Stat>,
    /// Status effects applied to the caster.
    pub source_status_effects: Vec<StatusEffect>,
}

impl Default for Effect {
    fn default() -> Self {
        Self {
            target: Entity::PLACEHOLDER,
            ..Default::default()
        }
    }
}

pub struct GameFlowApi<'a> {
    pub ally_targets: &'a mut Vec<Entity>,
    pub rival_targets: &'a mut Vec<Entity>,
    pub ally_non_targets: &'a mut Vec<Entity>,
    pub rival_non_targets: &'a mut Vec<Entity>,
}

#[derive(Debug)]
pub struct AbilityMeta {
    pub name: String,
    pub description: String,
    pub target_rule: TargetRule,
    pub cost: Option<Vec<Stat>>,
    pub effect: fn(api: &GameFlowApi) -> Vec<Effect>,
}

impl AbilityMeta {
    pub fn is_castable(&self, api: &GameFlowApi) -> bool {
        todo!()
    }
}

pub enum Ability {
    Instant(AbilityMeta),
    Lane(AbilityMeta),
    Ultimate(AbilityMeta),
}

#[derive(Component, Debug, Deref, DerefMut)]
pub struct InstantAbilities(pub Vec<AbilityMeta>);

#[derive(Component, Debug, Deref, DerefMut)]
pub struct LaneAbilities(pub Vec<AbilityMeta>);

#[derive(Component, Debug, Deref, DerefMut)]
pub struct UltimateAbilities(pub Vec<AbilityMeta>);

#[derive(Bundle)]
pub struct AbilityBundle {
    pub instant: InstantAbilities,
    pub lane: LaneAbilities,
    pub ultimate: UltimateAbilities,
}

#[derive(Bundle)]
pub struct StatBundle {
    pub shield: Shield,
    pub health: Health,

    pub defense: Defense,
    pub power: Power,

    pub resistance: Resistance,
    pub arcane: Arcane,
    pub mana: Mana,

    pub focus: Focus,
    pub speed: Speed,
}

impl Default for StatBundle {
    fn default() -> Self {
        Self {
            shield: Shield(0.),
            defense: Defense(0.),
            resistance: Resistance(0.),
            health: Health::new(60.),
            speed: Speed(1.),
            focus: Focus::new(25),
            mana: Mana::new(20, 40),
            arcane: Arcane(0.),
            power: Power(0.),
        }
    }
}

#[derive(Bundle)]
pub struct ActorBundle {
    party: Party,
    abilities: AbilityBundle,
    stats: StatBundle,
}

impl ActorBundle {
    pub fn new(party: Party, stats: StatBundle, abilities: AbilityBundle) -> Self {
        Self {
            party,
            stats,
            abilities,
        }
    }

    /// Return a new bundle of `self` and `position`.
    pub fn at_party_position(self, position: impl Into<PartyPosition>) -> impl Bundle {
        (self, position.into())
    }
}

use rand::seq::SliceRandom;

fn build_hyomoto_abilities() -> AbilityBundle {
    AbilityBundle {
        instant: InstantAbilities(vec![AbilityMeta {
            name: "Quick Attack".to_string(),
            // todo: {STAT} -> colorize the stat name.
            // todo: {:STAT:} -> replace with the stat value and colorize.
            // todo: {S:STAT} -> colorize S with the stat color.
            // todo: {STAT:N} -> multiply N by the stat value and colorize.
            description: "Deal {:power:} to a target and gain {5:focus}.".to_string(),
            cost: None,
            target_rule: Rule::TargetEnemies {
                max: Some(1),
                force_all: false,
            } + Rule::TargetAlive,

            effect: |api| {
                #[cfg(debug_assertions)]
                {
                    // Ensure that the target rule is enforced.
                    assert_eq!(api.rival_targets.len(), 1);
                }

                let single_target = *api.rival_targets.first().unwrap();

                // todo: impl world commands for GameFlowApi to get the caster's power, for now assume its 10.
                let source_power = 10.0;

                vec![Effect {
                    target: single_target,
                    target_stat_effects: vec![Stat::Offense(Offensive::Power(source_power))],
                    source_stat_effects: vec![Stat::Focus(5.0)],
                    ..default()
                }]
            },
        }]),
        lane: LaneAbilities(vec![AbilityMeta {
            name: "Fireball".to_string(),
            description: "Deal {:arcane:} to an enemy. If {focus} is half full, 50% chance of bouncing to another enemy.".to_string(),
            cost: Some(vec![Stat::Focus(10.0)]),

                target_rule: Rule::TargetEnemies { max: Some(1), force_all: false } + Rule::TargetAlive,
            effect: |api| {
                #[cfg(debug_assertions)]
                {
                    // Ensure that the target rule is enforced.
                    assert_eq!(api.rival_targets.len(), 1);
                }

                let main_target = *api.rival_targets.first().unwrap();


                // todo: impl world commands for GameFlowApi to get the caster's `Arcane`. For now just assume its 20.
                let source_arcane = 20.0;

                let mut effects = vec![
                    Effect {
                        target: main_target,
                        target_stat_effects: vec![Stat::Offense(Offensive::Arcane(source_arcane))],
                        ..default()
                    }
                ];

                // todo: impl world commands for GameFlowApi to get the caster's `Focus`. For now just assume its above 50%.
                let above_half_focus = true;

                if above_half_focus {
                    if let Some(bounce_effect) = api.rival_non_targets.choose(&mut rand::thread_rng()).map(|target| Effect {
                        target: *target,
                        target_stat_effects: vec![Stat::Offense(Offensive::Arcane(source_arcane))],
                        ..default()
                    }) {
                        effects.push(bounce_effect);
                    }
                }

                effects
            },

        }]),
        ultimate: UltimateAbilities(vec![]),
    }
}

fn spawn_demo_actors(mut commands: Commands) {
    let hyomoto_abilities = build_hyomoto_abilities();

    commands.spawn((
        Name::new("Hyomoto"),
        ActorBundle::new(Party::Ally, StatBundle::default(), hyomoto_abilities)
            .at_party_position(0),
    ));
}

fn init_gameflow(mut commands: Commands, mut gameflow: ResMut<NextState<GameFlow>>) {
    commands.insert_resource(GameRound::default());
    gameflow.0 = Some(GameFlow::Enter);
}

// ----

pub struct GameFlowPlugin;

impl Plugin for GameFlowPlugin {
    fn build(&self, app: &mut App) {
        register_types(app);
        app.init_resource::<GameRound>()
            .add_state::<GameFlow>()
            .add_systems(OnEnter(GameFlow::Enter), enter_combat_world)
            .add_systems(OnEnter(GameFlow::Start), start_next_round)
            .add_systems(OnEnter(GameFlow::Exit), exit_combat_world);
    }
}

///
fn enter_combat_world(
    mut combatants: ResMut<GameFlowCombatants>,
    query: Query<(Entity, &Party, Option<&PartyPosition>), With<Combatant>>,
) {
    combatants.load_from_query(query.into_iter())
}

///
fn start_next_round(mut gameflow: ResMut<NextState<GameFlow>>, round: Res<GameRound>) {
    gameflow.0 = Some(GameFlow::Enact(EnactLoop::Eval));
}

/// @sys.cleanup
fn exit_combat_world(mut commands: Commands) {}
