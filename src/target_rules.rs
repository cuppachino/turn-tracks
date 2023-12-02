#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Rule {
    TargetSelf { allowed: bool },
    TargetAllies { max: Option<usize>, force_all: bool },
    TargetEnemies { max: Option<usize>, force_all: bool },
    TargetAlive,
    TargetDead,
}

impl Add for Rule {
    type Output = TargetRule;
    fn add(self, rhs: Self) -> Self::Output {
        TargetRule::new_single(self) + TargetRule::new_single(rhs)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TargetRule {
    Single(Rule),
    Multiple(Vec<Rule>),
}

impl TargetRule {
    pub fn new_single(rule: Rule) -> Self {
        Self::Single(rule)
    }

    pub fn new_multiple(rules: Vec<Rule>) -> Self {
        Self::Multiple(rules)
    }
}

// ---

use std::{
    borrow::BorrowMut,
    ops::{Add, AddAssign, Not, Sub, SubAssign},
};

impl Not for Rule {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::TargetSelf { allowed } => Self::TargetSelf { allowed: !allowed },
            Self::TargetAllies { force_all, max } => Self::TargetEnemies { force_all, max },
            Self::TargetEnemies { force_all, max } => Self::TargetAllies { force_all, max },
            Self::TargetAlive => Self::TargetDead,
            Self::TargetDead => Self::TargetAlive,
        }
    }
}

impl Not for TargetRule {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Single(rule) => Self::Single(!rule),
            Self::Multiple(rules) => Self::Multiple(rules.into_iter().map(|r| !r).collect()),
        }
    }
}

impl Add for TargetRule {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Single(rule), Self::Single(other)) => Self::Multiple(vec![rule, other]),
            (Self::Single(rule), Self::Multiple(mut rules)) => {
                rules.push(rule);
                Self::Multiple(rules)
            }
            (Self::Multiple(mut rules), Self::Single(rule)) => {
                rules.push(rule);
                Self::Multiple(rules)
            }
            (Self::Multiple(mut rules), Self::Multiple(other_rules)) => {
                rules.extend(other_rules);
                Self::Multiple(rules)
            }
        }
    }
}

impl Sub for TargetRule {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Single(rule), Self::Single(other)) => {
                if rule == other {
                    Self::Multiple(vec![])
                } else {
                    Self::Single(rule)
                }
            }
            (Self::Single(rule), Self::Multiple(mut rules)) => {
                rules.retain(|r| *r != rule);
                if rules.len() == 1 {
                    Self::Single(rules[0].clone())
                } else {
                    Self::Multiple(rules)
                }
            }
            (Self::Multiple(mut rules), Self::Single(rule)) => {
                rules.retain(|r| *r != rule);
                if rules.len() == 1 {
                    Self::Single(rules[0].clone())
                } else {
                    Self::Multiple(rules)
                }
            }
            (Self::Multiple(mut rules), Self::Multiple(other_rules)) => {
                rules.retain(|r| !other_rules.contains(r));
                if rules.len() == 1 {
                    Self::Single(rules[0].clone())
                } else {
                    Self::Multiple(rules)
                }
            }
        }
    }
}
