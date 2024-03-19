use emath::{pos2, vec2, Pos2, Vec2};

mod actions;
mod game_state;
mod interaction;
mod network;
mod objects;
mod snake;
mod style;
mod world;
mod zone;

pub use actions::*;
pub use game_state::*;
pub use network::*;
pub use objects::*;
pub use snake::*;
pub use style::*;
pub use world::*;
pub use zone::*;

/// Get position from radius and cw angle with 0 radians as -y
fn pos_rt(r: f32, t: f32) -> Pos2 {
    pos2(0., 0.) + vec_rt(r, t)
}
/// Get vector from radius and cw angle with 0 radians as -y
fn vec_rt(r: f32, t: f32) -> Vec2 {
    r * vec2(t.sin(), -t.cos())
}
