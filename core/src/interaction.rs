use emath::Pos2;

use crate::snake::{LinkType, Snake, SnakePilot, SnakeState};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Interaction {
    Attract(f32),
}
impl Interaction {
    pub(crate) fn interact(&self, centre: Pos2, snake: &mut SnakePilot, dt: f32) {
        match *self {
            Interaction::Attract(strength) => {
                let dir = centre - snake.snake_mut().derivatives_mut()[0].to_pos2();
                let dist = (0.008 as f32).max(dir.length_sq());
                let dir = dir.normalized();
                match snake.snake().data().order() {
                    0 => match snake.state() {
                        SnakeState::Anchored(_) => {
                            snake.snake_mut().derivatives_mut()[0] +=
                                dir * strength * dt * dt / dist;
                            snake.anchor()
                        }
                        SnakeState::Linked(link_type) => match link_type {
                            &LinkType::ToSelf(i) => {
                                if i == 0 {
                                    snake.snake_mut().derivatives_mut()[0] +=
                                        dir * strength * dt * dt / dist;
                                }
                            }
                            LinkType::ToOther(_, _) => todo!(),
                        },
                        _ => {}
                    },
                    1 => match snake.state() {
                        SnakeState::Anchored(_) => {
                            snake.snake_mut().derivatives_mut()[1] += dir * strength * dt / dist;
                            snake.anchor()
                        }
                        SnakeState::Linked(link_type) => match link_type {
                            LinkType::ToSelf(i) => {
                                snake.snake_mut().derivatives_mut()[1] +=
                                    dir * strength * dt / dist;
                            }
                            LinkType::ToOther(_, _) => todo!(),
                        },
                        _ => {}
                    },
                    2 => match snake.state() {
                        SnakeState::Anchored(_) => {
                            snake.snake_mut().derivatives_mut()[2] += dir * strength * dt / dist;
                            snake.anchor()
                        }
                        _ => {}
                    },
                    _ => {
                        snake.snake_mut().derivatives_mut()[2] += dir * strength * dt / dist;
                    }
                };
            }
        }
    }
}
