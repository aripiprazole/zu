use std::sync::mpsc::{Receiver, Sender};

#[derive(Debug)]
pub enum Message {
    Error(Box<dyn miette::Diagnostic + Sync + Send>),
}

/// Global state of the compiler. It holds log messages etc.
pub struct GlobalState {
    tx: Sender<Message>,
    rx: Receiver<Message>
}

impl Default for GlobalState {
    /// Create a new global state.
    fn default() -> Self {
        let (tx, rx) = std::sync::mpsc::channel();

        Self { tx, rx }
    }
}

/// Presents the error to the user. It's a trait, so we can implement
/// different kind of presenters.
pub trait UiPresenter {
    fn present(&self, message: Message);
}

pub struct NarratorUiPresenter;

impl UiPresenter for NarratorUiPresenter {
    fn present(&self, message: Message) {
        match message {
            Message::Error(error) => {
                todo!()
            }
        }
    }
}