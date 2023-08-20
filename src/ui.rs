use std::panic::AssertUnwindSafe;

use ratatui::{backend::CrosstermBackend, Terminal};

use super::*;

lazy_static! {
    static ref STATE: (Sender<State>, Receiver<State>) = crossbeam_channel::unbounded();

    /// The state of the compiler.
    pub static ref SX: Sender<State> = STATE.0.clone();

    /// The state of the compiler.
    pub static ref RX: Receiver<State> = STATE.1.clone();
}

/// Logs a new state.
pub fn log(state: State) {
    SX.send(state).unwrap();
}

pub struct Checking {}

/// The state for the compiler UI.
pub enum State {
    Resize,
    Tick,
    Checking(Checking),
}

pub fn input_handling() {
    let tick_rate = std::time::Duration::from_millis(200);

    std::thread::spawn(move || {
        let mut last_tick = std::time::Instant::now();
        loop {
            // poll for tick rate duration, if no events, sent tick event.
            let timeout = tick_rate
                .checked_sub(last_tick.elapsed())
                .unwrap_or_else(|| std::time::Duration::from_secs(0));
            if crossterm::event::poll(timeout).unwrap() {
                match crossterm::event::read().unwrap() {
                    crossterm::event::Event::Key(_) => todo!(),
                    crossterm::event::Event::Resize(_, _) => SX.send(State::Resize).unwrap(),
                    _ => {}
                };
            }
            if last_tick.elapsed() >= tick_rate {
                SX.send(State::Tick).unwrap();
                last_tick = std::time::Instant::now();
            }
        }
    });
}

pub fn run_app<B: ratatui::backend::Backend>(term: &mut Terminal<B>) -> miette::Result<()> {
    loop {
        // Draw the UI
        term.draw(|f| ui(f)).into_diagnostic()?;

        match RX.try_recv() {
            Ok(State::Checking(_)) => {}
            Ok(State::Resize) => term.autoresize().into_diagnostic()?,
            Ok(State::Tick) => todo!(),
            Err(crossbeam_channel::TryRecvError::Empty) => continue,
            Err(crossbeam_channel::TryRecvError::Disconnected) => break,
        }
    }

    Ok(())
}

/// Renders the UI
pub fn ui<B: ratatui::backend::Backend>(f: &ratatui::Frame<B>) {
    let _ = f;
}

/// The terminal of the compiler.
#[derive(Clone)]
pub struct ZuTerminal {
    pub data: Arc<SyncUnsafeCell<Terminal<CrosstermBackend<std::io::Stdout>>>>,
}

impl ZuTerminal {
    /// Runs the app in a new thread.
    ///
    /// # Safety
    ///
    /// Only run this once. It's not safe to run this multiple times.
    pub unsafe fn run_app(&self) {
        let terminal = self.clone();
        std::thread::spawn(move || unsafe {
            let terminal = &mut *terminal.data.get();
            run_app(terminal).unwrap()
        });
    }

    /// Clears the terminal.
    ///
    /// # Safety
    ///
    /// Only run this once. It's not safe to run this multiple times.
    pub unsafe fn clear(&self) -> miette::Result<()> {
        unsafe {
            let terminal = &mut *self.data.get();
            terminal.clear().into_diagnostic()
        }
    }

    /// Set up the miette hook
    pub fn install_hook(&self) -> miette::Result<()> {
        let terminal = AssertUnwindSafe(self.clone());
        miette::set_hook(Box::new(move |_| {
            crossterm::terminal::disable_raw_mode().unwrap();

            // Clear the terminal
            unsafe { terminal.clear().unwrap() };

            Box::new(bupropion::BupropionHandlerOpts::new().build())
        }))?;

        Ok(())
    }
}

unsafe impl Send for ZuTerminal {}
unsafe impl Sync for ZuTerminal {}
