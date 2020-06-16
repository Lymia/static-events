use static_events::*;

#[derive(Events)]
pub struct HasParameter<T: Sync + 'static>(T);