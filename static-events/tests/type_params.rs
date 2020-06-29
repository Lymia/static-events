use static_events::prelude_sync::*;

#[derive(Events)]
pub struct HasParameter<T: Sync + Send + 'static>(T);