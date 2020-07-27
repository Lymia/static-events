use static_events::prelude_async::*;

struct MyEvent(u32);
simple_event!(MyEvent, u32, 0);

struct AsyncNonStatic<'a>(&'a u32);
simple_event!(['a] AsyncNonStatic<'a>, u32, 0);

#[derive(Events, Default)]
struct MyEventHandler;

#[events_impl]
impl MyEventHandler {
    #[event_handler]
    fn handle_event(_target: &Handler<impl Events>, ev: &MyEvent, i: &mut u32) {
        *i += ev.0;
    }

    #[event_handler]
    async fn handle_event_async(_target: &Handler<impl Events>, ev: &MyEvent, i: &mut u32) {
        *i += ev.0;
    }

    #[event_handler]
    async fn handle_event_async_nonstatic<'a>(
        _target: &Handler<impl Events>, ev: &AsyncNonStatic<'a>, i: &mut u32,
    ) {
        *i += *ev.0;
    }
}

#[test]
fn sync_in_async_event_test() {
    let handler = Handler::new(MyEventHandler);
    assert_eq!(futures::executor::block_on(handler.dispatch_async(MyEvent(20))), 40);
}

fn async_nonstatic_test() {
    let handler = Handler::new(MyEventHandler);
    assert_eq!(futures::executor::block_on(handler.dispatch_async(AsyncNonStatic(&20))), 20);
}