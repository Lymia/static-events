enum ErrorData {
    SynError(syn::Error),
    DarlingError(darling::Error),
    Combined(Vec<Error>),
    Empty,
}

/// A error type used by this crate.
pub struct Error(ErrorData);

impl Error {
    /// Creates a new error from a span and an message
    pub fn new<T: std::fmt::Display>(span: impl Into<proc_macro2::Span>, message: T) -> Self {
        Error(ErrorData::SynError(syn::Error::new(span.into(), message)))
    }

    /// Creates an error empty data for collecting errors.
    pub fn empty() -> Self {
        Error(ErrorData::Empty)
    }

    /// Returns whether this error data is empty.
    pub fn is_empty(&self) -> bool {
        match &self.0 {
            ErrorData::Empty => true,
            _ => false,
        }
    }

    /// Combines two errors into one so they can be emitted at one time.
    pub fn combine(self, other: impl Into<Error>) -> Self {
        let mut list = match self.0 {
            ErrorData::Combined(vec) => vec,
            ErrorData::Empty => Vec::new(),
            x => vec![Error(x)],
        };
        match other.into().0 {
            ErrorData::Combined(vec) => list.extend(vec),
            ErrorData::Empty => { }
            x => list.push(Error(x)),
        }
        Error(ErrorData::Combined(list))
    }

    /// Emits the error as a token stream.
    pub fn emit(self) -> proc_macro::TokenStream {
        match self.0 {
            ErrorData::SynError(e) => e.to_compile_error().into(),
            ErrorData::DarlingError(e) => e.write_errors().into(),
            ErrorData::Combined(e) => {
                let mut stream = proc_macro::TokenStream::new();
                for e in e {
                    stream.extend(e.emit());
                }
                stream
            }
            ErrorData::Empty => {
                Error::new(
                    proc_macro2::Span::call_site(),
                    "internal error: Error::empty() emitted?",
                ).emit()
            }
        }
    }
}

impl From<syn::Error> for Error {
    fn from(e: syn::Error) -> Self {
        Error(ErrorData::SynError(e))
    }
}

impl From<darling::Error> for Error {
    fn from(e: darling::Error) -> Self {
        Error(ErrorData::DarlingError(e))
    }
}

/// An error type specialized for this crate.
pub type Result<T> = std::result::Result<T, Error>;

#[macro_export]
macro_rules! try_syn {
    ($err:expr) => {
        match $err {
            Ok(v) => v,
            Err(e) => return e.emit().into(),
        }
    };
}
