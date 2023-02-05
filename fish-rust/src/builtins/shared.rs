use crate::builtins::wait;
use crate::ffi::{self, parser_t, wcharz_t, Repin, RustBuiltin};
use crate::wchar::{self, wstr};
use crate::wchar_ffi::{c_str, empty_wstring};
use libc::c_int;
use std::pin::Pin;

#[cxx::bridge]
mod builtins_ffi {
    extern "C++" {
        include!("wutil.h");
        include!("parser.h");
        include!("builtin.h");

        type wcharz_t = crate::ffi::wcharz_t;
        type parser_t = crate::ffi::parser_t;
        type io_streams_t = crate::ffi::io_streams_t;
        type RustBuiltin = crate::ffi::RustBuiltin;
    }
    extern "Rust" {
        fn rust_run_builtin(
            parser: Pin<&mut parser_t>,
            streams: Pin<&mut io_streams_t>,
            cpp_args: &Vec<wcharz_t>,
            builtin: RustBuiltin,
            status_code: &mut i32,
        ) -> bool;
    }

    impl Vec<wcharz_t> {}
}

pub enum CommandError {
    /// The status code used for failure exit in a command (but not if the args were invalid).
    CmdError,
    /// The status code used for invalid arguments given to a command. This is distinct from valid
    /// arguments that might result in a command failure. An invalid args condition is something
    /// like an unrecognized flag, missing or too many arguments, an invalid integer, etc. But
    InvalidArgs,
    /// The status code used when a command was not found.
    CmdUnknown,
    /// The status code used when an external command can not be run.
    NotExecutable,
    /// The status code used when a wildcard had no matches.
    UnmatchedWildcard,
    /// The status code used when illegal command name is encountered.
    IllegalCmd,
    /// The status code used when `read` is asked to consume too much data.
    ReadTooMuch,
    /// The status code when an expansion fails, for example, "$foo["
    ExpandError,
    Custom(i32),
}

impl From<CommandError> for c_int {
    fn from(err: CommandError) -> Self {
        match err {
            CommandError::CmdError => 1,
            CommandError::InvalidArgs => 2,
            CommandError::CmdUnknown => 127,
            CommandError::NotExecutable => 126,
            CommandError::UnmatchedWildcard => 124,
            CommandError::IllegalCmd => 123,
            CommandError::ReadTooMuch => 122,
            CommandError::ExpandError => 121,
            CommandError::Custom(n) => n,
        }
    }
}

/// A wrapper around output_stream_t.
pub struct output_stream_t(*mut ffi::output_stream_t);

impl output_stream_t {
    /// \return the underlying output_stream_t.
    fn ffi(&mut self) -> Pin<&mut ffi::output_stream_t> {
        unsafe { (*self.0).pin() }
    }

    /// Append a &wtr or WString.
    pub fn append<Str: AsRef<wstr>>(&mut self, s: Str) -> bool {
        self.ffi().append1(c_str!(s))
    }
}

// Convenience wrappers around C++ io_streams_t.
pub struct io_streams_t {
    streams: *mut builtins_ffi::io_streams_t,
    pub out: output_stream_t,
    pub err: output_stream_t,
}

impl io_streams_t {
    fn new(mut streams: Pin<&mut builtins_ffi::io_streams_t>) -> io_streams_t {
        let out = output_stream_t(streams.as_mut().get_out().unpin());
        let err = output_stream_t(streams.as_mut().get_err().unpin());
        let streams = streams.unpin();
        io_streams_t { streams, out, err }
    }

    fn ffi_pin(&mut self) -> Pin<&mut builtins_ffi::io_streams_t> {
        unsafe { Pin::new_unchecked(&mut *self.streams) }
    }

    fn ffi_ref(&self) -> &builtins_ffi::io_streams_t {
        unsafe { &*self.streams }
    }
}

fn rust_run_builtin(
    parser: Pin<&mut parser_t>,
    streams: Pin<&mut builtins_ffi::io_streams_t>,
    cpp_args: &Vec<wcharz_t>,
    builtin: RustBuiltin,
    status_code: &mut i32,
) -> bool {
    let mut storage = Vec::<wchar::WString>::new();
    for arg in cpp_args {
        storage.push(arg.into());
    }
    let mut args = Vec::new();
    for arg in &storage {
        args.push(arg.as_utfstr());
    }
    let streams = &mut io_streams_t::new(streams);

    match run_builtin(parser.unpin(), streams, args.as_mut_slice(), builtin) {
        None => false,
        Some(Ok(())) => {
            *status_code = 0;
            true
        }
        Some(Err(e)) => {
            *status_code = e.into();
            true
        }
    }
}

/// Runs the specified builtin. Returns `Some(status)` if `$status` should be updated, `None`
/// otherwise.
pub fn run_builtin(
    parser: &mut parser_t,
    streams: &mut io_streams_t,
    args: &mut [&wstr],
    builtin: RustBuiltin,
) -> Option<Result<(), CommandError>> {
    match builtin {
        RustBuiltin::Echo => Some(super::echo::echo(parser, streams, args)),
        RustBuiltin::Wait => Some(wait::wait(parser, streams, args)),
    }
}

// Covers of these functions that take care of the pinning, etc.
// These all return STATUS_INVALID_ARGS.
pub fn builtin_missing_argument(
    parser: &mut parser_t,
    streams: &mut io_streams_t,
    cmd: &wstr,
    opt: &wstr,
    print_hints: bool,
) {
    ffi::builtin_missing_argument(
        parser.pin(),
        streams.ffi_pin(),
        c_str!(cmd),
        c_str!(opt),
        print_hints,
    );
}

pub fn builtin_unknown_option(
    parser: &mut parser_t,
    streams: &mut io_streams_t,
    cmd: &wstr,
    opt: &wstr,
    print_hints: bool,
) {
    ffi::builtin_unknown_option(
        parser.pin(),
        streams.ffi_pin(),
        c_str!(cmd),
        c_str!(opt),
        print_hints,
    );
}

pub fn builtin_print_help(parser: &mut parser_t, streams: &io_streams_t, cmd: &wstr) {
    ffi::builtin_print_help(
        parser.pin(),
        streams.ffi_ref(),
        c_str!(cmd),
        empty_wstring(),
    );
}
