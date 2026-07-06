use std::collections::HashMap;
use std::fmt;
use std::io;
use std::rc::Rc;
use std::str::Utf8Error;

//===========================================================================//

/// Provides a means of fetching/caching binary and text source data.
pub trait SrcCache {
    /// Fetches the specified file as binary data, and appends it to the given
    /// byte vector.
    fn fetch_and_write_data(
        &mut self,
        path: &Rc<str>,
        out: &mut Vec<u8>,
    ) -> Result<(), SrcCacheError>;

    /// If necessary, fetches the specified file as UTF-8 source code and
    /// caches it, then returns the contents of the file from the cache.
    fn fetch_or_get_cached_utf8<'a>(
        &'a mut self,
        path: &Rc<str>,
    ) -> Result<&'a str, SrcCacheError>;
}

//===========================================================================//

/// An error that can occur while fetching a source file.
#[derive(Debug)]
pub enum SrcCacheError {
    /// No source file with the given path could be found.
    NotFound,
    /// An I/O error occurred while trying to fetch the contents of a source
    /// file.
    Io(io::Error),
    /// Failed to decode UTF8 data.
    Utf8(Utf8Error),
}

impl fmt::Display for SrcCacheError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::NotFound => write!(f, "no such file"),
            Self::Io(error) => write!(f, "I/O error: {error}"),
            Self::Utf8(error) => write!(f, "invalid UTF8: {error}"),
        }
    }
}

//===========================================================================//

/// A [`SrcCache`] implementation that stores data in-memory, and never reads
/// from the filesystem.
///
/// Since this implementation only ever provides data from memory, it will
/// never return a [`SrcCacheError::Io`] error from its [`SrcCache`] methods,
/// and since all its data is stored as UTF-8 `Rc<str>`s, it will never return
/// a [`SrcCacheError::Utf8`] error either.
pub struct StrSrcCache {
    srcs: HashMap<Rc<str>, Rc<str>>,
}

impl StrSrcCache {
    /// Constructs a new [`StrSrcCache`] that initially contains no files.
    ///
    /// Use [`add_source`](Self::add_source) to add files to the returned
    /// cache.
    pub fn new() -> Self {
        Self { srcs: HashMap::new() }
    }

    /// Inserts a new source file into the in-memory store.
    pub fn add_source(&mut self, path: Rc<str>, data: Rc<str>) {
        self.srcs.insert(path, data);
    }
}

impl Default for StrSrcCache {
    fn default() -> Self {
        Self::new()
    }
}

impl SrcCache for StrSrcCache {
    fn fetch_and_write_data(
        &mut self,
        path: &Rc<str>,
        out: &mut Vec<u8>,
    ) -> Result<(), SrcCacheError> {
        let string = self.srcs.get(path).ok_or(SrcCacheError::NotFound)?;
        out.extend_from_slice(string.as_bytes());
        Ok(())
    }

    fn fetch_or_get_cached_utf8<'a>(
        &'a mut self,
        path: &Rc<str>,
    ) -> Result<&'a str, SrcCacheError> {
        let string = self.srcs.get(path).ok_or(SrcCacheError::NotFound)?;
        Ok(Rc::as_ref(string))
    }
}

//===========================================================================//
