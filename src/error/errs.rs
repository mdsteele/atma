use std::fmt;

//===========================================================================//

/// An ordered list of errors.
#[derive(Eq, PartialEq)]
#[must_use]
pub struct Errs<E> {
    errors: Vec<E>,
}

impl<E> Errs<E> {
    /// Creates a new, empty list of errors.
    pub fn new() -> Errs<E> {
        Errs { errors: vec![] }
    }

    /// Creates a new error list containing a single error.
    pub fn one(error: E) -> Errs<E> {
        Errs { errors: vec![error] }
    }

    /// Returns `true` if there are no errors in the error list.
    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    /// Appends a single error to the error list.
    pub fn push(&mut self, error: E) {
        self.errors.push(error);
    }

    /// Appends a list of errors to the error list.
    pub fn append(&mut self, mut errors: Errs<E>) {
        self.errors.append(&mut errors.errors);
    }

    /// If the passed in result is `Ok`, returns the value, otherwise merges
    /// the errors into `self`.
    pub fn ok<T>(&mut self, result: Result<T, Errs<E>>) -> Option<T> {
        match result {
            Ok(value) => Some(value),
            Err(errs) => {
                self.append(errs);
                None
            }
        }
    }

    /// If the passed in result is `Ok`, returns the value, otherwise merges
    /// the errors into `self` and returns the given fallback value.
    pub fn ok_or<T>(&mut self, result: Result<T, Errs<E>>, fallback: T) -> T {
        self.ok(result).unwrap_or(fallback)
    }

    /// If the passed in result is `Ok`, returns the value, otherwise merges
    /// the errors into `self` and returns a default value.
    pub fn ok_or_default<T: Default>(
        &mut self,
        result: Result<T, Errs<E>>,
    ) -> T {
        self.ok(result).unwrap_or_default()
    }

    /// If the passed in result is `Ok`, returns the value, otherwise merges
    /// the errors into `self` and calls the function to produce a fallback
    /// value.
    pub fn ok_or_else<T, F: FnOnce() -> T>(
        &mut self,
        result: Result<T, Errs<E>>,
        func: F,
    ) -> T {
        self.ok(result).unwrap_or_else(func)
    }

    /// If the passed in result is `Err`, merges the errors into `self`.
    pub fn also(&mut self, result: Result<(), Errs<E>>) {
        if let Err(errs) = result {
            self.append(errs);
        }
    }

    /// Merges the errors into `self` and returns the value.
    pub fn with<T>(&mut self, (value, errors): (T, Errs<E>)) -> T {
        self.append(errors);
        value
    }

    /// If `self` is empty, returns `Ok(())`, otherwise returns `Err(self)`.
    pub fn result(self) -> Result<(), Errs<E>> {
        if self.errors.is_empty() { Ok(()) } else { Err(self) }
    }

    /// Transforms a list of one type of error into a list of another type of
    /// error, using the `From` trait.
    pub fn coerce<F: From<E>>(self) -> Errs<F> {
        self.into_iter().map(F::from).collect()
    }
}

impl<E: fmt::Debug> fmt::Debug for Errs<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        fmt::Debug::fmt(&self.errors, f)
    }
}

impl<E> Default for Errs<E> {
    fn default() -> Errs<E> {
        Errs::<E>::new()
    }
}

impl<E> FromIterator<E> for Errs<E> {
    fn from_iter<T>(iter: T) -> Errs<E>
    where
        T: IntoIterator<Item = E>,
    {
        Errs { errors: Vec::<E>::from_iter(iter) }
    }
}

impl<E> IntoIterator for Errs<E> {
    type Item = E;
    type IntoIter = ErrsIntoIter<E>;

    fn into_iter(self) -> ErrsIntoIter<E> {
        ErrsIntoIter { inner: self.errors.into_iter() }
    }
}

//===========================================================================//

/// An iterator that moves out of an `Errs`.
pub struct ErrsIntoIter<E> {
    inner: std::vec::IntoIter<E>,
}

impl<E> Iterator for ErrsIntoIter<E> {
    type Item = E;

    fn next(&mut self) -> Option<E> {
        self.inner.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<E> ExactSizeIterator for ErrsIntoIter<E> {
    fn len(&self) -> usize {
        self.inner.len()
    }
}

//===========================================================================//
