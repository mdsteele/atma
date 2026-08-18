use super::binary::{BinaryIo, Decoder, Encoder};
use crate::error::{SourceContext, SourceError, SrcLoc, SrcSpan};
use std::io;
use std::rc::Rc;

//===========================================================================//

const TAG_ROOT: u8 = 0x00;
const TAG_USE: u8 = 0x01;

//===========================================================================//

/// A span of byte offsets within a particular assembly source code context.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ObjSrcLoc {
    /// The span of byte offsets within the context.
    pub span: SrcSpan,
    /// The file context that this location exists within.
    pub context: Rc<ObjSrcContext>,
}

impl ObjSrcLoc {
    /// Returns the primary [`SrcLoc`] for `self`, ignoring any further
    /// context.
    pub(crate) fn primary(&self) -> SrcLoc {
        SrcLoc { path: self.context.path.clone(), span: self.span }
    }
}

impl BinaryIo for ObjSrcLoc {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        let span = SrcSpan::read_from(decoder)?;
        let context = Rc::<ObjSrcContext>::read_from(decoder)?;
        Ok(ObjSrcLoc { span, context })
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        self.span.write_to(encoder)?;
        self.context.write_to(encoder)?;
        Ok(())
    }
}

//===========================================================================//

/// A context in which an [`ObjSrcLoc`] exists.
#[derive(Debug, Eq, Hash, PartialEq)]
pub struct ObjSrcContext {
    /// The path for the source code file.
    pub path: Rc<str>,
    /// The parent context that gave rise to this context.
    pub parent: ObjSrcParent,
}

impl ObjSrcContext {
    pub(crate) fn root(path: Rc<str>) -> ObjSrcContext {
        ObjSrcContext { path, parent: ObjSrcParent::Root }
    }
}

impl SourceContext for ObjSrcContext {
    fn annotate(&self, mut error: SourceError) -> SourceError {
        let mut context: &ObjSrcContext = self;
        loop {
            match &context.parent {
                ObjSrcParent::Root => return error,
                ObjSrcParent::Use(loc) => {
                    let label =
                        format!("Included `{}` from here", context.path);
                    error = error.with_label(
                        SrcLoc::new(&loc.context.path, loc.span),
                        label,
                    );
                    context = &loc.context;
                }
            }
        }
    }
}

impl BinaryIo for ObjSrcContext {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        let path = Rc::<str>::read_from(decoder)?;
        let parent = ObjSrcParent::read_from(decoder)?;
        Ok(ObjSrcContext { path, parent })
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        self.path.write_to(encoder)?;
        self.parent.write_to(encoder)?;
        Ok(())
    }
}

//===========================================================================//

/// Describes the circumstances that gave rise to a particular
/// [`ObjSrcContext`].
#[derive(Debug, Eq, Hash, PartialEq)]
pub enum ObjSrcParent {
    /// Indicates that the context has no parent; it is already the main
    /// assembly code file.
    Root,
    /// Indicates that the context was created by a `.USE` directive, whose
    /// path expression is at the given location.
    Use(ObjSrcLoc),
}

impl BinaryIo for ObjSrcParent {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        match u8::read_from(decoder)? {
            TAG_ROOT => Ok(Self::Root),
            TAG_USE => Ok(Self::Use(ObjSrcLoc::read_from(decoder)?)),
            byte => Err(io::Error::other(format!(
                "unknown ObjSrcParent tag: 0x{:02x}",
                byte
            ))),
        }
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        match self {
            Self::Root => TAG_ROOT.write_to(encoder),
            Self::Use(loc) => {
                TAG_USE.write_to(encoder)?;
                loc.write_to(encoder)
            }
        }
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{ObjSrcContext, ObjSrcLoc, ObjSrcParent};
    use crate::error::SrcSpan;
    use crate::obj::assert_round_trips;
    use std::rc::Rc;

    #[test]
    fn round_trip_obj_src_parent() {
        assert_round_trips(ObjSrcParent::Root);
        assert_round_trips(ObjSrcParent::Use(ObjSrcLoc {
            span: SrcSpan::from_byte_range(5..15),
            context: Rc::new(ObjSrcContext {
                path: Rc::from("foo/bar.asm"),
                parent: ObjSrcParent::Root,
            }),
        }));
    }
}

//===========================================================================//
