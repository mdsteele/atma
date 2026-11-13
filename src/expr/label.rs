use crate::obj::{BinaryIo, Decoder, Encoder};
use num_bigint::{BigInt, Sign};
use std::fmt;
use std::io;
use std::ops;
use std::rc::Rc;

//===========================================================================//

/// A reference to a specific memory location.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ExprLabel {
    /// A reference to an absolute address in a particular address space.
    AddrAbsolute {
        /// The address space that the address exist in.
        space: Rc<str>,
        /// The memory address.
        address: BigInt,
        /// The symbol, plus offset, associated with this reference, if any.
        /// This will be `None` if this reference has no association with any
        /// symbol (e.g. if it was promoted from a `ChunkRelative` or
        /// `ChunkAbsolute` reference that had no symbol provenance).
        provenance: Option<ExprSymbolProvenance>,
    },
    /// A reference to an absolute address in the address space of a given
    /// chunk in the current object file.
    ChunkAbsolute {
        /// The index of the chunk whose address space the address exists in.
        chunk_index: usize,
        /// The memory address.
        address: BigInt,
        /// The symbol, plus offset, associated with this reference, if any.
        /// This will be `None` if this reference has no association with any
        /// symbol (e.g. if it was created from a raw address via a unary `*`
        /// operator).
        provenance: Option<ExprSymbolProvenance>,
    },
    /// A reference to a memory location relative to the start of a given chunk
    /// in the current object file.
    ChunkRelative {
        /// The index of the chunk.
        chunk_index: usize,
        /// The byte offset from the start of the chunk.
        offset: BigInt,
        /// The symbol, plus offset, associated with this reference, if any.
        /// This will be `None` if this reference has no association with any
        /// symbol (e.g. if it's a `$<` reference).
        provenance: Option<ExprSymbolProvenance>,
    },
    /// A reference to a memory location relative to a declared symbol.
    SymbolRelative {
        /// The symbol, plus offset, associated with this reference.
        provenance: ExprSymbolProvenance,
    },
}

impl ExprLabel {
    /// Returns the symbol provenance, if any, for this label.
    pub fn provenance(&self) -> Option<&ExprSymbolProvenance> {
        match self {
            Self::AddrAbsolute { provenance, .. } => provenance.as_ref(),
            Self::ChunkAbsolute { provenance, .. } => provenance.as_ref(),
            Self::ChunkRelative { provenance, .. } => provenance.as_ref(),
            Self::SymbolRelative { provenance } => Some(provenance),
        }
    }

    pub(crate) fn into_absolute_address(self) -> Option<BigInt> {
        match self {
            Self::AddrAbsolute { address, .. }
            | Self::ChunkAbsolute { address, .. } => Some(address),
            Self::ChunkRelative { .. } | Self::SymbolRelative { .. } => None,
        }
    }

    pub(crate) fn map_absolute_address<F>(self, func: F) -> Option<Self>
    where
        F: FnOnce(&BigInt) -> BigInt,
    {
        match self {
            Self::AddrAbsolute {
                space,
                address: old_addr,
                provenance: old_prov,
            } => {
                let new_addr = func(&old_addr);
                let new_prov =
                    old_prov.map(|prov| prov + (&new_addr - &old_addr));
                Some(Self::AddrAbsolute {
                    space,
                    address: new_addr,
                    provenance: new_prov,
                })
            }
            Self::ChunkAbsolute {
                chunk_index,
                address: old_addr,
                provenance: old_prov,
            } => {
                let new_addr = func(&old_addr);
                let new_prov =
                    old_prov.map(|prov| prov + (&new_addr - &old_addr));
                Some(Self::ChunkAbsolute {
                    chunk_index,
                    address: new_addr,
                    provenance: new_prov,
                })
            }
            Self::ChunkRelative { .. } | Self::SymbolRelative { .. } => None,
        }
    }

    pub(crate) fn try_subtract(
        &self,
        rhs: &ExprLabel,
    ) -> Result<BigInt, SubtractLabelsError> {
        match (self, rhs) {
            (
                Self::AddrAbsolute {
                    space: lhs_space,
                    address: lhs_addr,
                    provenance: _,
                },
                Self::AddrAbsolute {
                    space: rhs_space,
                    address: rhs_addr,
                    provenance: _,
                },
            ) => {
                if lhs_space != rhs_space {
                    Err(SubtractLabelsError::DifferentAddrspaces(
                        lhs_space.clone(),
                        rhs_space.clone(),
                    ))
                } else {
                    Ok(lhs_addr - rhs_addr)
                }
            }
            (
                Self::ChunkAbsolute {
                    chunk_index: lhs_index,
                    address: lhs_addr,
                    provenance: _,
                },
                Self::ChunkAbsolute {
                    chunk_index: rhs_index,
                    address: rhs_addr,
                    provenance: _,
                },
            ) if lhs_index == rhs_index => Ok(lhs_addr - rhs_addr),
            (
                Self::ChunkRelative {
                    chunk_index: lhs_index,
                    offset: lhs_offset,
                    provenance: _,
                },
                Self::ChunkRelative {
                    chunk_index: rhs_index,
                    offset: rhs_offset,
                    provenance: _,
                },
            ) if lhs_index == rhs_index => Ok(lhs_offset - rhs_offset),
            (lhs, rhs) => match (lhs.provenance(), rhs.provenance()) {
                (Some(lhs_prov), Some(rhs_prov))
                    if lhs_prov.symbol_name == rhs_prov.symbol_name =>
                {
                    Ok(&lhs_prov.offset - &rhs_prov.offset)
                }
                _ => Err(SubtractLabelsError::Unresolved),
            },
        }
    }
}

impl fmt::Display for ExprLabel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::AddrAbsolute { space, address, provenance } => {
                write!(f, "{space} ${address:x}")?;
                if let Some(prov) = provenance {
                    write!(f, " ({prov})")?;
                }
                Ok(())
            }
            Self::ChunkAbsolute { chunk_index, address, provenance } => {
                write!(f, "Chunk#{chunk_index} ${address:x}")?;
                if let Some(prov) = provenance {
                    write!(f, " ({prov})")?;
                }
                Ok(())
            }
            Self::ChunkRelative { chunk_index, offset, provenance } => {
                write!(f, "Chunk#{chunk_index}")?;
                match offset.sign() {
                    Sign::Plus | Sign::NoSign => write!(f, " + ${offset:x}")?,
                    Sign::Minus => write!(f, " - ${:x}", offset.magnitude())?,
                }
                if let Some(prov) = provenance {
                    write!(f, " ({prov})")?;
                }
                Ok(())
            }
            Self::SymbolRelative { provenance } => provenance.fmt(f),
        }
    }
}

impl ops::Add<BigInt> for ExprLabel {
    type Output = Self;

    fn add(self, rhs: BigInt) -> Self {
        match self {
            Self::AddrAbsolute { space, mut address, provenance } => {
                address += &rhs;
                Self::AddrAbsolute {
                    space,
                    address,
                    provenance: provenance.map(|p| p + rhs),
                }
            }
            Self::ChunkAbsolute { chunk_index, mut address, provenance } => {
                address += &rhs;
                Self::ChunkAbsolute {
                    chunk_index,
                    address,
                    provenance: provenance.map(|p| p + rhs),
                }
            }
            Self::ChunkRelative { chunk_index, mut offset, provenance } => {
                offset += &rhs;
                Self::ChunkRelative {
                    chunk_index,
                    offset,
                    provenance: provenance.map(|p| p + rhs),
                }
            }
            Self::SymbolRelative { provenance } => {
                Self::SymbolRelative { provenance: provenance + rhs }
            }
        }
    }
}

impl ops::Sub<BigInt> for ExprLabel {
    type Output = Self;

    fn sub(self, rhs: BigInt) -> Self {
        self + (-rhs)
    }
}

//===========================================================================//

/// Information about a memory location that is associated with a specific
/// symbol.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ExprSymbolProvenance {
    /// The fully-qualified name of the symbol.
    pub symbol_name: Rc<str>,
    /// The byte offset from the symbol's address.
    pub offset: BigInt,
}

impl fmt::Display for ExprSymbolProvenance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.symbol_name)?;
        match self.offset.sign() {
            Sign::Plus => write!(f, " + ${:x}", self.offset.magnitude()),
            Sign::Minus => write!(f, " - ${:x}", self.offset.magnitude()),
            Sign::NoSign => Ok(()),
        }
    }
}

impl ops::Add<BigInt> for ExprSymbolProvenance {
    type Output = Self;

    fn add(mut self, rhs: BigInt) -> Self {
        self.offset += rhs;
        self
    }
}

impl ops::Sub<BigInt> for ExprSymbolProvenance {
    type Output = Self;

    fn sub(self, rhs: BigInt) -> Self {
        self + (-rhs)
    }
}

impl BinaryIo for ExprSymbolProvenance {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        let symbol_name = Rc::<str>::read_from(decoder)?;
        let offset = BigInt::read_from(decoder)?;
        Ok(Self { symbol_name, offset })
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        self.symbol_name.write_to(encoder)?;
        self.offset.write_to(encoder)
    }
}

//===========================================================================//

/// An error that can occur while subtracting one label from another.
#[derive(Debug, Eq, PartialEq)]
pub(crate) enum SubtractLabelsError {
    /// Tried to subtract one label from another, but the labels were in the
    /// given two different address spaces.
    DifferentAddrspaces(Rc<str>, Rc<str>),
    /// Tried to subtract one label from another, but the labels have not yet
    /// been resolved and the delta is not yet known.
    Unresolved,
}

//===========================================================================//
