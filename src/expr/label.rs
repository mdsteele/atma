use num_bigint::{BigInt, Sign};
use std::fmt;
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
    },
    /// A reference to an absolute address in the address space of a given
    /// chunk in the current object file.
    ChunkAbsolute {
        /// The index of the chunk whose address space the address exists in.
        chunk_index: usize,
        /// The memory address.
        address: BigInt,
    },
    /// A reference to a memory location relative to the start of a given chunk
    /// in the current object file.
    ChunkRelative {
        /// The index of the chunk.
        chunk_index: usize,
        /// The byte offset from the start of the chunk.
        offset: BigInt,
    },
    /// A reference to a memory location relative to a declared symbol.
    SymbolRelative {
        /// The fully-qualified name of the symbol.
        name: Rc<str>,
        /// The byte offset from the symbol's address.
        offset: BigInt,
    },
}

impl ExprLabel {
    pub(crate) fn try_subtract(
        &self,
        rhs: &ExprLabel,
    ) -> Result<BigInt, SubtractLabelsError> {
        match (self, rhs) {
            (
                ExprLabel::AddrAbsolute {
                    space: lhs_space,
                    address: lhs_addr,
                },
                ExprLabel::AddrAbsolute {
                    space: rhs_space,
                    address: rhs_addr,
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
                ExprLabel::ChunkAbsolute {
                    chunk_index: lhs_index,
                    address: lhs_addr,
                },
                ExprLabel::ChunkAbsolute {
                    chunk_index: rhs_index,
                    address: rhs_addr,
                },
            ) if lhs_index == rhs_index => Ok(lhs_addr - rhs_addr),
            (
                ExprLabel::ChunkRelative {
                    chunk_index: lhs_index,
                    offset: lhs_offset,
                },
                ExprLabel::ChunkRelative {
                    chunk_index: rhs_index,
                    offset: rhs_offset,
                },
            ) if lhs_index == rhs_index => Ok(lhs_offset - rhs_offset),
            (
                ExprLabel::SymbolRelative {
                    name: lhs_name,
                    offset: lhs_offset,
                },
                ExprLabel::SymbolRelative {
                    name: rhs_name,
                    offset: rhs_offset,
                },
            ) if lhs_name == rhs_name => Ok(lhs_offset - rhs_offset),
            _ => Err(SubtractLabelsError::Unresolved),
        }
    }
}

impl fmt::Display for ExprLabel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExprLabel::AddrAbsolute { space, address } => {
                write!(f, "{space} ${address:x}")
            }
            ExprLabel::ChunkAbsolute { chunk_index, address } => {
                write!(f, "Chunk#{chunk_index} ${address:x}")
            }
            ExprLabel::ChunkRelative { chunk_index, offset } => {
                write!(f, "Chunk#{chunk_index}")?;
                match offset.sign() {
                    Sign::Plus | Sign::NoSign => write!(f, " + ${offset:x}"),
                    Sign::Minus => write!(f, " - ${:x}", offset.magnitude()),
                }
            }
            ExprLabel::SymbolRelative { name, offset } => {
                f.write_str(name)?;
                match offset.sign() {
                    Sign::Plus => write!(f, " + ${:x}", offset.magnitude()),
                    Sign::Minus => write!(f, " - ${:x}", offset.magnitude()),
                    Sign::NoSign => Ok(()),
                }
            }
        }
    }
}

impl ops::Add<BigInt> for ExprLabel {
    type Output = Self;

    fn add(self, rhs: BigInt) -> Self {
        match self {
            Self::AddrAbsolute { space, address } => {
                Self::AddrAbsolute { space, address: address + rhs }
            }
            Self::ChunkAbsolute { chunk_index, address } => {
                Self::ChunkAbsolute { chunk_index, address: address + rhs }
            }
            Self::ChunkRelative { chunk_index, offset } => {
                Self::ChunkRelative { chunk_index, offset: offset + rhs }
            }
            Self::SymbolRelative { name, offset } => {
                Self::SymbolRelative { name, offset: offset + rhs }
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
