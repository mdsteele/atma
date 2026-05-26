//===========================================================================//

/// Specifies a byte ordering for storing multi-byte values in memory.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Endianness {
    /// Big-endian ordering (most significant byte first).
    BigEndian,
    /// Little-endian ordering (least significant byte first).
    LittleEndian,
}

//===========================================================================//
