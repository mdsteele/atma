use super::patch::PatchedFile;
use super::positioned::{
    PositionedBinary, PositionedChunk, PositionedRegion, PositionedSection,
};
use std::io;

//===========================================================================//

/// Represents one piece of a final binary file.
pub enum LinkFragment {
    /// A slice of data that should be written into the binary.
    Data {
        /// The data to write.
        data: Box<[u8]>,
    },
    /// A span of the given size that should be filled with the given byte.
    Fill {
        /// The length of the fill span, in bytes.
        size: u64,
        /// The byte value to fill this span with.
        byte: u8,
    },
}

impl LinkFragment {
    pub(crate) fn from_patched_files(
        patched_files: Vec<PatchedFile>,
        positioned_binary: &PositionedBinary,
    ) -> Vec<LinkFragment> {
        let mut builder = FragmentBuilder {
            total_size: 0,
            patched_files,
            fragments: Vec::new(),
        };
        for region in &positioned_binary.regions {
            builder.add_region(region);
        }
        builder.fragments
    }

    /// Writes a sequence of binary fragments into a final binary.
    pub fn write_all(
        fragments: &[LinkFragment],
        writer: &mut impl io::Write,
    ) -> io::Result<()> {
        let mut fill_buffer = [0u8; 1024];
        for fragment in fragments {
            match fragment {
                LinkFragment::Data { data } => {
                    writer.write_all(data)?;
                }
                &LinkFragment::Fill { size, byte } => {
                    if fill_buffer[0] != byte {
                        let fill_len = size.min(fill_buffer.len() as u64);
                        fill_buffer[..fill_len as usize].fill(byte);
                    }
                    let mut remaining = size;
                    while remaining > 0 {
                        let write_len =
                            remaining.min(fill_buffer.len() as u64);
                        writer
                            .write_all(&fill_buffer[..write_len as usize])?;
                        remaining -= write_len;
                    }
                }
            }
        }
        Ok(())
    }
}

//===========================================================================//

struct FragmentBuilder {
    total_size: u64,
    patched_files: Vec<PatchedFile>,
    fragments: Vec<LinkFragment>,
}

impl FragmentBuilder {
    fn add_region(&mut self, region: &PositionedRegion) {
        let region_end = self.total_size + region.binary_size;
        for section in &region.sections {
            self.fill_to(section.binary_offset, region.fill);
            self.add_section(section);
        }
        self.fill_to(region_end, region.fill);
    }

    fn add_section(&mut self, section: &PositionedSection) {
        let section_end = self.total_size + section.binary_size;
        for chunk in &section.chunks {
            self.fill_to(chunk.binary_offset, section.fill);
            self.add_chunk(chunk);
        }
        self.fill_to(section_end, section.fill);
    }

    fn add_chunk(&mut self, chunk: &PositionedChunk) {
        let chunk_end = self.total_size + chunk.binary_size;
        let patched = &mut self.patched_files[chunk.id.object_index].chunks
            [chunk.id.chunk_index];
        let data = patched.take_data();
        self.total_size += data.len() as u64;
        self.fragments.push(LinkFragment::Data { data });
        self.fill_to(chunk_end, chunk.fill);
    }

    fn fill_to(&mut self, offset: u64, byte: u8) {
        let size = offset.strict_sub(self.total_size);
        if size > 0 {
            self.fragments.push(LinkFragment::Fill { size, byte });
        }
        self.total_size = offset;
    }
}

//===========================================================================//
