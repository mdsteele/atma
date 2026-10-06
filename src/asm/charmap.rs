use crate::error::SrcSpan;
use radix_trie::{Trie, TrieCommon};
use smallvec::SmallVec;
use static_assertions::const_assert_eq;
use std::rc::Rc;

//===========================================================================//

/// A `SmallVec` of bytes, with an array size that won't push the size of the
/// `SmallVec` any larger than its minimum size.
type BytesVec = SmallVec<[u8; 2 * std::mem::size_of::<usize>()]>;
const_assert_eq!(
    std::mem::size_of::<BytesVec>(),
    std::mem::size_of::<SmallVec<[u8; 1]>>()
);

//===========================================================================//

#[derive(Clone)]
pub(super) struct Charmap {
    trie: Trie<String, (SrcSpan, BytesVec)>,
}

impl Charmap {
    /// Returns a new, empty charmap with the given parent (if any).
    pub fn new() -> Self {
        Self { trie: Trie::new() }
    }

    /// Inserts a mapping into the charmap, or returns `Err` with the `SrcSpan`
    /// of a previous mapping that the new one would conflict with.
    pub fn try_insert(
        &mut self,
        span: SrcSpan,
        key: &str,
        bytes: &[u8],
    ) -> Result<(), (SrcSpan, Rc<str>)> {
        if let Some(value) = self.trie.get_mut(key) {
            // Replace the value for an existing mapping with this key.
            *value = (span, BytesVec::from_slice(bytes));
            Ok(())
        } else if let Some(subtrie) = self.trie.get_ancestor(key) {
            // Error: An existing mapping is a prefix of the new key.
            let prev_key = subtrie.key().unwrap();
            let &(prev_span, _) = subtrie.value().unwrap();
            Err((prev_span, Rc::from(prev_key.as_str())))
        } else if let Some(subtrie) = self.trie.get_raw_descendant(key) {
            // Error: The new key is a prefix of one or more existing mappings.
            let (prev_key, &(prev_span, _)) = subtrie.iter().next().unwrap();
            Err((prev_span, Rc::from(prev_key.as_str())))
        } else {
            // Insert a new mapping.
            let value = (span, BytesVec::from_slice(bytes));
            self.trie.insert(key.to_string(), value);
            Ok(())
        }
    }

    /// Given a string, returns the next mapped byte and the number of string
    /// bytes consumed, or returns `Err` with the shortest prefix of the input
    /// string that isn't a prefix of any mapping key in the charmap.
    fn lookup<'a>(
        &self,
        string: &'a str,
    ) -> Result<(&BytesVec, usize), &'a str> {
        let mut consumed: usize = 0;
        while consumed < string.len() {
            consumed = string.ceil_char_boundary(consumed + 1);
            let prefix = &string[..consumed];
            if let Some((_, bytes)) = self.trie.get(prefix) {
                return Ok((bytes, consumed));
            } else if self.trie.get_raw_descendant(prefix).is_none() {
                break;
            }
        }
        Err(&string[..consumed])
    }

    pub fn translate<'a>(
        &self,
        mut string: &'a str,
    ) -> Result<Vec<u8>, &'a str> {
        let mut translated = Vec::<u8>::new();
        while !string.is_empty() {
            let (bytes, len) = self.lookup(string)?;
            translated.extend_from_slice(bytes.as_slice());
            string = &string[len..];
        }
        Ok(translated)
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::Charmap;
    use crate::error::SrcSpan;
    use std::rc::Rc;

    #[test]
    fn empty_charmap() {
        let charmap = Charmap::new();
        assert_eq!(charmap.translate(""), Ok(vec![]));
        assert_eq!(charmap.translate("a"), Err("a"));
        assert_eq!(charmap.translate("bc"), Err("b"));
    }

    #[test]
    fn try_insert_errors() {
        let mut charmap = Charmap::new();
        let span1 = SrcSpan::from_byte_range(1..2);
        assert_eq!(charmap.try_insert(span1, "abc", &[1]), Ok(()));
        // It's an error to insert a new mapping that's a prefix of an existing
        // mapping.
        let span2 = SrcSpan::from_byte_range(2..3);
        assert_eq!(
            charmap.try_insert(span2, "ab", &[2]),
            Err((span1, Rc::from("abc")))
        );
        // It's OK to insert a new mapping that shares a common prefix with an
        // existing mapping.
        let span3 = SrcSpan::from_byte_range(3..4);
        assert_eq!(charmap.try_insert(span3, "abd", &[3]), Ok(()));
        // It's an error to insert a new mapping for which an existing mapping
        // is a prefix.
        assert_eq!(
            charmap.try_insert(span2, "abce", &[4]),
            Err((span1, Rc::from("abc")))
        );
        assert_eq!(
            charmap.try_insert(span2, "abde", &[4]),
            Err((span3, Rc::from("abd")))
        );
        // It's OK to completely replace an existing mapping.
        assert_eq!(charmap.try_insert(span2, "abc", &[4]), Ok(()));
    }

    #[test]
    fn translate_string() {
        let mut charmap = Charmap::new();
        charmap.try_insert(SrcSpan::INTERNAL, "a", &[1]).unwrap();
        charmap.try_insert(SrcSpan::INTERNAL, "b", &[2, 0]).unwrap();
        charmap.try_insert(SrcSpan::INTERNAL, "c", &[3]).unwrap();
        charmap.try_insert(SrcSpan::INTERNAL, "<>", &[4]).unwrap();
        assert_eq!(charmap.translate(""), Ok(vec![]));
        assert_eq!(charmap.translate("ac"), Ok(vec![1, 3]));
        assert_eq!(charmap.translate("abc"), Ok(vec![1, 2, 0, 3]));
        assert_eq!(charmap.translate("cab<>a"), Ok(vec![3, 1, 2, 0, 4, 1]));
        assert_eq!(charmap.translate("<><>b"), Ok(vec![4, 4, 2, 0]));
        assert_eq!(charmap.translate("abec"), Err("e"));
        assert_eq!(charmap.translate("ab<c>"), Err("<c"));
        assert_eq!(charmap.translate("ab<"), Err("<"));
    }
}

//===========================================================================//
