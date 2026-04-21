use std::collections::{HashMap, HashSet};
use std::rc::Rc;

//===========================================================================//

/// Manages definitions of and relationships between different architecture
/// settings.
pub(super) struct ArchTree {
    arches: HashMap<Rc<str>, ArchDefinition>,
}

impl ArchTree {
    /// The name of the root architecture.
    pub const ROOT_ARCH_NAME: &str = "none";

    /// Constructs a new architecture tree with only the root architecture
    /// defined.
    pub fn new() -> ArchTree {
        let mut arches = HashMap::new();
        arches.insert(
            Rc::from(ArchTree::ROOT_ARCH_NAME),
            ArchDefinition { parent: None, reserved: HashSet::new() },
        );
        ArchTree { arches }
    }

    /// Returns true if `name` is a defined architecture in this tree.
    pub fn contains_arch(&self, name: &str) -> bool {
        self.arches.contains_key(name)
    }

    /// Given an architecture name, returns the set of all identifier names
    /// that are reserved (e.g. because they are register names).  Any such
    /// names cannot be used for identifiers or labels while the specified
    /// architecture is set as the current architecture, and any macros
    /// parameter tokens that require matching one of these names exactly
    /// should do so case-insensitively.
    ///
    /// Panics if no such architecture is defined.
    pub fn reserved_names(&self, name: &str) -> &HashSet<Rc<str>> {
        match self.arches.get(name) {
            Some(def) => &def.reserved,
            None => panic!("No such architecture: {name:?}"),
        }
    }

    /// Given an architecture name, returns a list of all architectures that
    /// architecture is descended from, starting with the architecture itself
    /// and ending with the root architecture.  If no such architecture is
    /// defined, returns an empty list.
    pub fn get_all_ancestors(&self, name: &str) -> Vec<Rc<str>> {
        let mut arches = Vec::<Rc<str>>::new();
        let mut opt_next: Option<&str> = Some(name);
        while let Some(next) = opt_next {
            match self.arches.get_key_value(next) {
                Some((arch, def)) => {
                    arches.push(arch.clone());
                    opt_next = def.parent.as_deref();
                }
                None => break,
            }
        }
        arches
    }

    /// Defines a new architecture with the specified name and parent
    /// architecture.
    pub fn define_arch(
        &mut self,
        name: Rc<str>,
        parent: &str,
        reserved_names: &[Rc<str>],
    ) -> Result<(), DefineArchError> {
        if self.contains_arch(&name) {
            return Err(DefineArchError::ArchAlreadyExists);
        }
        let (parent, mut reserved) = match self.arches.get_key_value(parent) {
            None => return Err(DefineArchError::NoSuchParentArch),
            Some((parent, def)) => (parent.clone(), def.reserved.clone()),
        };
        for name in reserved_names {
            reserved.insert(name.clone());
        }
        self.arches
            .insert(name, ArchDefinition { parent: Some(parent), reserved });
        Ok(())
    }
}

//===========================================================================//

/// An error that can occur when defining a new architecture.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DefineArchError {
    ArchAlreadyExists,
    NoSuchParentArch,
}

//===========================================================================//

/// Helper struct for `ArchTree`.
struct ArchDefinition {
    /// The name of the parent architecture, or `None` is this is the root
    /// architecture.
    parent: Option<Rc<str>>,
    /// Identifier names that are reserved in this architecture (e.g. names of
    /// registers).  Any such names cannot be used for identifiers or labels
    /// while this is set as the current architecture, and any macros parameter
    /// tokens that require matching one of these names exactly will do so
    /// case-insensitively.
    reserved: HashSet<Rc<str>>,
}

//===========================================================================//
