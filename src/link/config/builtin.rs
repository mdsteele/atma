use crate::expr::{ExprType, ExprValue, make_global_builtin_values};
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

pub(super) const ENTITY_REGION_KIND: &str = "region kind";

pub(super) const REGION_KIND_BSS: &str = "%bss";
pub(super) const REGION_KIND_DATA: &str = "%data";
pub(super) const REGION_KIND_OMIT: &str = "%omit";

//===========================================================================//

pub(super) fn make_config_builtin_values()
-> HashMap<Rc<str>, (ExprValue, ExprType)> {
    let mut builtins = make_global_builtin_values();
    let region_kind = Rc::<str>::from(ENTITY_REGION_KIND);
    add_builtin_entity(REGION_KIND_BSS, region_kind.clone(), &mut builtins);
    add_builtin_entity(REGION_KIND_DATA, region_kind.clone(), &mut builtins);
    add_builtin_entity(REGION_KIND_OMIT, region_kind, &mut builtins);
    builtins
}

fn add_builtin_entity(
    entity_name: &str,
    entity_type: Rc<str>,
    builtins: &mut HashMap<Rc<str>, (ExprValue, ExprType)>,
) {
    let entity_name = Rc::<str>::from(entity_name);
    let entity_value = ExprValue::Entity(entity_name.clone());
    let entity_type = ExprType::Entity(entity_type);
    builtins.insert(entity_name, (entity_value, entity_type));
}

//===========================================================================//
