use atma::addr::{Addr, BitWidth};
use atma::asm::assemble_source;
use atma::link::{LinkConfig, LinkError};
use atma::obj::{ObjFile, ObjPatchIntType};
use num_bigint::BigInt;
use std::assert_matches;
use std::rc::Rc;

//===========================================================================//

const COMMON_CONFIG: &str = r#"\
.ADDRSPACES {
CPU: bits=16
}
.REGIONS {
MAIN: space=CPU, start=$0000, size=$10000
}
.SECTIONS {
CODE: region=MAIN, start=$0000
}
"#;

//===========================================================================//

fn link_errors(config_source: &str, asm_sources: &[&str]) -> Vec<LinkError> {
    let mut cache = atma::error::StrSrcCache::new();
    let obj_files = asm_sources
        .iter()
        .enumerate()
        .map(|(index, asm_source)| {
            let asm_path = Rc::<str>::from(format!("asm{index}"));
            let asm_source = Rc::<str>::from(*asm_source);
            cache.add_source(asm_path.clone(), asm_source.clone());
            assemble_source(&mut cache, asm_path, &asm_source).unwrap()
        })
        .collect::<Vec<ObjFile>>();
    let config = {
        let config_path = Rc::<str>::from("config");
        let config_source = Rc::<str>::from(config_source);
        cache.add_source(config_path.clone(), config_source.clone());
        LinkConfig::from_source(&mut cache, config_path, &config_source)
            .unwrap()
    };
    match config.link_objects(obj_files) {
        Ok(_) => vec![],
        Err(errs) => errs.into_iter().collect::<Vec<_>>(),
    }
}

//===========================================================================//

#[test]
fn chunk_section_does_not_exist() {
    let asm0 = r#"\
    .SECTION "CODE"
        .u8 0
    .END
    .SECTION "Fhqwhgads"
        .u8 1
    .END
    "#;
    assert_matches!(link_errors(COMMON_CONFIG, &[asm0]).as_slice(), [
        LinkError::ChunkSectionDoesNotExist{
            section_name,
            ..
        },
    ] if &**section_name == "Fhqwhgads");
}

#[test]
fn label_in_wrong_address_space_direct() {
    let asm0 = r#"\
    .IMPORT Foo
    .SECTION "CODE"
        .a16le Foo
    .END
    "#;
    let config = r#"\
    .ADDRSPACES {
    CPU: bits=16
    PPU: bits=15
    }
    .REGIONS {
    MAIN: space=CPU, start=$0000, size=$10000
    }
    .SECTIONS {
    CODE: region=MAIN
    }
    .EXPORTS {
    Foo: space=PPU, addr=$1234
    }
    "#;
    assert_matches!(link_errors(config, &[asm0]).as_slice(), [
        LinkError::LabelInWrongAddressSpace{
            actual_address,
            address_width,
            actual_space,
            required_space,
            ..
        },
    ] if *actual_address == Addr::from(0x1234u16)
      && *address_width == BitWidth::new(15)
      && &**actual_space == "PPU"
      && &**required_space == "CPU");
}

#[test]
fn label_in_wrong_address_space_loadable() {
    let asm0 = r#"\
    .IMPORT Foo
    .SECTION "CODE"
    .LOADABLE "AUDIO"
        .a24le Foo
    .END
    .END
    "#;
    let config = r#"\
    .ADDRSPACES {
    APU: bits=16
    CPU: bits=24
    }
    .REGIONS {
    ARAM: space=APU, start=$0000, size=$10000
    MAIN: space=CPU, start=$000000, size=$1000000
    }
    .SECTIONS {
    AUDIO: region=ARAM
    CODE:  region=MAIN
    }
    .EXPORTS {
    Foo: space=CPU, addr=$123456
    }
    "#;
    assert_matches!(link_errors(config, &[asm0]).as_slice(), [
        LinkError::LabelInWrongAddressSpace{
            actual_address,
            address_width,
            actual_space,
            required_space,
            ..
        },
    ] if *actual_address == Addr::from(0x123456u32)
      && *address_width == BitWidth::new(24)
      && &**actual_space == "CPU"
      && &**required_space == "APU");
}

#[test]
fn patch_value_out_of_range() {
    let asm0 = r#"\
    .SECTION "CODE", start=$123
    .EXPORT Foo: .u8 0
    .END
    "#;
    let asm1 = r#"\
    .IMPORT Foo
    .SECTION "CODE"
        .a8 Foo
    .END
    "#;
    assert_matches!(link_errors(COMMON_CONFIG, &[asm0, asm1]).as_slice(), [
        LinkError::PatchValueOutOfRange{
            int_type: ObjPatchIntType::A8,
            expr_value,
            ..
        },
    ] if expr_value == &BigInt::from(0x123));
}

#[test]
fn section_is_empty() {
    let asm0 = r#"\
    .SECTION "FOO"
        .u8 0
    .END
    "#;
    let config = r#"\
    .ADDRSPACES {
    CPU: bits=16
    }
    .REGIONS {
    MAIN: space=CPU, start=$0000, size=$10000
    }
    .SECTIONS {
    FOO: region=MAIN
    BAR: region=MAIN
    }
    .IMPORT Foo
    .EXPORTS {
    Bar: space=CPU, addr=&Foo
    }
    "#;
    assert_matches!(link_errors(config, &[asm0]).as_slice(), [
        LinkError::SectionIsEmpty{
            section_name,
            ..
        },
    ] if &**section_name == "BAR");
}

#[test]
fn symbol_export_collision_between_obj_and_config() {
    let asm0 = r#"\
    .SECTION "CODE"
    .EXPORT Foo: .u8 0
    .END
    "#;
    let config = r#"\
    .ADDRSPACES {
    CPU: bits=16
    }
    .REGIONS {
    MAIN: space=CPU, start=$0000, size=$10000
    }
    .SECTIONS {
    CODE: region=MAIN
    }
    .EXPORTS {
    Foo: space=CPU, addr=$1234
    }
    "#;
    assert_matches!(link_errors(config, &[asm0]).as_slice(), [
        LinkError::SymbolExportCollision{
            symbol_name,
            ..
        },
    ] if &**symbol_name == "Foo");
}

#[test]
fn symbol_export_collision_between_two_obj_files() {
    let asm0 = r#"\
    .SECTION "CODE"
    .EXPORT Foo: .u8 0
    .END
    "#;
    let asm1 = r#"\
    .SECTION "CODE"
    .EXPORT Foo: .u8 1
    .END
    "#;
    assert_matches!(link_errors(COMMON_CONFIG, &[asm0, asm1]).as_slice(), [
        LinkError::SymbolExportCollision{
            symbol_name,
            ..
        },
    ] if &**symbol_name == "Foo");
}

#[test]
fn symbol_import_unresolved_in_config() {
    let asm0 = r#"\
    .SECTION "CODE"
    Foo: .u8 0
    .END
    "#;
    let config = r#"\
    .ADDRSPACES {
    CPU: bits=16
    }
    .REGIONS {
    MAIN: space=CPU, start=$0000, size=$10000
    }
    .SECTIONS {
    CODE: region=MAIN
    }
    .IMPORT Foo
    .EXPORTS {
    Bar: space=CPU, addr=&Foo
    }
    "#;
    assert_matches!(link_errors(config, &[asm0]).as_slice(), [
        LinkError::SymbolImportUnresolved{
            symbol_name,
            ..
        },
    ] if &**symbol_name == "Foo");
}

#[test]
fn symbol_import_unresolved_in_obj_file() {
    let asm0 = r#"\
    .IMPORT Foo
    .SECTION "CODE"
        .u8 &Foo
    .END
    "#;
    assert_matches!(link_errors(COMMON_CONFIG, &[asm0]).as_slice(), [
        LinkError::SymbolImportUnresolved{
            symbol_name,
            ..
        },
    ] if &**symbol_name == "Foo");
}

//===========================================================================//
