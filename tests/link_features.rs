use atma::asm::assemble_source;
use atma::link::{LinkConfig, LinkedBinary};
use atma::obj::ObjFile;
use std::rc::Rc;

//===========================================================================//

fn link_binary(config_source: &str, asm_sources: &[&str]) -> LinkedBinary {
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
    config.link_objects(obj_files).unwrap()
}

fn binary_data(linked_binary: LinkedBinary) -> Vec<u8> {
    let mut data = Vec::<u8>::new();
    linked_binary.write_to(&mut data).unwrap();
    data
}

//===========================================================================//

#[test]
fn checksum() {
    let asm = r#"
    .SECTION "CODE"
        .u8 $12, $34, $56, $78, $9a, $bc, $de
    .EXPORT Checksum_u8:
        .reserve .u8
    .END
    "#;
    let config = r#"
    .ADDRSPACES { \\ CPU: bits=16 \\ }
    .REGIONS { \\ MAIN: space=CPU, start=0, size=8 \\ }
    .SECTIONS { \\ CODE: region=MAIN \\ }
    .CHECKSUMS { \\ Checksum_u8: sum="u8", unit="~u8", start=0, size=7 \\ }
    "#;
    assert_eq!(
        &binary_data(link_binary(config, &[asm])),
        &[0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xb1],
    );
}

#[test]
fn fill_byte() {
    let asm = r#"
    .SECTION "CODE", fill=$cc
        .u8 1
        .reserve .u8, 2
        .u8 2
    .END
    "#;
    let config = r#"
    .ADDRSPACES { \\ CPU: bits=16, fill=$aa \\ }
    .REGIONS space=CPU {
      FIRST:  start=0, size=8, fill=$22
      SECOND: start=8, size=2
    }
    .SECTIONS { \\ CODE: region=FIRST, start=$0001, size=6, fill=$55 \\ }
    "#;
    assert_eq!(
        &binary_data(link_binary(config, &[asm])),
        &[0x22, 1, 0xcc, 0xcc, 2, 0x55, 0x55, 0x22, 0xaa, 0xaa]
    );
}

#[test]
fn mirrored_section() {
    let asm = r#"
    .SECTION "ORIGINAL"
        .u8 $37
        .reserve .u8
    .END
    "#;
    let config = r#"
    .ADDRSPACES { \\ CPU: bits=16 \\ }
    .REGIONS { \\ MAIN: space=CPU, start=0, size=4 \\ }
    .SECTIONS region=MAIN {
      ORIGINAL: fill=$ff
      CLONE: mirror=ORIGINAL
    }
    "#;
    assert_eq!(
        &binary_data(link_binary(config, &[asm])),
        &[0x37, 0xff, 0x37, 0xff]
    );
}

//===========================================================================//
