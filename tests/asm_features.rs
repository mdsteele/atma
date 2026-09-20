use atma::obj::ObjFile;
use std::rc::Rc;

//===========================================================================//

fn assemble(source: &str) -> ObjFile {
    let asm_path = Rc::<str>::from("input");
    let asm_source = Rc::<str>::from(source);
    let mut cache = atma::error::StrSrcCache::new();
    cache.add_source(asm_path.clone(), asm_source.clone());
    atma::asm::assemble_source(&mut cache, asm_path, &asm_source).unwrap()
}

fn static_data(obj_file: ObjFile) -> Vec<u8> {
    assert_eq!(obj_file.chunks.len(), 1);
    let obj_chunk = &obj_file.chunks[0];
    assert!(obj_chunk.patches.is_empty());
    obj_chunk.data.to_vec()
}

//===========================================================================//

#[test]
fn compound_id_for_named_scope() {
    let source = r#"\
    .SECTION "TEST"
        .u8 Foo::Bar
    Foo: {
        .u8 1
    Bar:
        .u8 2
    }
    .END
    "#;
    assert_eq!(assemble(source).chunks[0].patches.len(), 1);
}

#[test]
fn static_here_address() {
    let source = r#"\
    .SECTION "TEST", start=$10
        .u8 1
        .u8 $<
    .END
    "#;
    assert_eq!(static_data(assemble(source)), vec![0x01, 0x11]);
}

#[test]
fn struct_field_offsets() {
    let source = r#"\
    .STRUCT sFoo {
        Bar_u8_arr3 : .u8, 3
        Baz_u16     : .u16
        Blarg_u8    : .u8
    }
    .SECTION "TEST", fill=$ab
        .reserve sFoo
        .u8 sFoo::Bar_u8_arr3
        .u8 sFoo::Baz_u16
        .u8 sFoo::Blarg_u8
        .u8 sFoo::%size
    .END
    "#;
    assert_eq!(
        static_data(assemble(source)),
        vec![0xab, 0xab, 0xab, 0xab, 0xab, 0xab, 0x00, 0x03, 0x05, 0x06]
    );
}

//===========================================================================//
