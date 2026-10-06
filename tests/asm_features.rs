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

fn static_data(obj_file: ObjFile) -> Vec<Vec<u8>> {
    assert!(obj_file.variables.is_empty());
    obj_file
        .chunks
        .into_iter()
        .map(|obj_chunk| {
            assert!(obj_chunk.patches.is_empty());
            obj_chunk.data.to_vec()
        })
        .collect()
}

//===========================================================================//

#[test]
fn charmap_data() {
    let source = r#"\
    .CHARMAP "Foo" {
        " " -> $00
        ("A", "Z") -> ($01, $1a)
        "!" -> $1b
        "," -> $1c
        "<SMILE>" -> {$1e, $1f}
        ("a", "z") -> ($21, $3a)
    }
    .SECTION "TEST", charmap="Foo"
        .chars "Oh, hi! <SMILE>"
    .END
    "#;
    assert_eq!(
        static_data(assemble(source)),
        vec![vec![0x0f, 0x28, 0x1c, 0x00, 0x28, 0x29, 0x1b, 0x00, 0x1e, 0x1f]]
    );
}

#[test]
fn charmap_inheritence() {
    let source = r#"\
    .CHARMAP "Foo" {
        " " -> $00
        ("A", "Z") -> ($01, $1a)
        "<SMILE>" -> {$1e, $1f}
        ("a", "z") -> ($21, $3a)
        "!" -> $3f
    }
    .CHARMAP "Bar" : "Foo" {
        "!" -> $1b  ; override existing mapping
        "," -> $1c  ; add an additional mapping
    }
    .SECTION "TEST", charmap="Bar"
        .chars "Oh, hi! <SMILE>"
    .END
    "#;
    assert_eq!(
        static_data(assemble(source)),
        vec![vec![0x0f, 0x28, 0x1c, 0x00, 0x28, 0x29, 0x1b, 0x00, 0x1e, 0x1f]]
    );
}

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
fn elsewhere_chunk() {
    let source = r#"\
    .SECTION "TEST"
        .u8 1
    .ELSEWHERE "OTHER"
        .u8 4
    .END
        .u8 2
        .u8 3
    .END
    "#;
    assert_eq!(
        static_data(assemble(source)),
        vec![vec![0x01, 0x02, 0x03], vec![0x04]]
    );
}

#[test]
fn enum_automatic_values() {
    let source = r#"\
    .ENUM eFoobar {
        Foo
        Bar
        Baz
    }
    .ASSERT eFoobar::Foo == 0
    .ASSERT eFoobar::Bar == 1
    .ASSERT eFoobar::Baz == 2
    .ASSERT eFoobar::%values == {0, 1, 2}
    "#;
    assert!(assemble(source).variables.is_empty());
}

#[test]
fn enum_manual_values() {
    let source = r#"\
    .ENUM eFoobar {
        Foo = 3
        Bar
        Baz = Bar - Foo
    }
    .ASSERT eFoobar::Foo == 3
    .ASSERT eFoobar::Bar == 4
    .ASSERT eFoobar::Baz == 1
    .ASSERT eFoobar::%values == {3, 4, 1}
    "#;
    assert!(assemble(source).variables.is_empty());
}

#[test]
fn label_references() {
    let source = r#"\
    .SECTION "TEST"
        .u16le Foo::Bar
    Foo: {
        .u16le Foo
        .u16le Bar
    Bar:
        .u16le Bar
        .u16le Foo
    }
        .u16le Foo::Bar
    .END
    "#;
    assemble(source);
}

#[test]
fn loadable_chunk() {
    let source = r#"\
    .SECTION "TEST", start=$10
        .u8 1
    .LOADABLE "OTHER", start=$20
        .u8 2
        .u8 $<
    .END
        .u8 $<
    .END
    "#;
    assert_eq!(
        static_data(assemble(source)),
        vec![vec![0x01, 0x02, 0x21, 0x13], vec![]]
    );
}

#[test]
fn static_here_address() {
    let source = r#"\
    .SECTION "TEST", start=$10
        .u8 1
        .u8 $<
    {
        .u8 3
        .u8 $^
    }
    .END
    "#;
    assert_eq!(
        static_data(assemble(source)),
        vec![vec![0x01, 0x11, 0x03, 0x12]]
    );
}

#[test]
fn string_data() {
    let source = r#"\
    .SECTION "TEST", fill=$ab
        .ascii "Foo", $1b
        .utf8 "\u{1F602}", $80
    .END
    "#;
    assert_eq!(
        static_data(assemble(source)),
        vec![vec![0x46, 0x6f, 0x6f, 0x1b, 0xf0, 0x9f, 0x98, 0x82, 0xc2, 0x80]]
    );
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
        vec![vec![0xab, 0xab, 0xab, 0xab, 0xab, 0xab, 0x00, 0x03, 0x05, 0x06]]
    );
}

#[test]
fn with_addr_operator() {
    let source = r#"\
    .SECTION "TEST", start=$8000
        .u8 %sqrtx($< - *$7edf)
    .END
    "#;
    assert_eq!(static_data(assemble(source)), vec![vec![0x11]]);
}

#[test]
fn with_arch_attr() {
    let source = r#"\
    .WITH arch="6502"
    .SECTION "TEST"
        cld
        .with arch="SM83"
        halt
        .end
        dex
    .END
    .END
    "#;
    assert_eq!(static_data(assemble(source)), vec![vec![0xd8, 0x76, 0xca]]);
}

#[test]
fn with_fill_attr() {
    let source = r#"\
    .WITH fill=$01
    .SECTION "TEST"
        .reserve .u8
        .with fill=$02
        .reserve .u8
        .end
        .reserve .u8
        .u8 $03
        .with fill=$04
        .reserve .u8
        .end
    .END
    .END
    "#;
    assert_eq!(
        static_data(assemble(source)),
        vec![vec![0x01, 0x02, 0x01, 0x03, 0x04]]
    );
}

//===========================================================================//
