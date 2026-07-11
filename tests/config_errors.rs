use atma;
use atma::addr::AlignTryFromError;
use atma::expr::ExprType;
use atma::link::{ConfigAttr, ConfigEntryKind, ConfigError, LinkConfig};
use num_bigint::BigInt;
use std::assert_matches;

//===========================================================================//

fn config_errors(source: &str) -> Vec<ConfigError> {
    match LinkConfig::from_source(source) {
        Ok(_) => vec![],
        Err(errs) => errs.into_iter().collect::<Vec<_>>(),
    }
}

//===========================================================================//

#[test]
fn attr_type_error() {
    let source = r#"\
    .ADDRSPACES {
        CPU: bits=%true
    }
    "#;
    assert_matches!(config_errors(source).as_slice(), [
        ConfigError::AttrTypeError{
            attribute: ConfigAttr::AddrspaceBits,
            expr_type: ExprType::Boolean,
            valid_types,
            ..
        },
    ] if valid_types == &[ExprType::Integer]);
}

#[test]
fn duplicate_attr_name() {
    let source = r#"\
    .ADDRSPACES {
        CPU: bits=16, bits=16
    }
    "#;
    assert_matches!(config_errors(source).as_slice(), [
        ConfigError::DuplicateAttrName{
            entry_name,
            attr_name,
            ..
        },
    ] if &**entry_name == "CPU" && &**attr_name == "bits");
}

#[test]
fn duplicate_entry_name() {
    let source = r#"\
    .ADDRSPACES {
        CPU: bits=16
        CPU: bits=16
    }
    "#;
    assert_matches!(config_errors(source).as_slice(), [
        ConfigError::DuplicateEntryName{ entry_name, .. },
    ] if &**entry_name == "CPU");
}

#[test]
fn duplicate_export() {
    let source = r#"\
    .ADDRSPACES {
        CPU: bits=16
    }
    .EXPORTS {
        PPUCTRL: space=CPU, addr=$2000
        PPUCTRL: space=CPU, addr=$2008
    }
    "#;
    assert_matches!(config_errors(source).as_slice(), [
        ConfigError::DuplicateExport{ symbol_name, .. },
    ] if &**symbol_name == "PPUCTRL");
}

#[test]
fn export_imported_symbol() {
    let source = r#"\
    .ADDRSPACES {
        CPU: bits=16
    }
    .IMPORT PPUCTRL
    .EXPORTS {
        PPUCTRL: space=CPU, addr=$2000
    }
    "#;
    assert_matches!(config_errors(source).as_slice(), [
        ConfigError::ExportImportedSymbol{ symbol_name, .. },
    ] if &**symbol_name == "PPUCTRL");
}

#[test]
fn invalid_alignment_value() {
    let source = r#"\
    .ADDRSPACES {
        CPU: bits=16
    }
    .MEMORY {
        ROM: space=CPU, start=0, size=$8000
    }
    .SECTIONS {
        CODE: region=ROM, align=$18
    }
    "#;
    assert_matches!(config_errors(source).as_slice(), [
        ConfigError::InvalidAlignmentAttr{
            attribute: ConfigAttr::SectionAlign,
            error: AlignTryFromError::NotAPowerOfTwo,
            expr_value,
            ..
        },
    ] if *expr_value == BigInt::from(0x18));
}

#[test]
fn invalid_attr_name() {
    let source = r#"\
    .ADDRSPACES {
        CPU: bits=16, foobar=17
    }
    "#;
    assert_matches!(config_errors(source).as_slice(), [
        ConfigError::InvalidAttrName{
            entry_kind: ConfigEntryKind::Addrspace,
            attr_name,
            ..
        },
    ] if &**attr_name == "foobar");
}

#[test]
fn invalid_checksum_format_attr() {
    let source = r#"\
    .CHECKSUMS {
        HeaderChecksum: sum={"u8"}, unit="foobar"
    }
    "#;
    assert_matches!(config_errors(source).as_slice(), [
        ConfigError::InvalidChecksumFormatAttr{
            attribute: ConfigAttr::ChecksumUnit,
            expr_value,
            ..
        },
    ] if &**expr_value == "foobar");
}

#[test]
fn missing_attr() {
    let source = r#"\
    .ADDRSPACES {
        CPU: fill=$ff
    }
    "#;
    assert_matches!(config_errors(source).as_slice(), [
        ConfigError::MissingAttr{
            entry_name,
            attribute: ConfigAttr::AddrspaceBits,
            ..
        },
    ] if &**entry_name == "CPU");
}

#[test]
fn mutually_exclusive_attrs() {
    let source = r#"\
    .CHECKSUMS {
        HeaderChecksum: sum={"u8"}, start=0, size=$4000, end=$8000
    }
    "#;
    assert_matches!(
        config_errors(source).as_slice(),
        [ConfigError::MutuallyExclusiveAttrs {
            attribute_1: ConfigAttr::ChecksumSize,
            attribute_2: ConfigAttr::ChecksumEnd,
            ..
        },]
    );
}

#[test]
fn non_static_attr() {
    let source = r#"\
    .IMPORT Foo
    .IMPORT Bar
    .ADDRSPACES {
        CPU: bits=(Foo - Bar)
    }
    "#;
    assert_matches!(
        config_errors(source).as_slice(),
        [ConfigError::NonStaticAttr {
            attribute: ConfigAttr::AddrspaceBits,
            ..
        },]
    );
}

#[test]
fn out_of_range_attr() {
    let source = r#"\
    .ADDRSPACES {
        CPU: bits=0
    }
    "#;
    assert_matches!(config_errors(source).as_slice(), [
        ConfigError::OutOfRangeAttr{
            attribute: ConfigAttr::AddrspaceBits,
            value,
            ..
        },
    ] if *value == BigInt::ZERO);
}

#[test]
fn parse_error() {
    let source = r#"\
    .ADDRSPACES (
        CPU: bits=16
    )
    "#;
    assert_matches!(
        config_errors(source).as_slice(),
        [ConfigError::ParseError { .. }]
    );
}

#[test]
fn region_range_overflow() {
    let source = r#"\
    .ADDRSPACES {
        CPU: bits=16
    }
    .MEMORY {
        ROM: space=CPU, start=$c000, size=$8000
    }
    "#;
    assert_matches!(config_errors(source).as_slice(), [
        ConfigError::RegionRangeOverflow{
            entry_name,
            start,
            size,
            bits: 16,
            ..
        },
    ] if (&**entry_name == "ROM"
          && u64::from(*start) == 0xc000
          && u128::from(*size) == 0x8000));
}

//===========================================================================//
