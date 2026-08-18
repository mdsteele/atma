use ariadne::{self, Label, ReportKind, Source};
use atma::addr::{Addr, Align, Offset};
use atma::asm::{AsmError, assemble_source};
use atma::bus::WatchKind;
use atma::db::{AdsEnvironment, AdsError};
use atma::error::{Errs, SourceError, SrcCache, SrcCacheError};
use atma::link::{LinkConfig, LinkError};
use atma::obj::{BinaryIo, ObjFile};
use atma::proc::SimBreak;
use atma::system::{BinaryFormat, SimSystem, load_binary_with_format};
use clap::builder::{PossibleValuesParser, TypedValueParser};
use clap::{Parser, Subcommand};
use std::collections::hash_map;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display};
use std::fs;
use std::io::{self, Read, Write};
use std::path::PathBuf;
use std::process::ExitCode;
use std::rc::Rc;

//===========================================================================//

#[derive(Parser)]
#[clap(author, about, long_about = None, version)]
struct Cli {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Assembles a source file into an object file.
    Asm {
        /// The source file to assemble.
        source: PathBuf,
        /// The path of the output file.
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
    /// Simulates and debugs a compiled binary.
    Db {
        /// The format of the binary file.
        #[arg(short, long, default_value="auto",
              value_parser = binary_format_parser())]
        format: BinaryFormat,
        /// The compiled binary file to load and debug.
        binary: PathBuf,
        /// The debugger script to run, or none for interactive mode.
        script: Option<PathBuf>,
    },
    /// Links object files together.
    Ld {
        /// The linker configuration file.
        config: PathBuf,
        /// The object files to link together.
        objects: Vec<PathBuf>,
        /// The path of the output file.
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
    /// Displays information from an object file.
    Obj {
        /// The object file to inspect.
        objfile: PathBuf,
    },
}

//===========================================================================//

const ALL_BINARY_FORMATS: &[(BinaryFormat, &str)] = &[
    (BinaryFormat::Auto, "auto"),
    (BinaryFormat::Gb, "gb"),
    (BinaryFormat::Gbs, "gbs"),
    (BinaryFormat::Nes, "nes"),
    (BinaryFormat::Nsf, "nsf"),
    (BinaryFormat::Sfc, "sfc"),
    (BinaryFormat::Sim65, "sim65"),
    (BinaryFormat::Spc, "spc"),
];

fn binary_format_parser() -> impl TypedValueParser {
    PossibleValuesParser::new(ALL_BINARY_FORMATS.iter().map(|(_, name)| name))
        .map(|s| {
            ALL_BINARY_FORMATS.iter().find(|(_, name)| name == &s).unwrap().0
        })
}

//===========================================================================//

enum CliError {
    Io(io::Error),
    Source(FileSrcCache, Errs<SourceError>),
}

impl From<io::Error> for CliError {
    fn from(error: io::Error) -> CliError {
        CliError::Io(error)
    }
}

//===========================================================================//

fn main() -> ExitCode {
    match run_cli() {
        Ok(()) => ExitCode::SUCCESS,
        Err(code) => code,
    }
}

fn run_cli() -> Result<(), ExitCode> {
    let result = match Cli::parse().command {
        Command::Asm { source, output } => command_asm(source, output),
        Command::Db { format, binary, script } => {
            command_db(format, binary, script)
        }
        Command::Ld { config, objects, output } => {
            command_ld(config, objects, output)
        }
        Command::Obj { objfile } => command_obj(objfile),
    };
    match result {
        Ok(()) => Ok(()),
        Err(CliError::Io(io_error)) => {
            report_io_error(io_error);
            Err(ExitCode::FAILURE)
        }
        Err(CliError::Source(cache, source_errors)) => {
            report_source_errors(cache, source_errors);
            Err(ExitCode::FAILURE)
        }
    }
}

//===========================================================================//

fn command_asm(
    source_path: PathBuf,
    opt_output_path: Option<PathBuf>,
) -> Result<(), CliError> {
    let mut cache = FileSrcCache::new();
    let source_code = io::read_to_string(fs::File::open(&source_path)?)?;
    let path = Rc::<str>::from(source_path.to_string_lossy());
    let obj = match assemble_source(&mut cache, path, &source_code) {
        Ok(obj) => obj,
        Err(asm_errors) => {
            let source_errors = asm_errors.map(AsmError::to_source_error);
            return Err(CliError::Source(cache, source_errors));
        }
    };
    if let Some(output_path) = opt_output_path {
        let mut options = fs::OpenOptions::new();
        options.write(true).create(true).truncate(true);
        let writer = io::BufWriter::new(options.open(&output_path)?);
        let mut encoder = atma::obj::Encoder::new(writer);
        obj.write_to(&mut encoder)?;
        encoder.into_writer().flush()?;
    } else {
        dump_object_file(&obj);
    }
    Ok(())
}

//===========================================================================//

fn command_db(
    binary_format: BinaryFormat,
    binary_path: PathBuf,
    opt_ads_path: Option<PathBuf>,
) -> Result<(), CliError> {
    let mut system = {
        let file = fs::File::open(&binary_path)?;
        load_binary_with_format(binary_format, io::BufReader::new(file))?
    };
    print!("{}", system.description());
    if let Some(ads_path) = opt_ads_path {
        let mut cache = FileSrcCache::new();
        let mut ads_env = {
            let src_path = Rc::<str>::from(ads_path.to_string_lossy());
            let file = fs::File::open(&ads_path)?;
            let source = io::read_to_string(file)?;
            match AdsEnvironment::create(
                &mut cache,
                src_path,
                &source,
                system,
                io::stdout(),
            ) {
                Ok(ads_env) => ads_env,
                Err(ads_errors) => {
                    let source_errors =
                        ads_errors.map(AdsError::to_source_error);
                    return Err(CliError::Source(cache, source_errors));
                }
            }
        };
        loop {
            match ads_env.step() {
                Ok(true) => return Ok(()),
                Ok(false) => {}
                Err(error) => {
                    let errs = Errs::one(error.to_source_error());
                    return Err(CliError::Source(cache, errs));
                }
            }
        }
    } else {
        system.watch_address(Addr::from(0xfff7u16), WatchKind::Pc);
        system.watch_address(Addr::from(0xfff9u16), WatchKind::Pc);
        loop {
            let result = if system.is_mid_instruction() {
                system.step()
            } else {
                let pc = system.pc();
                let instruction = system.disassemble(pc).1;
                let result = system.step();
                println!(
                    "${:04x} | {:16} {}",
                    pc,
                    instruction,
                    format_registers(&system)
                );
                result
            };
            match result {
                Ok(()) => {}
                Err(SimBreak::Watchpoint(kind, id)) => {
                    println!("Watchpoint: {kind:?} {id:?}");
                    return Ok(());
                }
                Err(SimBreak::HaltOpcode(mnemonic, opcode)) => {
                    println!("Halted by {mnemonic} opcode ${opcode:02x}");
                    return Ok(());
                }
            }
        }
    }
}

//===========================================================================//

fn command_ld(
    config_path: PathBuf,
    objfile_paths: Vec<PathBuf>,
    opt_output_path: Option<PathBuf>,
) -> Result<(), CliError> {
    let mut cache = FileSrcCache::new();
    let config = {
        let source_code = io::read_to_string(fs::File::open(&config_path)?)?;
        let path = Rc::<str>::from(config_path.to_string_lossy());
        match LinkConfig::from_source(&mut cache, path, &source_code) {
            Ok(config) => config,
            Err(errs) => {
                let path = Rc::<str>::from(config_path.to_string_lossy());
                let source_errors =
                    errs.map(|error| error.to_source_error(&path));
                return Err(CliError::Source(cache, source_errors));
            }
        }
    };
    let object_files = {
        let mut objfiles = Vec::<ObjFile>::with_capacity(objfile_paths.len());
        for objfile_path in &objfile_paths {
            let reader = io::BufReader::new(fs::File::open(objfile_path)?);
            let mut decoder = atma::obj::Decoder::new(reader);
            objfiles.push(ObjFile::read_from(&mut decoder)?);
        }
        objfiles
    };
    let linked_binary = config.link_objects(object_files).map_err(|errs| {
        let source_errors = errs.map(LinkError::to_source_error);
        CliError::Source(cache, source_errors)
    })?;
    if let Some(output_path) = opt_output_path {
        let mut writer = io::BufWriter::new(fs::File::create(output_path)?);
        linked_binary.write_to(&mut writer)?;
        writer.flush()?;
    } else {
        eprintln!("Link successful.");
    }
    Ok(())
}

//===========================================================================//

fn command_obj(objfile_path: PathBuf) -> Result<(), CliError> {
    let reader = io::BufReader::new(fs::File::open(&objfile_path)?);
    let mut decoder = atma::obj::Decoder::new(reader);
    let objfile = ObjFile::read_from(&mut decoder)?;
    dump_object_file(&objfile);
    Ok(())
}

fn dump_object_file(obj: &ObjFile) {
    for (index, chunk) in obj.chunks.iter().enumerate() {
        print!(
            "Chunk {index}: {:?}, size=${:x}",
            chunk.section_name, chunk.size
        );
        if chunk.align != Align::default() {
            print!(", align=${:x}", chunk.align);
        }
        if let Some(within) = chunk.within {
            print!(", within=${within:x}");
        }
        for symbol in chunk.symbols.iter() {
            print!("\n  - {:04x} {}", symbol.offset, symbol.name);
        }
        let symbol_offsets = chunk
            .symbols
            .iter()
            .map(|symbol| symbol.offset)
            .collect::<HashSet<Offset>>();
        for (index, &byte) in chunk.data.iter().enumerate() {
            match index % 16 {
                0 => print!("\n  "),
                8 => print!("  "),
                _ => {}
            }
            let offset = Offset::try_from(index).unwrap();
            let prefix =
                if symbol_offsets.contains(&offset) { ":" } else { " " };
            print!(" {}{byte:02x}", prefix);
        }
        println!();
    }
}

//===========================================================================//

fn format_registers(system: &SimSystem) -> String {
    system
        .register_names(&system.selected_processor_name())
        .iter()
        .map(|name| {
            let value = system.get_register(name).unwrap();
            format!("{name}=${value:02x}")
        })
        .collect::<Vec<String>>()
        .join(" ")
}

//===========================================================================//

enum CacheEntry {
    Utf8(String),
    Source(ariadne::Source),
}

impl CacheEntry {
    fn as_str(&self) -> &str {
        match self {
            Self::Utf8(string) => string.as_str(),
            Self::Source(source) => source.text(),
        }
    }

    fn as_source(&mut self) -> &ariadne::Source {
        match self {
            Self::Utf8(string) => {
                let source = ariadne::Source::from(std::mem::take(string));
                *self = Self::Source(source);
                let Self::Source(source) = self else {
                    unreachable!();
                };
                source
            }
            Self::Source(source) => source,
        }
    }
}

struct FileSrcCache {
    cache: HashMap<Rc<str>, CacheEntry>,
}

impl FileSrcCache {
    pub fn new() -> Self {
        Self { cache: HashMap::new() }
    }

    fn ensure_cached(
        &mut self,
        path: Rc<str>,
    ) -> Result<&mut CacheEntry, SrcCacheError> {
        match self.cache.entry(path) {
            hash_map::Entry::Occupied(entry) => Ok(entry.into_mut()),
            hash_map::Entry::Vacant(entry) => {
                let mut file = fs::File::open(&**entry.key())
                    .map_err(SrcCacheError::Io)?;
                let mut data = Vec::new();
                file.read_to_end(&mut data).map_err(SrcCacheError::Io)?;
                match String::from_utf8(data) {
                    Ok(string) => Ok(entry.insert(CacheEntry::Utf8(string))),
                    Err(error) => Err(SrcCacheError::Utf8(error.utf8_error())),
                }
            }
        }
    }
}

impl SrcCache for FileSrcCache {
    fn fetch_and_write_data(
        &mut self,
        path: &Rc<str>,
        out: &mut Vec<u8>,
    ) -> Result<(), SrcCacheError> {
        if let Some(entry) = self.cache.get(path) {
            out.extend_from_slice(entry.as_str().as_bytes());
        } else {
            let mut file =
                fs::File::open(&**path).map_err(SrcCacheError::Io)?;
            file.read_to_end(out).map_err(SrcCacheError::Io)?;
        }
        Ok(())
    }

    fn fetch_or_get_cached_utf8<'a>(
        &'a mut self,
        path: &Rc<str>,
    ) -> Result<&'a str, SrcCacheError> {
        Ok(self.ensure_cached(path.clone())?.as_str())
    }
}

impl ariadne::Cache<Rc<str>> for FileSrcCache {
    type Storage = String;

    fn fetch(
        &mut self,
        path: &Rc<str>,
    ) -> Result<&ariadne::Source<String>, impl Debug> {
        Ok::<_, SrcCacheError>(self.ensure_cached(path.clone())?.as_source())
    }

    fn display<'a>(&self, path: &'a Rc<str>) -> Option<impl Display + 'a> {
        Some(Rc::as_ref(path))
    }
}

//===========================================================================//

fn report_source_errors(mut cache: FileSrcCache, errors: Errs<SourceError>) {
    for error in errors {
        report_source_error(&mut cache, error);
    }
}

fn report_source_error(
    cache: &mut impl ariadne::Cache<Rc<str>>,
    error: SourceError,
) {
    let mut colors = ariadne::ColorGenerator::new();
    let mut builder = ariadne::Report::build(
        ReportKind::Error,
        (error.loc.path.clone(), error.loc.span.byte_range()),
    )
    .with_config(make_report_config())
    .with_message(&error.message);
    for label in error.labels {
        let mut report_label =
            Label::new((label.loc.path, label.loc.span.byte_range()));
        if !label.message.is_empty() {
            report_label = report_label.with_message(label.message);
        }
        builder.add_label(report_label.with_color(colors.next()));
    }
    for note in error.notes {
        builder.add_note(note);
    }
    builder.finish().print(cache).unwrap()
}

fn report_io_error(error: io::Error) {
    let span = ("", 0..0);
    ariadne::Report::build(ReportKind::Error, span)
        .with_config(make_report_config())
        .with_message(error)
        .finish()
        .print(("", Source::from("")))
        .unwrap()
}

fn make_report_config() -> ariadne::Config {
    ariadne::Config::new().with_index_type(ariadne::IndexType::Byte)
}

//===========================================================================//
