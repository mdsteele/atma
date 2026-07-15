use ariadne::{self, Label, ReportKind, Source};
use atma::addr::{Addr, Align, Offset};
use atma::asm::assemble_source;
use atma::bus::WatchKind;
use atma::db::{AdsEnvironment, AdsRuntimeError, SimEnv};
use atma::error::{Errs, SourceError, SrcCache, SrcCacheError};
use atma::link::{LinkConfig, LinkError};
use atma::obj::{BinaryIo, ObjFile};
use atma::proc::SimBreak;
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

enum CliError {
    Io(io::Error),
    Source(FileSrcCache, Rc<str>, Errs<SourceError>),
    Link(Errs<LinkError>),
    AdsRuntimeError(AdsRuntimeError),
}

impl From<io::Error> for CliError {
    fn from(error: io::Error) -> CliError {
        CliError::Io(error)
    }
}

impl From<Errs<LinkError>> for CliError {
    fn from(errors: Errs<LinkError>) -> CliError {
        CliError::Link(errors)
    }
}

impl From<AdsRuntimeError> for CliError {
    fn from(error: AdsRuntimeError) -> CliError {
        CliError::AdsRuntimeError(error)
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
        Command::Db { binary, script } => command_db(binary, script),
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
        Err(CliError::Source(cache, path, source_errors)) => {
            report_source_errors(cache, path, source_errors);
            Err(ExitCode::FAILURE)
        }
        Err(CliError::Link(link_errors)) => {
            report_link_errors(link_errors);
            Err(ExitCode::FAILURE)
        }
        Err(CliError::AdsRuntimeError(_)) => {
            // TODO: Report error details
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
    let src_path = Rc::<str>::from(source_path.to_string_lossy());
    let obj = match assemble_source(&mut cache, src_path.clone(), &source_code)
    {
        Ok(obj) => obj,
        Err(asm_errors) => {
            let source_errors = SourceError::from_errors(asm_errors);
            return Err(CliError::Source(cache, src_path, source_errors));
        }
    };
    if let Some(output_path) = opt_output_path {
        let mut options = fs::OpenOptions::new();
        options.write(true).create(true).truncate(true);
        let mut writer = io::BufWriter::new(options.open(&output_path)?);
        obj.write_to(&mut writer)?;
        writer.flush()?;
    } else {
        dump_object_file(&obj);
    }
    Ok(())
}

//===========================================================================//

fn command_db(
    binary_path: PathBuf,
    opt_ads_path: Option<PathBuf>,
) -> Result<(), CliError> {
    let mut sim_env = {
        let file = fs::File::open(&binary_path)?;
        atma::db::load_binary(io::BufReader::new(file))?
    };
    print!("{}", sim_env.description());
    if let Some(ads_path) = opt_ads_path {
        let mut ads_env = {
            let mut cache = FileSrcCache::new();
            let src_path = Rc::<str>::from(ads_path.to_string_lossy());
            let file = fs::File::open(&ads_path)?;
            let source = io::read_to_string(file)?;
            match AdsEnvironment::create(
                &mut cache,
                src_path.clone(),
                &source,
                sim_env,
                io::stdout(),
            ) {
                Ok(ads_env) => ads_env,
                Err(ads_errors) => {
                    // TODO: require paths to be UTF-8
                    let source_errors = SourceError::from_errors(ads_errors);
                    return Err(CliError::Source(
                        cache,
                        src_path,
                        source_errors,
                    ));
                }
            }
        };
        loop {
            let finished = ads_env.step()?;
            if finished {
                return Ok(());
            }
        }
    } else {
        sim_env.watch_address(Addr::from(0xfff7u16), WatchKind::Pc);
        sim_env.watch_address(Addr::from(0xfff9u16), WatchKind::Pc);
        loop {
            let result = if sim_env.is_mid_instruction() {
                sim_env.step()
            } else {
                let pc = sim_env.pc();
                let instruction = sim_env.disassemble(pc).1;
                let result = sim_env.step();
                println!(
                    "${:04x} | {:16} {}",
                    pc,
                    instruction,
                    format_registers(&sim_env)
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
    let cache = FileSrcCache::new();
    let config = {
        let source = io::read_to_string(fs::File::open(&config_path)?)?;
        LinkConfig::from_source(&source).map_err(|config_errors| {
            // TODO: require paths to be UTF-8
            let path = Rc::<str>::from(config_path.to_string_lossy());
            let source_errors = SourceError::from_errors(config_errors);
            CliError::Source(cache, path, source_errors)
        })?
    };
    let object_files = {
        let mut objfiles = Vec::<ObjFile>::with_capacity(objfile_paths.len());
        for objfile_path in &objfile_paths {
            let mut reader = io::BufReader::new(fs::File::open(objfile_path)?);
            objfiles.push(ObjFile::read_from(&mut reader)?);
        }
        objfiles
    };
    let linked_binary = config.link_objects(object_files)?;
    if let Some(output_path) = opt_output_path {
        let mut output = io::BufWriter::new(fs::File::create(output_path)?);
        linked_binary.write_to(&mut output)?;
        output.flush()?;
    } else {
        eprintln!("Link successful.");
    }
    Ok(())
}

//===========================================================================//

fn command_obj(objfile_path: PathBuf) -> Result<(), CliError> {
    let mut reader = io::BufReader::new(fs::File::open(&objfile_path)?);
    let objfile = ObjFile::read_from(&mut reader)?;
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

fn format_registers(sim_env: &SimEnv) -> String {
    sim_env
        .register_names()
        .iter()
        .map(|name| {
            let value = sim_env.get_register(name).unwrap();
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

fn report_link_errors(errors: Errs<LinkError>) {
    for error in errors {
        report_link_error(error);
    }
}

fn report_link_error(error: LinkError) {
    eprintln!("Link error: {error:?}");
}

fn report_source_errors(
    mut cache: FileSrcCache,
    path: Rc<str>,
    errors: Errs<SourceError>,
) {
    for error in errors {
        report_source_error(&mut cache, &path, error);
    }
}

fn report_source_error(
    cache: &mut impl ariadne::Cache<Rc<str>>,
    path: &Rc<str>,
    error: SourceError,
) {
    let mut colors = ariadne::ColorGenerator::new();
    let mut builder = ariadne::Report::build(
        ReportKind::Error,
        (path.clone(), error.span.byte_range()),
    )
    .with_config(make_report_config())
    .with_message(&error.message);
    if error.labels.is_empty() {
        builder.add_label(
            Label::new((path.clone(), error.span.byte_range()))
                .with_color(colors.next()),
        );
    }
    for error_label in error.labels {
        let mut report_label =
            Label::new((path.clone(), error_label.span.byte_range()));
        if !error_label.message.is_empty() {
            report_label = report_label.with_message(error_label.message);
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
