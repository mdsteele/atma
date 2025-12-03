use ariadne::{self, Label, ReportKind, Source};
use atma::asm::assemble_source;
use atma::bus::WatchKind;
use atma::db::{AdsEnvironment, AdsRuntimeError, SimEnv};
use atma::obj::{Align32, BinaryIo, ObjFile};
use atma::parse::ParseError;
use atma::proc::SimBreak;
use clap::{Parser, Subcommand};
use std::collections::HashSet;
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;
use std::process::ExitCode;

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
    /// Displays information from an object file.
    Obj {
        /// The object file to inspect.
        objfile: PathBuf,
    },
}

//===========================================================================//

enum CliError {
    Io(io::Error),
    Parse(String, Vec<ParseError>),
    AdsRuntimeError(AdsRuntimeError),
}

impl From<io::Error> for CliError {
    fn from(error: io::Error) -> CliError {
        CliError::Io(error)
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
        Command::Obj { objfile } => command_obj(objfile),
    };
    match result {
        Ok(()) => Ok(()),
        Err(CliError::Io(io_error)) => {
            report_io_error(io_error);
            Err(ExitCode::FAILURE)
        }
        Err(CliError::Parse(source, parse_errors)) => {
            report_parse_errors(&source, parse_errors);
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
    let source = io::read_to_string(fs::File::open(&source_path)?)?;
    let obj = match assemble_source(&source) {
        Ok(obj) => obj,
        Err(parse_errors) => {
            return Err(CliError::Parse(source, parse_errors));
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
            let file = fs::File::open(&ads_path)?;
            let source = io::read_to_string(file)?;
            match AdsEnvironment::create(&source, sim_env, io::stdout()) {
                Ok(ads_env) => ads_env,
                Err(parse_errors) => {
                    return Err(CliError::Parse(source, parse_errors));
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
        sim_env.watch_address(0xfff7, WatchKind::Pc);
        sim_env.watch_address(0xfff9, WatchKind::Pc);
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
        if chunk.align != Align32::default() {
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
            .collect::<HashSet<u32>>();
        for (offset, &byte) in chunk.data.iter().enumerate() {
            match offset % 16 {
                0 => print!("\n  "),
                8 => print!("  "),
                _ => {}
            }
            let prefix = if symbol_offsets.contains(&(offset as u32)) {
                ":"
            } else {
                " "
            };
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

fn report_parse_errors(source: &str, errors: Vec<ParseError>) {
    for error in errors {
        report_parse_error(source, error);
    }
}

fn report_parse_error(source: &str, error: ParseError) {
    let id = "input";
    let mut colors = ariadne::ColorGenerator::new();
    let mut builder = ariadne::Report::build(
        ReportKind::Error,
        (id, error.span.byte_range()),
    )
    .with_config(make_report_config())
    .with_message(&error.message);
    if error.labels.is_empty() {
        builder = builder.with_label(
            Label::new((id, error.span.byte_range()))
                .with_color(colors.next()),
        );
    }
    for label in error.labels {
        builder = builder.with_label(
            Label::new((id, label.span.byte_range()))
                .with_message(label.message)
                .with_color(colors.next()),
        );
    }
    builder.finish().print((id, Source::from(source))).unwrap()
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
