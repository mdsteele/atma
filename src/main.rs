use ariadne::{self, Label, ReportKind, Source};
use atma::db::{AdsEnvironment, AdsProgram, AdsRuntimeError, SimEnv};
use atma::parse::ParseError;
use atma::proc::{Breakpoint, SimBreak};
use clap::{Parser, Subcommand};
use std::fs::File;
use std::io;
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
    /// Simulates and debugs a compiled binary.
    Db {
        /// The compiled binary file to load and debug.
        binary: PathBuf,
        /// The debugger script to run, or none for interactive mode.
        script: Option<PathBuf>,
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
        Command::Db { binary, script } => command_db(binary, script),
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

fn command_db(
    binary_path: PathBuf,
    opt_ads_path: Option<PathBuf>,
) -> Result<(), CliError> {
    let mut sim_env = {
        let file = File::open(&binary_path)?;
        atma::db::load_binary(io::BufReader::new(file))?
    };
    print!("{}", sim_env.description());
    if let Some(ads_path) = opt_ads_path {
        let program = {
            let file = File::open(&ads_path)?;
            match AdsProgram::read_from(file) {
                Ok(program) => program,
                Err(parse_errors) => {
                    let file = File::open(&ads_path)?;
                    let source = io::read_to_string(file)?;
                    return Err(CliError::Parse(source, parse_errors));
                }
            }
        };
        let mut ads_env = AdsEnvironment::new(sim_env, program);
        loop {
            let finished = ads_env.step()?;
            if finished {
                return Ok(());
            }
        }
    } else {
        sim_env.add_breakpoint(Breakpoint::Pc(0xfff7));
        sim_env.add_breakpoint(Breakpoint::Pc(0xfff9));
        loop {
            let pc = sim_env.pc();
            let instruction = sim_env.disassemble(sim_env.pc()).1;
            let result = sim_env.step();
            println!(
                "${:04x} | {:16} {}",
                pc,
                instruction,
                format_registers(&sim_env)
            );
            match result {
                Ok(()) => {}
                Err(SimBreak::Breakpoint(breakpoint)) => {
                    println!("Breakpoint: {breakpoint:?}");
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

fn format_registers(sim_env: &SimEnv) -> String {
    sim_env
        .registers()
        .into_iter()
        .map(|(name, value)| format!("{name}=${value:02x}"))
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
    let mut colors = ariadne::ColorGenerator::new();
    let span = ("input", error.span.byte_range());
    ariadne::Report::build(ReportKind::Error, span.clone())
        .with_config(make_report_config())
        .with_message(&error.message)
        .with_label(Label::new(span.clone()).with_color(colors.next()))
        .finish()
        .print(("input", Source::from(source)))
        .unwrap()
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
