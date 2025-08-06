use atma::db::{AdsEnvironment, AdsProgram, SimEnv};
use atma::proc::{Breakpoint, SimBreak};
use clap::{Parser, Subcommand};
use std::fs::File;
use std::io;
use std::path::PathBuf;

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

fn format_registers(sim_env: &SimEnv) -> String {
    sim_env
        .registers()
        .into_iter()
        .map(|(name, value)| format!("{name}=${value:02x}"))
        .collect::<Vec<String>>()
        .join(" ")
}

fn main() -> io::Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Command::Db { binary, script } => {
            let mut sim_env = {
                let file = File::open(&binary)?;
                atma::db::load_binary(io::BufReader::new(file))?
            };
            print!("{}", sim_env.description());
            if let Some(ads_path) = script {
                let program = {
                    let file = File::open(ads_path)?;
                    AdsProgram::read_from(file).unwrap() // TODO handle error
                };
                let mut ads_env = AdsEnvironment::new(sim_env, program);
                loop {
                    match ads_env.step() {
                        Ok(true) => break,
                        Ok(false) => {}
                        Err(error) => panic!("{error:?}"),
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
                            break;
                        }
                        Err(SimBreak::HaltOpcode(mnemonic, opcode)) => {
                            println!(
                                "Halted by {mnemonic} opcode ${opcode:02x}"
                            );
                            break;
                        }
                    }
                }
            }
        }
    }
    Ok(())
}

//===========================================================================//
