use atma::db::{Breakpoint, SimBreak};
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

fn main() -> io::Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Command::Db { binary, .. } => {
            let mut sim_env = {
                let file = File::open(&binary)?;
                atma::db::load_binary(io::BufReader::new(file))?
            };
            print!("{}", sim_env.description());
            sim_env.add_breakpoint(Breakpoint::Pc(0xfff7));
            sim_env.add_breakpoint(Breakpoint::Pc(0xfff9));
            loop {
                println!("    {}", sim_env.disassemble(sim_env.pc()).1);
                match sim_env.step() {
                    Ok(()) => {}
                    Err(SimBreak::Breakpoint(breakpoint)) => {
                        println!("Breakpoint: {:?}", breakpoint);
                        break;
                    }
                    Err(SimBreak::HaltOpcode(mnemonic, opcode)) => {
                        println!(
                            "Halted by {} opcode ${:02x}",
                            mnemonic, opcode
                        );
                        break;
                    }
                }
            }
        }
    }
    Ok(())
}

//===========================================================================//
