mod assembler;
mod tests;

use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <input_file> [debug_level]", args[0]);
        eprintln!("  debug_level: 1-3 (higher = more verbose)");
        std::process::exit(1);
    }

    let input_file = &args[1];

    let debug_level = if args.len() >= 3 {
        args[2].parse().unwrap_or(0)
    } else {
        0
    };
    assembler::set_debug_level(debug_level);

    if let Err(e) = assembler::assemble(input_file.clone()) {
        eprintln!("Error assembling {}: {}", input_file, e);
        std::process::exit(1);
    }
}
