/* 
  ArmAsm - A very minimal subset of the ARM-A-Profile ISA instructions.

  The purpose of the project was to learn assembly by doing it my next few things are as follows.

  (A subset of) ArmAsm Instructions -> ArmCore (Verilog) -> ArmCore running on Actix 7 - To learn cpu design and understand limitations of the cpu.

  ArmAsm -> ArmLd -> runnable on apple silicon - To learn ELF and macho and so on and so on.

  I also want to do something comparable for the GPU as it seems like they may be the most important commodity in human history soon enough.

  Note: Not even all the parsed instructions are actually encoded yet or ever.

  By 1957-001B

*/


use core::panic;
use std::{
    collections::HashMap,
    fs::File,
    io::{self, BufWriter, Read, Write},
};

#[derive(Debug, PartialEq)]
enum Instruction {
    // Subset of ARM A Profile Architecture
    MOVK {
        rd: Operand,
        imm: Operand,
        shift: Operand,
    },
    MOVZ {
        rd: Operand,
        imm: Operand,
        shift: Operand,
    },
    ORR {
        dest: Operand,
        src1: Operand,
        src2: Operand,
    },
    ADD {
        dest: Operand,
        src1: Operand,
        src2: Operand,
    },
    B {
        addr: Operand,
    },
    LDR {
        rt: Operand,
        label: Operand,
    },
    STUR {
        src: Operand,
        addr: Operand,
    },
    SVC {
        syscall: Operand,
    },
}

#[derive(Debug, PartialEq)]
enum Operand {
    Reg(Reg),
    Imm(ImmType), // TODO implement Uimm (unisgned) and Simm (signed) for use with ADD and B relative adressing
}

#[derive(Debug, PartialEq)]
enum ImmType {
    Unsigned(u64),
    Unsigned16(u16),
    Address(u64),
    UnresolvedSymbol(String),
}

#[derive(Debug, Clone)]
struct State {
    current_addr: u64,
    labels: HashMap<String, u64>,
    unresolved_refs: Vec<(String, usize)>,
    current_section: String,
    data: Vec<u8>,
}
impl State {
    fn new() -> Self {
        State {
            labels: HashMap::new(),
            current_addr: 0,
            unresolved_refs: Vec::new(),
            current_section: ".text".to_string(),
            data: Vec::new(),
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
#[repr(u32)]
enum Reg {
    // keeping only 64 bit registers for simplicity easy to implement 32 bit
    X0 = 0,
    X1 = 1,
    X2 = 2,
    X3 = 3,
    X4 = 4,
    X5 = 5,
    X6 = 6,
    X7 = 7,
    X8 = 8,
    X9 = 9,
    X10 = 10,
    X11 = 11,
    X12 = 12,
    X13 = 13,
    X14 = 14,
    X15 = 15,
    X16 = 16,
    X17 = 17,
    X18 = 18,
    X19 = 19,
    X20 = 20,
    X21 = 21,
    X22 = 22,
    X23 = 23,
    X24 = 24,
    X25 = 25,
    X26 = 26,
    X27 = 27,
    X28 = 28,
    X29 = 29,
    X30 = 30,
    XZR = 31,
    // needs implementing properly SP = 31,
}

fn parse_register(reg: &str) -> Reg {
    match reg.to_uppercase().as_str() {
        "X0" => Reg::X0,
        "X1" => Reg::X1,
        "X2" => Reg::X2,
        "X3" => Reg::X3,
        "X4" => Reg::X4,
        "X5" => Reg::X5,
        "X6" => Reg::X6,
        "X7" => Reg::X7,
        "X8" => Reg::X8,
        "X9" => Reg::X9,
        "X10" => Reg::X10,
        "X11" => Reg::X11,
        "X12" => Reg::X12,
        "X13" => Reg::X13,
        "X14" => Reg::X14,
        "X15" => Reg::X15,
        "X16" => Reg::X16,
        "X17" => Reg::X17,
        "X18" => Reg::X18,
        "X19" => Reg::X19,
        "X20" => Reg::X20,
        "X21" => Reg::X21,
        "X22" => Reg::X22,
        "X23" => Reg::X23,
        "X24" => Reg::X24,
        "X25" => Reg::X25,
        "X26" => Reg::X26,
        "X27" => Reg::X27,
        "X28" => Reg::X28,
        "X29" => Reg::X29,
        "X30" => Reg::X30,
        "XZR" => Reg::XZR,
        "PC" => panic!("PC is not a valid register"),

        _ => panic!("Not a valid register"),
    }
}

fn parse_directive(directive: &str, state: &mut State) -> Option<Vec<u8>> {
/*
Currently non-functional at this time I am writing a minimal cpu which will not recognise these anyways .
*/ 

let parts: Vec<&str> = directive.split_whitespace().collect();
    match parts[0] {
        ".data" => {
            state.current_section = ".data".to_string();
            None
        }
        ".text" => {
            state.current_section = ".text".to_string();
            None
        }
        ".asciz" => {
            let s = directive.split('"').nth(1).unwrap_or("");
            let mut bytes = s.as_bytes().to_vec();
            bytes.push(0); // Null terminator
            Some(bytes)
        }

        _ => None,
    }
}

fn parse_line(line: &str, state: &mut State) -> Option<Instruction> {

    // Determine if a label
    if line.ends_with(':') {
        state.labels.insert(
            line.strip_suffix(':').unwrap().to_string(),
            state.current_addr,
        );
        return None;
    }

    // Determine if directive
    if line.starts_with('.') {
        if let Some(bytes) = parse_directive(line, state) {
            state.data.extend(bytes);
        }
        return None;
    }

    // Split and clean the line
    let mut parts: Vec<String> = line
        .split(|c: char| c == ',' || c.is_whitespace())
        .filter(|s| !s.is_empty())
        .map(|s| s.to_string())
        .collect();

    // Uppercase the opcode
    if let Some(first) = parts.first_mut() {
        *first = first.to_uppercase().to_string();
    }
    
    // Vec<String --> &Str>
    let parts: Vec<&str> = parts.iter().map(|s| s.as_str()).collect();
    // Match the instruction and add it to the tree
    let inst = match parts.as_slice() {
        // Unshifted
        ["MOVK", rd, imm] => dbg!(Some(Instruction::MOVK {
            rd: Operand::Reg(parse_register(rd.trim_end_matches(','))),
            imm: Operand::Imm(ImmType::Unsigned16(
                u16::from_str_radix(imm.strip_prefix("#0x").unwrap().trim_end_matches(','), 16)
                    .unwrap(),
            )),
            shift: Operand::Imm(ImmType::Unsigned16(0)),
        }
    )),

        //Shifted (not fully implemented)
        ["MOVK", rd, imm, "LSL", shift] => Some(Instruction::MOVK {
            rd: Operand::Reg(parse_register(rd.trim_end_matches(','))),
            imm: Operand::Imm(ImmType::Unsigned16(
                u16::from_str_radix(imm.strip_prefix("#0x").unwrap().trim_end_matches(','), 16)
                    .unwrap(),
            )),
            shift: {
                let shift_value = shift.strip_prefix('#').unwrap();
                Operand::Imm(ImmType::Unsigned16(shift_value.parse::<u16>().unwrap()))
            },
        }),
        
        // Unshifted
        ["MOVZ", rd, imm] => Some(Instruction::MOVZ {
            rd: Operand::Reg(parse_register(rd.trim_end_matches(','))),
            imm: Operand::Imm(ImmType::Unsigned16(
                u16::from_str_radix(imm.strip_prefix("#0x").unwrap().trim_end_matches(','), 16)
                    .unwrap(),
            )),
            shift: Operand::Imm(ImmType::Unsigned16(0)),
        }),

        //Shifted
        ["MOVZ", rd, imm, "LSL", shift] => Some(Instruction::MOVZ {
            rd: Operand::Reg(parse_register(rd.trim_end_matches(','))),
            imm: Operand::Imm(ImmType::Unsigned16(
                u16::from_str_radix(imm.strip_prefix("#0x").unwrap().trim_end_matches(','), 16)
                    .unwrap(),
            )),
            shift: {
                let shift_value = shift.strip_prefix('#').unwrap();
                Operand::Imm(ImmType::Unsigned16(shift_value.parse::<u16>().unwrap()))
            },
        }),
        // This is the instruction that MOVZ X0, X1 actually aliases to as we can use X31/XZR to do ORR X0, XZR, X1 shouldn't accept non-register operands
        ["ORR", dest, src1, src2] => Some(Instruction::ORR {
            dest: Operand::Reg(parse_register(dest.trim_end_matches(','))),
            src1: if src1.starts_with('X') {
                Operand::Reg(parse_register(src1.trim_end_matches(',')))
            } else {
              panic!("ORR only supports register movements in this assembler");
            },

            src2: if src2.starts_with('X') {
                Operand::Reg(parse_register(src2.trim_end_matches(',')))
            } else {
              panic!("ORR only supports register movements in this assembler");
            },
        }),

        // https://developer.arm.com/documentation/ddi0602/2024-12/Base-Instructions/ADD--immediate---Add-immediate-value-?lang=en or register???
        ["ADD", dest, src1, src2] => Some(Instruction::ADD {
            dest: Operand::Reg(parse_register(dest.trim_end_matches(','))),
            src1: if src1.starts_with('X') {
                Operand::Reg(parse_register(src1.trim_end_matches(',')))
            } else {
                Operand::Imm(ImmType::Unsigned(
                    src1.trim_end_matches(',').parse().unwrap(),
                ))
            },

            src2: if src2.starts_with('X') {
                Operand::Reg(parse_register(src2.trim_end_matches(',')))
            } else {
                Operand::Imm(ImmType::Unsigned(
                    src2.trim_end_matches(',').parse().unwrap(),
                ))
            },
        }),

        // https://developer.arm.com/documentation/ddi0602/2024-12/Base-Instructions/LDR--immediate---Load-register--immediate--?lang=en#XnSP_option
        ["LDR", rt, label] => Some(Instruction::LDR {
            rt: Operand::Reg(parse_register(rt.trim_end_matches(','))),
            label: if label.starts_with('=') {
                if label.starts_with("0x") {
                    Operand::Imm(ImmType::Address(
                        u64::from_str_radix(label.strip_prefix("0x").unwrap(), 16).unwrap_or(0),
                    ))
                } else {
                    let symbol_name = label.strip_prefix('=').unwrap().to_string();
                    let unref = Operand::Imm(ImmType::UnresolvedSymbol(symbol_name.clone()));
                    state
                        .unresolved_refs
                        .push((symbol_name, state.current_addr.try_into().unwrap()));
                    unref
                }
            } else {
                panic!("Literal Addr Only")
            },
        }),

        // https://developer.arm.com/documentation/ddi0602/2024-12/Base-Instructions/STR--immediate---Store-register--immediate--?lang=en
        ["STUR", dest, addr] => Some(Instruction::STUR {
            src: Operand::Reg(parse_register(dest.trim_end_matches(','))),
            addr: if addr.starts_with('=') {
                Operand::Imm(ImmType::Address(
                    u64::from_str_radix(addr.trim_start_matches("=0x"), 16).unwrap(),
                ))
            } else {
                panic!("Only literal addressing supported");
            },
        }),

        // https://developer.arm.com/documentation/ddi0602/2024-12/Base-Instructions/B--Branch-?lang=en
        ["B", addr] => Some(Instruction::B {
            addr: if addr.starts_with("0x") {
                Operand::Imm(ImmType::Address(
                    u64::from_str_radix(addr.trim_start_matches("0x"), 16).unwrap(),
                ))
            } else if addr.starts_with('.') {
                Operand::Imm(ImmType::Unsigned(0))
            } else {
                panic!("Invalid Address")
            },
        }),

        // https://developer.arm.com/documentation/ddi0602/2024-12/Base-Instructions/SVC--Supervisor-call-?lang=en
        ["SVC", syscall] => Some(Instruction::SVC {
            syscall: if syscall.starts_with('#') {
                Operand::Imm(ImmType::Unsigned16(
                    u16::from_str_radix(
                        syscall.trim_start_matches('#').trim_start_matches("0x"),
                        16,
                    )
                    .unwrap(),
                ))
            } else {
                panic!("Syntax error # for imm");
            },
        }),

        _ => None,
    };
    if !line.trim_start().starts_with('.') {
        state.current_addr += 4;
    }
    inst
}

fn encode_movk(rd: &Operand, imm: &Operand, shift: &Operand) -> u32 {
    // Extract register number
    let rd_val = match rd {
        Operand::Reg(reg) => *reg as u32,
        _ => panic!("Expected register for rd"),
    };

    // Extract immediate value
    let imm_val = match imm {
        Operand::Imm(ImmType::Unsigned16(val)) => *val as u32,
        _ => panic!("Expected Unsigned16 for imm"),
    };

    // Extract shift amount
    let shift_val = match shift {
        Operand::Imm(ImmType::Unsigned16(val)) => *val as u32,
        _ => panic!("Expected Unsigned16 for shift"),
    };

    // Calculate hw field based on shift
    let hw = match shift_val {
        0 => 0b00,
        16 => 0b01,
        32 => 0b10,
        48 => 0b11,
        _ => panic!("Invalid shift value for MOVK"),
    };

    // Set sf bit (1 for 64-bit, 0 for 32-bit)
    let sf = 1; // 64-bit variant

    // Construct the instruction using the encoding format:
    // sf(1) opc(7) hw(2) imm16(16) Rd(5) op(1)
    let encoded = (sf << 31) |            // sf bit
                 (0b111100101<< 23) |     // opc field
                 (hw << 21) |             // hw field
                 (imm_val << 5) |         // imm16 field
                 rd_val; // Rd field
    println!("Movk encoding: {:0b}", encoded);
    encoded
}


fn encode_movz(rd: &Operand, imm: &Operand, shift: &Operand) -> u32 {
    // Extract register number
    let rd_val = match rd {
        Operand::Reg(reg) => *reg as u32,
        _ => panic!("Expected register for rd"),
    };

    // Extract immediate value
    let imm_val = match imm {
        Operand::Imm(ImmType::Unsigned16(val)) => *val as u32,
        _ => panic!("Expected Unsigned16 for imm"),
    };

    // Extract shift amount
    let shift_val = match shift {
        Operand::Imm(ImmType::Unsigned16(val)) => *val as u32,
        _ => panic!("Expected Unsigned16 for shift"),
    };

    // Calculate hw field based on shift
    let hw = match shift_val {
        0 => 0b00,
        16 => 0b01,
        32 => 0b10,
        48 => 0b11,
        _ => panic!("Invalid shift value for MOVK"),
    };

    // Set sf bit (1 for 64-bit, 0 for 32-bit)
    let sf = 1; // 64-bit variant

    // Construct the instruction using the encoding format:
    // sf(1) opc(7) hw(2) imm16(16) Rd(5) op(1)
    let encoded = (sf << 31) |            // sf bit
                 (0b010100101<< 23) |     // opc field
                 (hw << 21) |             // hw field
                 (imm_val << 5) |         // imm16 field
                 rd_val; // Rd field

    println!("Movz encoding: {:0b}", encoded);
    encoded
}
fn encode_ldr(rt: &Operand) -> u32 {
    // Extract register number Rt
    let rt = match rt {
        Operand::Reg(reg) => *reg as u32,
        _ => panic!("Expected register for rd"),
    };

    // For LDR literal (PC-relative)
    // Opcode is 0x58 (01011000) for 64-bit variant
    // Zero out the imm19 field - it will be filled in during the second pass

    let opc = 1;
    let sf  = 1;
    let encoded = (sf << 31) |
                (0b00011000 << 24) |  // Fixed bits for LDR literal
                (opc << 30) |          // opc field (01 for 64-bit variant)
                (0 << 5) |             // imm19 field (zeroed out)
                (rt); // Rt field

    encoded
}
fn encode_svc(syscall: &Operand) -> u32 {
    // Extract register number Rt
    let syscall: u16 = match syscall {
        Operand::Imm(ImmType::Unsigned16(syscall)) => *syscall as u16,
        _ => panic!("promlem"),
    };

    // For LDR literal (PC-relative)
    // Opcode is 0x58 (01011000) for 64-bit variant
    // Zero out the imm19 field - it will be filled in during the second pass

    let encoded: u32 = (0b11010100000 << 21) | ((syscall as u32) << 5) | 0b1;

    encoded
}

fn resolve_address(encoding: u32, state: &State) -> u32 {
    // +/-1MB, is encoded as "imm19" times 4.
    if let Some((symbol, instr_addr)) = state.unresolved_refs.last() {
        if let Some(&targ_addr) = state.labels.get(symbol) {
            let pc = instr_addr + 8;

            let offset = (targ_addr as i64) - (pc as i64);

            assert!(
                offset >= -1048576 && offset <= 1048575,
                "LDR offset out of range"
            ); //  +/-1MB?

            let imm19 = ((offset >> 2) & 0x7FFFF) as u32;

            let resolved_encoding: u32 = (encoding & 0xFF00001F) | (imm19 << 5);

            return resolved_encoding;
        }
    }
    encoding
}

fn encode_line(op: &Instruction, _state: &mut State) -> u32 {
    match op {
        Instruction::MOVK { rd, imm, shift } => encode_movk(&rd, &imm, shift),
        Instruction::MOVZ { rd, imm, shift } => encode_movz(&rd, &imm, &shift),
        Instruction::LDR { rt, label: _ } => encode_ldr(&rt),
        Instruction::SVC { syscall } => encode_svc(syscall),

        _ => 0,
    }
}
fn assemble(path: String) -> io::Result<()> {
    let mut file = File::open(path)?;
    let mut contents = String::new();

    file.read_to_string(&mut contents)?;

    let contents: Vec<&str> = contents
        .split('\n')
        .map(|l| l.trim())
        .filter(|l| !l.starts_with("//") && !l.trim().is_empty())
        .collect();

    let mut state = State::new();

    let mut parsed = Vec::new();
    // first pass
    for l in &contents {
        if let Some(instruction) = parse_line(l, &mut state) {
            parsed.push(instruction);
        }
    }

    let mut encoded: Vec<u32> = Vec::new();

    //second pass
    for (_l, instruction) in parsed.iter().enumerate() {
        let mut encoded_line: u32 = encode_line(instruction, &mut state);

        match instruction {
            Instruction::LDR { rt: _, label } => {
                if let Operand::Imm(ImmType::UnresolvedSymbol(_symbol)) = label {
                    encoded_line = resolve_address(encoded_line, &state);
                }
            }
            _ => {}
        }
        // println!(
        //     "{:?} : {:0b} - {:0x}",
        //     debug_contents[l], &encoded_line, &encoded_line
        // );

        encoded.push(encoded_line);
    }

    let file = File::create("output.bin").expect("Failed to create file");

    let mut writer = BufWriter::new(file);

    for instruction in &encoded {
        let bytes = instruction.to_le_bytes();

        writer.write_all(&bytes)?;
    }

    let current_pos = encoded.len() * 4; // Each instruction is 4 bytes
    let padding = (8 - (current_pos % 8)) % 8;
    for _ in 0..padding {
        writer.write_all(&[0])?;
    }

    writer.write_all(&state.data)?;

    return Ok(());
}
fn main() {
    let _ = assemble("./as.s".to_string());
    // println!("{:#?}",a)
}

// tests

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;
    use std::io::Read;

    #[test]
    fn test_asm() -> io::Result<()> {
        // Assemble the source file
        assemble("./example/goal.s".to_string())?;

        // Read the expected output
        let mut file = File::open("./example/output.bin")?;
        let mut expected = Vec::new();
        file.read_to_end(&mut expected)?;

        // Read the actual output
        let mut actual_file = File::open("output.bin")?;
        let mut actual = Vec::new();
        actual_file.read_to_end(&mut actual)?;

        // Compare binary outputs
        assert_eq!(actual, expected);
        Ok(())
    }
}
/* 
Congratulations you reached the end of this mess some variable names are wrong because it was written so fast that I just copy pasted my own boilerplate and sometimes didn't even change the variable names or comments.
I wrote this from 7pm to 3:30am in one night ~9 hours and about 6 hours the following week here and there I learnt alot
*/

