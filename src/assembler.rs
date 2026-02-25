use std::{
    collections::HashMap,
    fs::File,
    io::{self, BufWriter, Read, Write},
    sync::atomic::{AtomicU8, Ordering},
};

static DEBUG_LEVEL: AtomicU8 = AtomicU8::new(0);

pub fn set_debug_level(level: u8) {
    DEBUG_LEVEL.store(level, Ordering::SeqCst);
}

pub fn get_debug_level() -> u8 {
    DEBUG_LEVEL.load(Ordering::SeqCst)
}

#[macro_export]
macro_rules! dbg_level {
    ($level:expr, $($arg:tt)*) => {{
        if $crate::assembler::get_debug_level() >= $level {
            let _ = dbg!($(&$arg)*);
        }
    }};
}

#[derive(Debug, PartialEq)]
#[allow(clippy::upper_case_acronyms)]
pub enum Instruction {
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
pub enum Operand {
    Reg(Reg),
    Imm(ImmType),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ImmType {
    Unsigned(u64),
    Unsigned16(u16),
    Address(u64),
    UnresolvedSymbol(String),
}

#[derive(Debug, Clone)]
pub struct State {
    pub current_addr: u64,
    pub labels: HashMap<String, u64>,
    pub unresolved_refs: Vec<(String, usize)>,
    pub current_section: String,
    pub data: Vec<u8>,
}
impl State {
    pub fn new() -> Self {
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
#[allow(clippy::upper_case_acronyms)]
pub enum Reg {
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
}

pub fn parse_register(reg: &str) -> Reg {
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
            bytes.push(0);
            Some(bytes)
        }
        _ => None,
    }
}

pub fn parse_line(line: &str, state: &mut State) -> Option<Instruction> {
    if line.ends_with(':') {
        state.labels.insert(
            line.strip_suffix(':').unwrap().to_string(),
            state.current_addr,
        );
        return None;
    }

    if line.starts_with('.') {
        if let Some(bytes) = parse_directive(line, state) {
            state.data.extend(bytes);
        }
        return None;
    }

    let mut parts: Vec<String> = line
        .split(|c: char| c == ',' || c.is_whitespace())
        .filter(|s| !s.is_empty())
        .map(|s| s.to_string())
        .collect();

    if let Some(first) = parts.first_mut() {
        *first = first.to_uppercase().to_string();
    }

    let parts: Vec<&str> = parts.iter().map(|s| s.as_str()).collect();
    let inst = match parts.as_slice() {
        ["MOVK", rd, imm] => Some(Instruction::MOVK {
            rd: Operand::Reg(parse_register(rd.trim_end_matches(','))),
            imm: Operand::Imm(ImmType::Unsigned16(
                u16::from_str_radix(imm.strip_prefix("#0x").unwrap().trim_end_matches(','), 16)
                    .unwrap(),
            )),
            shift: Operand::Imm(ImmType::Unsigned16(0)),
        }),

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

        ["MOVZ", rd, imm] => Some(Instruction::MOVZ {
            rd: Operand::Reg(parse_register(rd.trim_end_matches(','))),
            imm: Operand::Imm(ImmType::Unsigned16(
                u16::from_str_radix(imm.strip_prefix("#0x").unwrap().trim_end_matches(','), 16)
                    .unwrap(),
            )),
            shift: Operand::Imm(ImmType::Unsigned16(0)),
        }),

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

        ["ADD", dest, src1, src2] => Some(Instruction::ADD {
            dest: Operand::Reg(parse_register(dest.trim_end_matches(','))),
            src1: if src1.starts_with('X') {
                Operand::Reg(parse_register(src1.trim_end_matches(',')))
            } else {
                let val = src1.trim_start_matches('#').trim_end_matches(',');
                Operand::Imm(ImmType::Unsigned(val.parse().unwrap()))
            },

            src2: if src2.starts_with('X') {
                Operand::Reg(parse_register(src2.trim_end_matches(',')))
            } else {
                let val = src2.trim_start_matches('#').trim_end_matches(',');
                Operand::Imm(ImmType::Unsigned(val.parse().unwrap()))
            },
        }),

        ["LDR", rt, label] => Some(Instruction::LDR {
            rt: Operand::Reg(parse_register(rt.trim_end_matches(','))),
            label: if label.starts_with('=') {
                let label_inner = label.strip_prefix('=').unwrap();
                if label_inner.starts_with("0x") {
                    Operand::Imm(ImmType::Address(
                        u64::from_str_radix(label_inner.strip_prefix("0x").unwrap(), 16)
                            .unwrap_or(0),
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
    let rd_val = match rd {
        Operand::Reg(reg) => *reg as u32,
        _ => panic!("Expected register for rd"),
    };

    let imm_val = match imm {
        Operand::Imm(ImmType::Unsigned16(val)) => *val as u32,
        _ => panic!("Expected Unsigned16 for imm"),
    };

    let shift_val = match shift {
        Operand::Imm(ImmType::Unsigned16(val)) => *val as u32,
        _ => panic!("Expected Unsigned16 for shift"),
    };

    let hw = match shift_val {
        0 => 0b00,
        16 => 0b01,
        32 => 0b10,
        48 => 0b11,
        _ => panic!("Invalid shift value for MOVK"),
    };

    let sf = 1;

    (sf << 31) | (0b111100101 << 23) | (hw << 21) | (imm_val << 5) | rd_val
}

fn encode_movz(rd: &Operand, imm: &Operand, shift: &Operand) -> u32 {
    let rd_val = match rd {
        Operand::Reg(reg) => *reg as u32,
        _ => panic!("Expected register for rd"),
    };

    let imm_val = match imm {
        Operand::Imm(ImmType::Unsigned16(val)) => *val as u32,
        _ => panic!("Expected Unsigned16 for imm"),
    };

    let shift_val = match shift {
        Operand::Imm(ImmType::Unsigned16(val)) => *val as u32,
        _ => panic!("Expected Unsigned16 for shift"),
    };

    let hw = match shift_val {
        0 => 0b00,
        16 => 0b01,
        32 => 0b10,
        48 => 0b11,
        _ => panic!("Invalid shift value for MOVK"),
    };

    let sf = 1;

    (sf << 31) | (0b010100101 << 23) | (hw << 21) | (imm_val << 5) | rd_val
}

fn encode_ldur(rt: &Operand) -> u32 {
    let rt = match rt {
        Operand::Reg(reg) => *reg as u32,
        _ => panic!("Expected register for rd"),
    };

    // LDUR literal 64-bit: 0xD8 << 24 | rt (signed load)
        let opc = 1;
        let sf = 1;
        (sf << 31) | (0b00011000 << 24) | (opc << 30) | rt
}

fn encode_svc(syscall: &Operand) -> u32 {
    let syscall = match syscall {
        Operand::Imm(ImmType::Unsigned16(syscall)) => *syscall,
        _ => panic!("Expected Unsigned16 for syscall"),
    };

    (0b11010100000 << 21) | ((syscall as u32) << 5) | 0b1
}

fn resolve_address(encoding: u32, state: &State) -> u32 {
    if let Some((symbol, instr_addr)) = state.unresolved_refs.last() {
        if let Some(&targ_addr) = state.labels.get(symbol) {
            let pc = instr_addr + 8;

            let offset = (targ_addr as i64) - (pc as i64);

            assert!(
                (-1048576..=1048575).contains(&offset),
                "LDR offset out of range"
            );

            let imm19 = ((offset >> 2) & 0x7FFFF) as u32;

            let resolved_encoding: u32 = (encoding & 0xFF00001F) | (imm19 << 5);

            return resolved_encoding;
        }
    }
    encoding
}

fn encode_line(op: &Instruction, _state: &mut State) -> u32 {
    let encoded = match op {
        Instruction::MOVK { rd, imm, shift } => encode_movk(rd, imm, shift),
        Instruction::MOVZ { rd, imm, shift } => encode_movz(rd, imm, shift),
        Instruction::LDR { rt, label: _ } => encode_ldur(rt),
        Instruction::SVC { syscall } => encode_svc(syscall),
        _ => 0,
    };
    if get_debug_level() >= 2 {
        eprintln!("Encoded instruction: {:08x}", encoded);
    }
    encoded
}

pub fn assemble(path: String) -> io::Result<()> {
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
    for l in &contents {
        if let Some(instruction) = parse_line(l, &mut state) {
            parsed.push(instruction);
        }
    }

    let mut encoded: Vec<u32> = Vec::new();

    for instruction in &parsed {
        let mut encoded_line: u32 = encode_line(instruction, &mut state);

        if let Instruction::LDR {
            rt: _,
            label: Operand::Imm(ImmType::UnresolvedSymbol(_)),
        } = instruction
        {
            encoded_line = resolve_address(encoded_line, &state);
        }

        encoded.push(encoded_line);
    }

    let file = File::create("output.bin").expect("Failed to create file");

    let mut writer = BufWriter::new(file);

    for instruction in &encoded {
        let bytes = instruction.to_le_bytes();

        writer.write_all(&bytes)?;
    }

    let current_pos = encoded.len() * 4;
    let padding = (8 - (current_pos % 8)) % 8;
    for _ in 0..padding {
        writer.write_all(&[0])?;
    }

    writer.write_all(&state.data)?;

    Ok(())
}
