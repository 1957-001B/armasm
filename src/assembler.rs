use std::{
    collections::HashMap,
    fs::File,
    io::{self, BufWriter, Read, Write},
    sync::atomic::{AtomicU8, Ordering},
    time::Instant,
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
    /// General-purpose register encoding number (X0=0 … X30=30, XZR=31).
    Reg(u32),
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

/// Parse an X-register name into its 5-bit encoding (0–31).
/// Accepts `X0`–`X30`, `X31`/`XZR`. `PC` is rejected.
pub fn parse_register(reg: &str) -> u32 {
    let reg = reg.trim_end_matches(',').to_uppercase();
    if reg == "PC" {
        panic!("PC is not a valid register");
    }
    if reg == "XZR" || reg == "X31" {
        return 31;
    }
    let num = reg
        .strip_prefix('X')
        .unwrap_or_else(|| panic!("Not a valid register: {reg}"));
    let n: u32 = num
        .parse()
        .unwrap_or_else(|_| panic!("Not a valid register: {reg}"));
    if n > 30 {
        panic!("Not a valid register: {reg}");
    }
    n
}

/// Shared operand parse for move-wide (MOVK / MOVZ): `Rd, #0ximm` [`, LSL #n`].
fn parse_mov_wide(rd: &str, imm: &str, shift: Option<&str>) -> (Operand, Operand, Operand) {
    let rd = Operand::Reg(parse_register(rd));
    let imm = Operand::Imm(ImmType::Unsigned16(
        u16::from_str_radix(imm.strip_prefix("#0x").unwrap().trim_end_matches(','), 16).unwrap(),
    ));
    let shift = match shift {
        Some(s) => {
            let shift_value = s.strip_prefix('#').unwrap();
            Operand::Imm(ImmType::Unsigned16(shift_value.parse::<u16>().unwrap()))
        }
        None => Operand::Imm(ImmType::Unsigned16(0)),
    };
    (rd, imm, shift)
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
    if get_debug_level() >= 2 {
        eprintln!("{}", line);
    }

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
        ["MOVK", rd, imm] => {
            let (rd, imm, shift) = parse_mov_wide(rd, imm, None);
            Some(Instruction::MOVK { rd, imm, shift })
        }
        ["MOVK", rd, imm, "LSL", shift] => {
            let (rd, imm, shift) = parse_mov_wide(rd, imm, Some(shift));
            Some(Instruction::MOVK { rd, imm, shift })
        }
        ["MOVZ", rd, imm] => {
            let (rd, imm, shift) = parse_mov_wide(rd, imm, None);
            Some(Instruction::MOVZ { rd, imm, shift })
        }
        ["MOVZ", rd, imm, "LSL", shift] => {
            let (rd, imm, shift) = parse_mov_wide(rd, imm, Some(shift));
            Some(Instruction::MOVZ { rd, imm, shift })
        }
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

/// Place `width` low bits of `val` at bit position `lo` (inclusive).
/// Field layouts taken from the A64 encoding diagrams in the Arm A-profile ISA
/// (DDI 0602), mirrored at https://www.scs.stanford.edu/~zyedidia/arm64/
#[inline]
fn put(val: u32, lo: u32, width: u32) -> u32 {
    debug_assert!(width < 32);
    (val & ((1u32 << width) - 1)) << lo
}

fn reg_num(op: &Operand) -> u32 {
    match op {
        Operand::Reg(n) => *n,
        _ => panic!("expected register operand, got {op:?}"),
    }
}

fn imm_u16(op: &Operand) -> u32 {
    match op {
        Operand::Imm(ImmType::Unsigned16(v)) => *v as u32,
        _ => panic!("expected Unsigned16 immediate, got {op:?}"),
    }
}

fn imm_u64(op: &Operand) -> u64 {
    match op {
        Operand::Imm(ImmType::Unsigned(v)) => *v,
        Operand::Imm(ImmType::Unsigned16(v)) => *v as u64,
        Operand::Imm(ImmType::Address(v)) => *v,
        _ => panic!("expected numeric immediate, got {op:?}"),
    }
}

/// hw field for move-wide: shift amount {0,16,32,48} → {0,1,2,3}.
fn mov_hw(shift: &Operand) -> u32 {
    match imm_u16(shift) {
        0 => 0b00,
        16 => 0b01,
        32 => 0b10,
        48 => 0b11,
        s => panic!("move-wide LSL must be 0/16/32/48, got {s}"),
    }
}

/// MOVK — Move wide with keep (64-bit).
/// Diagram: sf | opc=11 | 100101 | hw | imm16 | Rd
/// https://developer.arm.com/documentation/ddi0602/2025-12/Base-Instructions/MOVK--Move-wide-with-keep-
fn encode_movk(rd: &Operand, imm: &Operand, shift: &Operand) -> u32 {
    let sf = 1u32; // 64-bit (X registers)
    // bits [30:23] = 11100101  (opc=11, fixed 100101)
    put(sf, 31, 1)
        | put(0b11100101, 23, 8)
        | put(mov_hw(shift), 21, 2)
        | put(imm_u16(imm), 5, 16)
        | put(reg_num(rd), 0, 5)
}

/// MOVZ — Move wide with zero (64-bit).
/// Diagram: sf | opc=10 | 100101 | hw | imm16 | Rd
/// https://developer.arm.com/documentation/ddi0602/2025-12/Base-Instructions/MOVZ--Move-wide-with-zero-
fn encode_movz(rd: &Operand, imm: &Operand, shift: &Operand) -> u32 {
    let sf = 1u32;
    // bits [30:23] = 10100101  (opc=10, fixed 100101)
    put(sf, 31, 1)
        | put(0b10100101, 23, 8)
        | put(mov_hw(shift), 21, 2)
        | put(imm_u16(imm), 5, 16)
        | put(reg_num(rd), 0, 5)
}

/// ORR (shifted register) — 64-bit, no shift applied (LSL #0).
/// Diagram: sf | opc=01 | 01010 | shift | N=0 | Rm | imm6 | Rn | Rd
/// https://developer.arm.com/documentation/ddi0602/2025-12/Base-Instructions/ORR--shifted-register---Bitwise-OR--shifted-register--
fn encode_orr(rd: &Operand, rn: &Operand, rm: &Operand) -> u32 {
    let sf = 1u32;
    // bits [30:24] = 0101010  (opc=01, fixed 01010)
    put(sf, 31, 1)
        | put(0b0101010, 24, 7)
        | put(0b00, 22, 2) // shift = LSL
        | put(0, 21, 1) // N
        | put(reg_num(rm), 16, 5)
        | put(0, 10, 6) // imm6 = 0
        | put(reg_num(rn), 5, 5)
        | put(reg_num(rd), 0, 5)
}

/// ADD (immediate) — 64-bit, sh=0 (unshifted imm12).
/// Diagram: sf | op=0 | S=0 | 100010 | sh | imm12 | Rn | Rd
/// https://developer.arm.com/documentation/ddi0602/2025-12/Base-Instructions/ADD--immediate---Add--immediate--
fn encode_add_imm(rd: &Operand, rn: &Operand, imm: u32) -> u32 {
    assert!(imm < (1 << 12), "ADD imm12 out of range: {imm}");
    let sf = 1u32;
    // bits [30:23] = 00100010
    put(sf, 31, 1)
        | put(0b00100010, 23, 8)
        | put(0, 22, 1) // sh
        | put(imm, 10, 12)
        | put(reg_num(rn), 5, 5)
        | put(reg_num(rd), 0, 5)
}

/// ADD (shifted register) — 64-bit, LSL #0.
/// Diagram: sf | op=0 | S=0 | 01011 | shift | 0 | Rm | imm6 | Rn | Rd
/// https://developer.arm.com/documentation/ddi0602/2025-12/Base-Instructions/ADD--shifted-register---Add--shifted-register--
fn encode_add_reg(rd: &Operand, rn: &Operand, rm: &Operand) -> u32 {
    let sf = 1u32;
    // bits [30:24] = 0001011
    put(sf, 31, 1)
        | put(0b0001011, 24, 7)
        | put(0b00, 22, 2) // shift = LSL
        | put(0, 21, 1)
        | put(reg_num(rm), 16, 5)
        | put(0, 10, 6) // imm6
        | put(reg_num(rn), 5, 5)
        | put(reg_num(rd), 0, 5)
}

/// B — Unconditional branch (immediate).
/// Diagram: op=0 | 00101 | imm26
/// https://developer.arm.com/documentation/ddi0602/2025-12/Base-Instructions/B--Branch-
/// `target` and `pc` are byte addresses; imm26 encodes (target-pc)/4.
fn encode_b(target: u64, pc: u64) -> u32 {
    let offset = target as i64 - pc as i64;
    assert!(offset % 4 == 0, "B target not word-aligned");
    let imm26 = offset >> 2;
    assert!(
        (-(1 << 25)..(1 << 25)).contains(&imm26),
        "B offset out of range (±128MB)"
    );
    put(0b000101, 26, 6) | put(imm26 as u32, 0, 26)
}

/// LDR (literal) — 64-bit (opc=01).
/// Diagram: opc | 011 | V=0 | 00 | imm19 | Rt
/// https://developer.arm.com/documentation/ddi0602/2025-12/Base-Instructions/LDR--literal---Load-register--literal--
/// imm19 is left 0 when the target is unresolved; patched by `resolve_ldr_literal`.
fn encode_ldr_literal(rt: &Operand, imm19: u32) -> u32 {
    assert!(imm19 < (1 << 19));
    // opc=01 (64-bit), bits [29:24] = 011000
    put(0b01, 30, 2) | put(0b011000, 24, 6) | put(imm19, 5, 19) | put(reg_num(rt), 0, 5)
}

/// STUR — Store register (unscaled), 64-bit (size=11).
/// Diagram: size | 111 | V=0 | 00 | 0 | imm9 | 00 | Rn | Rt
/// https://developer.arm.com/documentation/ddi0602/2025-12/Base-Instructions/STUR--Store-register--unscaled--
/// Parser only has `src` + absolute `=addr`; we encode as STUR Xt, [XZR, #imm9]
/// when `addr` fits in a signed 9-bit unscaled offset.
fn encode_stur(rt: &Operand, addr: &Operand) -> u32 {
    let rt = reg_num(rt);
    let offset = match addr {
        Operand::Imm(ImmType::Address(a)) => *a as i64,
        Operand::Imm(ImmType::Unsigned(a)) => *a as i64,
        _ => panic!("STUR expected address immediate, got {addr:?}"),
    };
    assert!(
        (-256..256).contains(&offset),
        "STUR imm9 out of range (-256..255): {offset}"
    );
    let imm9 = (offset as u32) & 0x1FF;
    let rn = 31u32; // XZR — no base register in current AST
    // size=11, bits [29:21] = 111000000  → [31:21] = 11111000000
    put(0b11111000000, 21, 11)
        | put(imm9, 12, 9)
        | put(0b00, 10, 2)
        | put(rn, 5, 5)
        | put(rt, 0, 5)
}

/// SVC — Supervisor call.
/// Diagram: 11010100 | opc=000 | imm16 | op2=000 | LL=01
/// https://developer.arm.com/documentation/ddi0602/2025-12/Base-Instructions/SVC--Supervisor-call-
fn encode_svc(imm: &Operand) -> u32 {
    // bits [31:21] = 11010100000, bits [4:0] = 00001
    put(0b11010100000, 21, 11) | put(imm_u16(imm), 5, 16) | put(0b00001, 0, 5)
}

/// Patch LDR (literal) imm19 once the symbol address is known.
/// A64: offset is from the address of this instruction (not PC+8).
fn resolve_ldr_literal(encoding: u32, instr_addr: usize, targ_addr: u64) -> u32 {
    let pc = instr_addr as i64;
    let offset = targ_addr as i64 - pc;
    assert!(
        offset % 4 == 0 && (-(1 << 20)..(1 << 20)).contains(&offset),
        "LDR literal offset out of range: {offset}"
    );
    let imm19 = ((offset >> 2) as u32) & 0x7FFFF;
    (encoding & 0xFF00_001F) | put(imm19, 5, 19)
}

fn encode(op: &Instruction, state: &State) -> u32 {
    let pc = state.current_addr;
    let encoded = match op {
        Instruction::MOVK { rd, imm, shift } => encode_movk(rd, imm, shift),
        Instruction::MOVZ { rd, imm, shift } => encode_movz(rd, imm, shift),
        Instruction::ORR { dest, src1, src2 } => encode_orr(dest, src1, src2),
        Instruction::ADD { dest, src1, src2 } => match (src1, src2) {
            (Operand::Reg(_), Operand::Reg(_)) => encode_add_reg(dest, src1, src2),
            (Operand::Reg(_), Operand::Imm(_)) => encode_add_imm(dest, src1, imm_u64(src2) as u32),
            _ => panic!("ADD expects Rn reg and Rm reg or imm12, got {src1:?}, {src2:?}"),
        },
        Instruction::B { addr } => match addr {
            Operand::Imm(ImmType::Address(t)) => encode_b(*t, pc),
            // `B .` — parser stores Unsigned(0) as "branch to self"
            Operand::Imm(ImmType::Unsigned(0)) => encode_b(pc, pc),
            _ => panic!("B expected address, got {addr:?}"),
        },
        Instruction::LDR { rt, label } => {
            let imm19 = match label {
                Operand::Imm(ImmType::UnresolvedSymbol(_)) => 0u32,
                Operand::Imm(ImmType::Address(t)) => {
                    let offset = *t as i64 - pc as i64;
                    assert!(offset % 4 == 0 && (-(1 << 20)..(1 << 20)).contains(&offset));
                    ((offset >> 2) as u32) & 0x7FFFF
                }
                _ => panic!("LDR expected literal label/address, got {label:?}"),
            };
            encode_ldr_literal(rt, imm19)
        }
        Instruction::STUR { src, addr } => encode_stur(src, addr),
        Instruction::SVC { syscall } => encode_svc(syscall),
    };

    if get_debug_level() >= 2 {
        eprintln!("{:?} @{:#x} -> {:08x}", op, pc, encoded);
    }
    encoded
}

pub fn assemble(path: String) -> io::Result<()> {
    let start = Instant::now();

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
    if get_debug_level() >= 2 {
        eprintln!("\n\n");
    }

    let mut encoded: Vec<u32> = Vec::new();

    // Re-walk PC for PC-relative forms (B, LDR literal).
    state.current_addr = 0;
    // unresolved_refs: (symbol, instr_addr) collected at parse time
    let mut unres = state.unresolved_refs.clone();
    unres.reverse(); // so we can pop matching LDRs in order if needed

    for instruction in &parsed {
        let mut encoded_line: u32 = encode(instruction, &state);

        if let Instruction::LDR {
            rt: _,
            label: Operand::Imm(ImmType::UnresolvedSymbol(sym)),
        } = instruction
        {
            // Prefer the ref recorded for this PC, else look up the symbol.
            let instr_addr = state.current_addr as usize;
            let targ = state
                .labels
                .get(sym)
                .copied()
                .unwrap_or_else(|| panic!("unresolved symbol: {sym}"));
            // consume matching unresolved_refs entry if present
            if let Some(pos) = unres.iter().position(|(s, a)| s == sym && *a == instr_addr) {
                unres.remove(pos);
            }
            encoded_line = resolve_ldr_literal(encoded_line, instr_addr, targ);
        }

        encoded.push(encoded_line);
        state.current_addr += 4;
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

    eprintln!("Assembled in {:?}", start.elapsed());

    Ok(())
}