#[cfg(test)]
mod tests {
    use crate::assembler::{ImmType::*, Instruction::*, Operand::*, *};
    use std::fs;
    #[test]
    fn assembles_example() -> std::io::Result<()> {
        assemble("./example/goal.s".into())?;
        assert_eq!(fs::read("output.bin")?, fs::read("example/output.bin")?);
        Ok(())
    }

    #[test]
    fn encodes_instructions() {
        // Fixed A64 words from the DDI 0602 layouts, independent of the encoder.
        for (source, expected) in [
            ("MOVZ X0, #0xABCD", 0xD295_79A0),
            ("MOVZ X1, #0xFF, LSL #32", 0xD2C0_1FE1),
            ("MOVK X8, #0x1234", 0xF282_4688),
            ("MOVK X8, #0x1234, LSL #16", 0xF2A2_4688),
            ("SVC #0x0", 0xD400_0001),
            ("SVC #0x4", 0xD400_0081),
            ("ORR X0, X1, X2", 0xAA02_0020),
            ("ADD X0, X1, X2", 0x8B02_0020),
            ("ADD X0, X1, #42", 0x9100_A820),
            // Current STUR syntax uses SP as the implicit base.
            ("STUR X0, =0x0", 0xF800_03E0),
        ] {
            let mut state = State::new();
            let instruction = parse_line(source, &mut state).expect(source);
            assert_eq!(encode_line(&instruction, &state), expected, "{source}");
        }
    }

    #[test]
    fn encodes_ldr_opcode_before_relocation() {
        // Only checks the opcode and Rt; encode_line leaves imm19 unresolved.
        let mut state = State::new();
        let instruction = parse_line("LDR X1, =message", &mut state).unwrap();
        assert_eq!(encode_line(&instruction, &state), 0x5800_0001);
    }

    #[test]
    fn encodes_pc_relative_instructions() {
        for (source, expected) in [
            ("B .", 0x1400_0000),
            ("B 0x1008", 0x1400_0002),
            ("B 0xFF8", 0x17FF_FFFE),
            ("LDR X1, =0x1008", 0x5800_0041),
            ("LDR X1, =0xFF8", 0x58FF_FFC1),
        ] {
            let mut state = State::new();
            let instruction = parse_line(source, &mut state).expect(source);
            // Encoding uses the instruction's address, not the parser's next PC.
            state.current_addr = 0x1000;
            assert_eq!(encode_line(&instruction, &state), expected, "{source}");
        }
    }

    #[test]
    fn parses_instructions() {
        let cases = [
            (
                "MOVK X8, #0x1234",
                MOVK {
                    rd: parse_register("X8"),
                    imm: Imm(Unsigned16(0x1234)),
                    shift: Imm(Unsigned16(0)),
                },
            ),
            (
                "MOVK X8, #0x1234, LSL #16",
                MOVK {
                    rd: parse_register("X8"),
                    imm: Imm(Unsigned16(0x1234)),
                    shift: Imm(Unsigned16(16)),
                },
            ),
            (
                "MOVZ X0, #0xABCD",
                MOVZ {
                    rd: parse_register("X0"),
                    imm: Imm(Unsigned16(0xABCD)),
                    shift: Imm(Unsigned16(0)),
                },
            ),
            (
                "MOVZ X1, #0xFF, LSL #32",
                MOVZ {
                    rd: parse_register("X1"),
                    imm: Imm(Unsigned16(0xFF)),
                    shift: Imm(Unsigned16(32)),
                },
            ),
            (
                "ORR X0, X1, X2",
                ORR {
                    dest: parse_register("X0"),
                    src1: parse_register("X1"),
                    src2: parse_register("X2"),
                },
            ),
            (
                "ADD X0, X1, X2",
                ADD {
                    dest: parse_register("X0"),
                    src1: parse_register("X1"),
                    src2: Reg(parse_register("X2")),
                },
            ),
            (
                "ADD X0, X1, #42",
                ADD {
                    dest: parse_register("X0"),
                    src1: parse_register("X1"),
                    src2: Imm(Unsigned(42)),
                },
            ),
            (
                "LDR X1, =message",
                LDR {
                    rt: parse_register("X1"),
                    label: Imm(UnresolvedSymbol("message".into())),
                },
            ),
            (
                "LDR X0, =0x1000",
                LDR {
                    rt: parse_register("X0"),
                    label: Imm(Address(0x1000)),
                },
            ),
            (
                "STUR X0, =0x1000",
                STUR {
                    src: parse_register("X0"),
                    addr: Imm(Address(0x1000)),
                },
            ),
            (
                "B 0x1000",
                B {
                    addr: Imm(Address(0x1000)),
                },
            ),
            (
                "SVC #0x0",
                SVC {
                    syscall: Imm(Unsigned16(0)),
                },
            ),
            (
                "SVC #0x4",
                SVC {
                    syscall: Imm(Unsigned16(4)),
                },
            ),
        ];
        for (source, expected) in cases {
            assert_eq!(
                parse_line(source, &mut State::new()),
                Some(expected),
                "{source}"
            );
        }
    }

    #[test]
    fn records_labels() {
        let mut state = State::new();
        state.current_addr = 16;
        assert_eq!(parse_line("main:", &mut state), None);
        assert_eq!(state.labels.get("main"), Some(&16));
    }

    #[test]
    fn switches_sections() {
        let mut state = State::new();
        for section in [".data", ".text"] {
            assert_eq!(parse_line(section, &mut state), None);
            assert_eq!(state.current_section, section);
        }
    }

    #[test]
    fn emits_null_terminated_string() {
        let mut state = State::new();
        assert_eq!(parse_line(".asciz \"hello\"", &mut state), None);
        assert_eq!(state.data, b"hello\0");
    }
}
