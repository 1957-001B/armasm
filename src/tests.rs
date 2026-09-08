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
<<<<<<< Updated upstream
    fn test_parse_movk() {
        let mut state = make_state();
        let result = parse_line("MOVK X8, #0x1234", &mut state);
        assert!(result.is_some());
        let inst = result.unwrap();
        match inst {
            Instruction::MOVK { rd, imm, shift } => {
                assert!(matches!(rd, Operand::Reg(8)));
                assert!(matches!(imm, Operand::Imm(ImmType::Unsigned16(0x1234))));
                assert!(matches!(shift, Operand::Imm(ImmType::Unsigned16(0))));
            }
            _ => panic!("Expected MOVK"),
=======
    fn encodes_instructions() {
        // Fixed A64 words from the DDI 0602 layouts, independent of the encoder.
        for (source, expected) in [
            ("MOVZ X0, #0xABCD", 0xD295_79A0),
            ("MOVZ X1, #0xFF, LSL #32", 0xD2C0_1FE1),
            ("MOVK X8, #0x1234", 0xF282_4688),
            ("MOVK X8, #0x1234, LSL #16", 0xF2A2_4688),
            ("SVC #0x0", 0xD400_0001),
            ("SVC #0x4", 0xD400_0081),
        ] {
            let mut state = State::new();
            let instruction = parse_line(source, &mut state).expect(source);
            assert_eq!(encode_line(&instruction, &mut state), expected, "{source}");
>>>>>>> Stashed changes
        }
    }

    #[test]
<<<<<<< Updated upstream
    fn test_parse_movk_lsl() {
        let mut state = make_state();
        let result = parse_line("MOVK X8, #0x1234, LSL #16", &mut state);
        assert!(result.is_some());
        let inst = result.unwrap();
        match inst {
            Instruction::MOVK { rd, imm, shift } => {
                assert!(matches!(rd, Operand::Reg(8)));
                assert!(matches!(imm, Operand::Imm(ImmType::Unsigned16(0x1234))));
                assert!(matches!(shift, Operand::Imm(ImmType::Unsigned16(16))));
            }
            _ => panic!("Expected MOVK with LSL"),
=======
    fn encodes_ldr_opcode_before_relocation() {
        // Only checks the opcode and Rt; encode_line leaves imm19 unresolved.
        let mut state = State::new();
        let instruction = parse_line("LDR X1, =message", &mut state).unwrap();
        assert_eq!(encode_line(&instruction, &mut state), 0x5800_0001);
    }

    #[test]
    fn parses_instructions() {

        use crate::assembler::parse_register;

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
>>>>>>> Stashed changes
        }
    }

    #[test]
<<<<<<< Updated upstream
    fn test_parse_movz() {
        let mut state = make_state();
        let result = parse_line("MOVZ X0, #0xABCD", &mut state);
        assert!(result.is_some());
        let inst = result.unwrap();
        match inst {
            Instruction::MOVZ { rd, imm, shift } => {
                assert!(matches!(rd, Operand::Reg(0)));
                assert!(matches!(imm, Operand::Imm(ImmType::Unsigned16(0xABCD))));
                assert!(matches!(shift, Operand::Imm(ImmType::Unsigned16(0))));
            }
            _ => panic!("Expected MOVZ"),
=======
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
>>>>>>> Stashed changes
        }
    }

    #[test]
<<<<<<< Updated upstream
    fn test_parse_movz_lsl() {
        let mut state = make_state();
        let result = parse_line("MOVZ X1, #0xFF, LSL #32", &mut state);
        assert!(result.is_some());
        let inst = result.unwrap();
        match inst {
            Instruction::MOVZ { rd, imm, shift } => {
                assert!(matches!(rd, Operand::Reg(1)));
                assert!(matches!(imm, Operand::Imm(ImmType::Unsigned16(0xFF))));
                assert!(matches!(shift, Operand::Imm(ImmType::Unsigned16(32))));
            }
            _ => panic!("Expected MOVZ with LSL"),
        }
    }

    #[test]
    fn test_parse_orr() {
        let mut state = make_state();
        let result = parse_line("ORR X0, X1, X2", &mut state);
        assert!(result.is_some());
        let inst = result.unwrap();
        match inst {
            Instruction::ORR { dest, src1, src2 } => {
                assert!(matches!(dest, Operand::Reg(0)));
                assert!(matches!(src1, Operand::Reg(1)));
                assert!(matches!(src2, Operand::Reg(2)));
            }
            _ => panic!("Expected ORR"),
        }
    }

    #[test]
    fn test_parse_add_reg() {
        let mut state = make_state();
        let result = parse_line("ADD X0, X1, X2", &mut state);
        assert!(result.is_some());
        let inst = result.unwrap();
        match inst {
            Instruction::ADD { dest, src1, src2 } => {
                assert!(matches!(dest, Operand::Reg(0)));
                assert!(matches!(src1, Operand::Reg(1)));
                assert!(matches!(src2, Operand::Reg(2)));
            }
            _ => panic!("Expected ADD"),
        }
    }

    #[test]
    fn test_parse_add_imm() {
        let mut state = make_state();
        let result = parse_line("ADD X0, X1, #42", &mut state);
        assert!(result.is_some());
        let inst = result.unwrap();
        match inst {
            Instruction::ADD { dest, src1, src2 } => {
                assert!(matches!(dest, Operand::Reg(0)));
                assert!(matches!(src1, Operand::Reg(1)));
                assert!(matches!(src2, Operand::Imm(ImmType::Unsigned(42))));
            }
            _ => panic!("Expected ADD with immediate"),
        }
    }

    #[test]
    fn test_parse_ldr_literal() {
        let mut state = make_state();
        let result = parse_line("LDR X1, =message", &mut state);
        assert!(result.is_some());
        let inst = result.unwrap();
        match inst {
            Instruction::LDR { rt, label } => {
                assert!(matches!(rt, Operand::Reg(1)));
                assert!(
                    matches!(label, Operand::Imm(ImmType::UnresolvedSymbol(s)) if s == "message")
                );
            }
            _ => panic!("Expected LDR"),
        }
    }

    #[test]
    fn test_parse_ldr_hex() {
        let mut state = make_state();
        let result = parse_line("LDR X0, =0x1000", &mut state);
        assert!(result.is_some());
        let inst = result.unwrap();
        match inst {
            Instruction::LDR { rt, label } => {
                assert!(matches!(rt, Operand::Reg(0)));
                assert!(matches!(label, Operand::Imm(ImmType::Address(0x1000))));
            }
            _ => panic!("Expected LDR with hex address"),
        }
    }

    #[test]
    fn test_parse_stur() {
        let mut state = make_state();
        let result = parse_line("STUR X0, =0x1000", &mut state);
        assert!(result.is_some());
        let inst = result.unwrap();
        match inst {
            Instruction::STUR { src, addr } => {
                assert!(matches!(src, Operand::Reg(0)));
                assert!(matches!(addr, Operand::Imm(ImmType::Address(0x1000))));
            }
            _ => panic!("Expected STUR"),
        }
    }

    #[test]
    fn test_parse_b() {
        let mut state = make_state();
        let result = parse_line("B 0x1000", &mut state);
        assert!(result.is_some());
        let inst = result.unwrap();
        match inst {
            Instruction::B { addr } => {
                assert!(matches!(addr, Operand::Imm(ImmType::Address(0x1000))));
            }
            _ => panic!("Expected B"),
        }
    }

    #[test]
    fn test_parse_svc() {
        let mut state = make_state();
        let result = parse_line("SVC #0x0", &mut state);
        assert!(result.is_some());
        let inst = result.unwrap();
        match inst {
            Instruction::SVC { syscall } => {
                assert!(matches!(syscall, Operand::Imm(ImmType::Unsigned16(0))));
            }
            _ => panic!("Expected SVC"),
        }
    }

    #[test]
    fn test_parse_svc_hex() {
        let mut state = make_state();
        let result = parse_line("SVC #0x4", &mut state);
        assert!(result.is_some());
        let inst = result.unwrap();
        match inst {
            Instruction::SVC { syscall } => {
                assert!(matches!(syscall, Operand::Imm(ImmType::Unsigned16(4))));
            }
            _ => panic!("Expected SVC with hex"),
        }
    }

    #[test]
    fn test_parse_label() {
        let mut state = make_state();
        let result = parse_line("main:", &mut state);
        assert!(result.is_none());
        assert!(state.labels.contains_key("main"));
        assert_eq!(state.labels.get("main"), Some(&0));
    }

    #[test]
    fn test_parse_directive_text() {
        let mut state = make_state();
        let result = parse_line(".text", &mut state);
        assert!(result.is_none());
        assert_eq!(state.current_section, ".text");
    }

    #[test]
    fn test_parse_directive_data() {
        let mut state = make_state();
        let result = parse_line(".data", &mut state);
        assert!(result.is_none());
        assert_eq!(state.current_section, ".data");
    }

    #[test]
    fn test_parse_directive_asciz() {
        let mut state = make_state();
        let result = parse_line(".asciz \"hello\"", &mut state);
        assert!(result.is_none());
=======
    fn emits_null_terminated_string() {
        let mut state = State::new();
        assert_eq!(parse_line(".asciz \"hello\"", &mut state), None);
>>>>>>> Stashed changes
        assert_eq!(state.data, b"hello\0");
    }
}
