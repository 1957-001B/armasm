#[cfg(test)]
mod tests {
    use crate::assembler::assemble;
    use crate::assembler::*;
    use std::fs::File;
    use std::io::Read;

    #[test]
    fn test_asm() -> std::io::Result<()> {
        assemble("./example/goal.s".to_string())?;

        let mut file = File::open("./example/output.bin")?;
        let mut expected = Vec::new();
        file.read_to_end(&mut expected)?;

        let mut actual_file = File::open("output.bin")?;
        let mut actual = Vec::new();
        actual_file.read_to_end(&mut actual)?;

        assert_eq!(actual, expected);
        Ok(())
    }

    fn make_state() -> State {
        State::new()
    }

    #[test]
    fn test_parse_movk() {
        let mut state = make_state();
        let result = parse_line("MOVK X8, #0x1234", &mut state);
        assert!(result.is_some());
        let inst = result.unwrap();
        match inst {
            Instruction::MOVK { rd, imm, shift } => {
                assert!(matches!(rd, Operand::Reg(Reg::X8)));
                assert!(matches!(imm, Operand::Imm(ImmType::Unsigned16(0x1234))));
                assert!(matches!(shift, Operand::Imm(ImmType::Unsigned16(0))));
            }
            _ => panic!("Expected MOVK"),
        }
    }

    #[test]
    fn test_parse_movk_lsl() {
        let mut state = make_state();
        let result = parse_line("MOVK X8, #0x1234, LSL #16", &mut state);
        assert!(result.is_some());
        let inst = result.unwrap();
        match inst {
            Instruction::MOVK { rd, imm, shift } => {
                assert!(matches!(rd, Operand::Reg(Reg::X8)));
                assert!(matches!(imm, Operand::Imm(ImmType::Unsigned16(0x1234))));
                assert!(matches!(shift, Operand::Imm(ImmType::Unsigned16(16))));
            }
            _ => panic!("Expected MOVK with LSL"),
        }
    }

    #[test]
    fn test_parse_movz() {
        let mut state = make_state();
        let result = parse_line("MOVZ X0, #0xABCD", &mut state);
        assert!(result.is_some());
        let inst = result.unwrap();
        match inst {
            Instruction::MOVZ { rd, imm, shift } => {
                assert!(matches!(rd, Operand::Reg(Reg::X0)));
                assert!(matches!(imm, Operand::Imm(ImmType::Unsigned16(0xABCD))));
                assert!(matches!(shift, Operand::Imm(ImmType::Unsigned16(0))));
            }
            _ => panic!("Expected MOVZ"),
        }
    }

    #[test]
    fn test_parse_movz_lsl() {
        let mut state = make_state();
        let result = parse_line("MOVZ X1, #0xFF, LSL #32", &mut state);
        assert!(result.is_some());
        let inst = result.unwrap();
        match inst {
            Instruction::MOVZ { rd, imm, shift } => {
                assert!(matches!(rd, Operand::Reg(Reg::X1)));
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
                assert!(matches!(dest, Operand::Reg(Reg::X0)));
                assert!(matches!(src1, Operand::Reg(Reg::X1)));
                assert!(matches!(src2, Operand::Reg(Reg::X2)));
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
                assert!(matches!(dest, Operand::Reg(Reg::X0)));
                assert!(matches!(src1, Operand::Reg(Reg::X1)));
                assert!(matches!(src2, Operand::Reg(Reg::X2)));
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
                assert!(matches!(dest, Operand::Reg(Reg::X0)));
                assert!(matches!(src1, Operand::Reg(Reg::X1)));
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
                assert!(matches!(rt, Operand::Reg(Reg::X1)));
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
                assert!(matches!(rt, Operand::Reg(Reg::X0)));
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
                assert!(matches!(src, Operand::Reg(Reg::X0)));
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
        assert_eq!(state.data, b"hello\0");
    }
}
