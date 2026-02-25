#[cfg(test)]
mod tests {
    use crate::assembler::assemble;
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
}
