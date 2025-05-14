.text
.global _start

// Program entry point
_start:
    // Load syscall numbers and simple values
    MOVZ X8, #0x4
    MOVZ X0, #0x1
    
    // Load message address with symbol
    LDR X1, =message
    
    // Load message length in hex
    MOVZ X2, #0xD
    
    // Make the write syscall
    SVC #0x0
    
    // Exit program
    MOVZ X8, #0x1
    MOVZ X0, #0x0
    SVC #0x0

// Data section
.data
// Message label
message:
    .asciz "Hello, World!\n"
