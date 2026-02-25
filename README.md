# ArmAsm

Minimal ARM Assembler written in Rust.

Features:
- movz
- movk 
- ldr 
- svc 
- PC-relative addressing 
- symbol resolution 
- literal pooling
- example assembly file that works

You can easily add more instructions. Currently it parses more instructions than it encodes, in case I want to implement them later.


## Example: 
```assembly
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
    .asciz "Hello, World!\n"%  
```
to 
`xxd output.bin`
```
00000000: 8800 80d2 2000 80d2 8100 0058 a201 80d2  .... ......X....
00000010: 0100 00d4 2800 80d2 0000 80d2 0100 00d4  ....(...........
00000020: 4865 6c6c 6f2c 2057 6f72 6c64 215c 6e00  Hello, World!\n.
```

![verify](./decomp.png)


## TODO

### Finish project
- What we need:
  - movz [x]
  - movk [x]
  - ldr  [ ]
  - svc  [ ]
  - PC-relative addressing [x]
  - symbol resolution [x]
  - literal pooling [ ]
  - example assembly file that works [ ]
  - Good tests [ ]
  - Taste :0 [ ]

