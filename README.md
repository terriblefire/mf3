# Multiface 3 ROM Disassembly

![Build Status](https://github.com/YOUR_USERNAME/mf3/workflows/Build%20Multiface%203%20ROM/badge.svg)

A fully documented and buildable disassembly of the Multiface 3 ROM for the ZX Spectrum +3.

## What is Multiface 3?

The Multiface 3 by Romantic Robot was a hardware interface for the Spectrum +3 that provided:
- **Universal backup** - Save/load programs between tape and disk
- **Development toolkit** - Memory inspection, modification, and debugging tools
- **Screen printing** - Print screen contents to printer
- **DOS access in 48K mode** - Use +3DOS even when locked in 48K mode
- **Direct jump feature** - Programmable instant jump on button press

## How It Works

### Hardware Architecture

The Multiface 3 plugs into the Spectrum +3 expansion port and contains:
- **8KB ROM** with the Multiface operating system
- **8KB RAM** for temporary workspace and state preservation
- **Red button** that triggers an NMI (Non-Maskable Interrupt)
- **Paging logic** to temporarily overlay the Spectrum's memory map

### Key Technical Features

#### 1. NMI Entry and State Preservation
When the red button is pressed, the hardware triggers an NMI at address 0x0066:
- All CPU registers saved (AF, BC, DE, HL, IX, IY, I, R, alternates)
- Memory banking state captured from ports 0x7FFD and 0x1FFD
- AY sound chip registers scanned and stored
- Complete machine state frozen for later restoration

#### 2. ROM Return Vector Search
One of the most ingenious parts - the ROM searches the Spectrum OS for a safe return point:
- Looks for byte sequence `F1 C9` (POP AF, RET) in ROM range 0x260B-0x3FC3
- This becomes the exit point to cleanly return to the interrupted program
- Falls back to 0x4004 if no suitable sequence found

#### 3. Memory Context Switching
The Multiface operates in two contexts simultaneously:
- **Multiface context**: ROM paged in, menu visible
- **Spectrum context**: Original program state preserved

Memory swapping routines handle:
- Screen buffer preservation (bottom 16 lines for menu)
- +3DOS workspace (0xDB00-0xE600) preservation
- Bank switching for 48K/128K modes
- Interop code in RAMAREA (0x5FE0) for cross-context calls

#### 4. Interop System
A bridge layer copied to main RAM allows calling Spectrum ROM while MF3 is paged out:
- **CALL_IX** - Execute routine via IX register
- **CALL_48ROM** - Call 48K ROM routine at address in HL
- **CALL_BEEPKEY** - Keyboard input with beeper feedback
- **CALL_RST10** - Print character
- **CALL_LDIR** - Block copy without MF3 ROM
- **CALL_RECLAIM/MAKEROOM** - BASIC memory management
- **CALL_READROM** - Read ROM byte

Each interop routine pages MF3 out, executes the operation, then pages MF3 back in.

#### 5. Direct Jump Feature
Program a custom jump address and enable instant execution:
- POKE 8192-8193 with jump address
- POKE 8195-8197 with "RUN" (82, 85, 78) to enable
- Press red button → jumps directly, bypassing menu
- Press red button + BREAK simultaneously to disable
- RST 0 from jumped code returns to interrupted program

#### 6. 48K Mode Detection
The system detects if +3DOS is available:
- Checks bit 3 of memory location 0x2006
- Locked mode (48K): Disables disk operations in menu
- Unlocked mode (+3): Full disk/tape transfer functionality

#### 7. Printer Configuration System
EPSON-compatible printer control via memory-mapped registers (see manual section 4.1):
- **0x2008**: Print mode (0x01=Large+CRLF, 0xF1=Shaded+CRLF, 0x00=Large+CR, 0xF0=Shaded+CR)
- **0x200B**: Left margin (0-255)
- **0x200C**: Bottom margin (0-23)
- **0x200D**: Top margin (0-23)
- **0x200E-0x2010**: ESC sequence for line spacing (default: 27,51,23)
- **0x2011-0x2014**: ESC sequence for graphic mode (default: 27,76,0,3)

All configurable via POKE from the toolkit for different printer models.

#### 8. Register Storage Format
CPU state stored at **0x3FE4** (16356 decimal) in INTEL format (low byte, high byte):
- Order: `PC IY IX BC' DE' HL' AF' BC DE R- I- HL AF SP`
- Total 28 bytes (0x3FE4-0x3FFF)
- Accessible via toolkit 'R' command for inspection and modification

#### 9. Save/Load System Details
- **Filenames**: Maximum 7 characters (no extensions, spaces, or punctuation)
- **Special filename**: "DISK" triggers autoboot on Spectrum+3
- **48K mode**: Saves blocks 5, 2, 0 only
- **128K mode**: Saves all blocks 0-7
- **Bank clearing**: Clears banks 1, 3, 4, 6 (fills with 0xE5 for RAM disc)
- **Bank 7**: Never cleared (used by Spectrum+3 as workspace)
- **Compression**: Automatic (can be disabled), always off for screen saves

#### 10. Toolkit Features
Full-screen memory editor with:
- **Window display**: 16 lines × 8 bytes (128 bytes total)
- **Navigation**: Arrow keys (byte/line), N/M keys (page backward/forward)
- **Modes**: Hex/Decimal toggle ('H'), ASCII text view ('T')
- **Bank selection**: 'S' + 0-7 to view different RAM banks at 0xC000-0xFFFF
- **PEEK/POKE**: Enter address with SPACE, value with hex digits, ENTER to write
- **Register view**: 'R' jumps to 0x3FE4 to inspect/modify CPU state
- **Print**: 'P' outputs ASCII dump of window contents

## Building

### Automated Builds

Every push to the repository triggers an automated build via GitHub Actions:
- Builds the ROM using Docker and z88dk
- Verifies the ROM is exactly 8192 bytes
- Confirms MD5 checksum matches the original: `afe2218c3a6a43f1854fe824424d4167`
- Uploads the ROM as a build artifact

Pre-built ROMs are available in the [Actions](../../actions) tab or as [Releases](../../releases).

### Local Build

The source has been updated for modern z88dk z80asm (2024+):

```bash
make clean    # Clean build artifacts
make          # Build mf3.rom
make diff     # Verify binary matches original
```

### Requirements
- Docker (uses z88dk/z88dk image with platform linux/amd64)
- The build produces a bit-perfect replica: MD5 `afe2218c3a6a43f1854fe824424d4167`

### Syntax Updates
The original code used older z80asm syntax. It has been updated to:
- Local labels: `.label` → `@label` (modern z88dk syntax)
- Character constants: `"A"` → `'A'` (single quotes required)

## Project Structure

```
mf3.asm                      - Main assembly source (fully commented with descriptive labels)
original.rom                 - Original 8KB Multiface 3 ROM dump
sysvarp3.sym                 - ZX Spectrum +3 system variable definitions
p3dos.sym                    - +3DOS routine addresses
M3LOADER                     - Spectrum +3 BASIC loader program
docs/                        - Documentation
  Multiface3_Manual.txt      - Original manual (text format)
  Manuale Multiface 3.pdf    - Official manual (PDF)
utils/def2sym.py             - Symbol file converter
.github/workflows/build.yml  - CI/CD pipeline for automated builds
```

## History

**2009**: Initial disassembly project to understand and preserve the Multiface 3 ROM.

**2024-2025**: Major updates:
- Comprehensive documentation of all major subsystems
- Analysis of official manual to identify memory-mapped registers
- Renamed 27+ key routines with descriptive names for readability
- Updated for modern z88dk z80asm syntax
- Added GitHub Actions CI/CD for automated builds
- Achieved bit-perfect reproduction (MD5: `afe2218c3a6a43f1854fe824424d4167`)

## Technical Highlights

The ROM demonstrates several advanced techniques:
- **ROM search algorithm** to find safe return points dynamically
- **Dual-context execution** with seamless context switching
- **Minimal footprint** - entire OS in 8KB including UI, file operations, toolkit
- **Zero impact** on running software when not activated
- **Bank switching mastery** - handles all Spectrum +3 memory modes
- **Elegant space optimization** - shared code paths, implied returns, LDIR tricks

## Documentation

### Source Code Documentation
The source file `mf3.asm` contains:
- Detailed header comments for all major subsystems
- Inline documentation of key algorithms
- Memory location usage tables
- Cross-references to official manual sections
- 27+ descriptive routine names (e.g., `screen_print_main`, `validate_mem_addr`, `swap_screen_buffers`)

### Key Memory Locations

| Address | Purpose |
|---------|---------|
| 0x2006 | Status flags (bit 3=DOS available, bit 6=hex/text, bit 7=write mode) |
| 0x2008 | Printer mode configuration |
| 0x200B-0x200D | Printer margins (left, bottom, top) |
| 0x200E-0x2014 | Printer ESC sequences |
| 0x3FE4-0x3FFF | CPU register storage (28 bytes) |
| 0x5FFA | Current toolkit display address |
| 0x6001 | Current bank selection (0-7) |
| 0x6003 | Toolkit input buffer pointer |

### Major Subsystems Documented

1. **NMI Entry and State Preservation** - Complete CPU context capture
2. **ROM Return Vector Search** - F1 C9 pattern matching
3. **Memory Context Switching** - Dual-context execution
4. **Interop System** - Cross-context ROM calls
5. **Save/Load System** - Tape and disk operations with compression
6. **Toolkit/Memory Editor** - Full-screen hex editor with bank switching
7. **Screen Print System** - EPSON-compatible hi-res printing
8. **Printer Configuration** - Memory-mapped control registers
9. **Banking System** - 48K/128K mode handling
10. **DOS Integration** - File operations and error handling
