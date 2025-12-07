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
mf3.asm           - Main assembly source (fully commented)
original.rom      - Original 8KB Multiface 3 ROM dump
sysvarp3.sym      - ZX Spectrum +3 system variable definitions
p3dos.sym         - +3DOS routine addresses
M3LOADER          - Spectrum +3 BASIC loader program
docs/             - Original Multiface 3 manual
utils/def2sym.py  - Symbol file converter
CLAUDE.md         - Architecture documentation for AI assistants
```

## History

Started in 2009 as a disassembly project to understand and preserve the Multiface 3 ROM.

Updated in 2024 with comprehensive documentation and modern build tools.

## Technical Highlights

The ROM demonstrates several advanced techniques:
- **ROM search algorithm** to find safe return points dynamically
- **Dual-context execution** with seamless context switching
- **Minimal footprint** - entire OS in 8KB including UI, file operations, toolkit
- **Zero impact** on running software when not activated
- **Bank switching mastery** - handles all Spectrum +3 memory modes
- **Elegant space optimization** - shared code paths, implied returns, LDIR tricks

## Documentation

See `CLAUDE.md` for high-level architecture overview.

The source file `mf3.asm` contains detailed comments explaining each major routine block.
