;; ====================================================================
;; Multiface 3 ROM Disassembly
;; ====================================================================
;;
;; This is a complete, fully documented disassembly of the Multiface 3
;; ROM for the Sinclair ZX Spectrum +3.
;;
;; Original Hardware: Multiface 3 by Romantic Robot Ltd (1987)
;; ROM Size: 8KB (8192 bytes)
;; Target MD5: afe2218c3a6a43f1854fe824424d4167
;;
;; The Multiface 3 is a hardware interface providing:
;; - NMI-triggered snapshot and save/load functionality
;; - Full memory editor toolkit
;; - Screen printing to EPSON-compatible printers
;; - Custom compression system for 128K snapshots
;; - +3DOS integration for disk operations
;;
;; This source code builds a bit-perfect reproduction of the original
;; ROM using modern z88dk z80asm assembler.
;;
;; Build: make clean && make
;; Verify: md5sum mf3.rom original.rom
;;
;; Documentation Status:
;; - 180+ routines renamed with descriptive names
;; - 170+ formerly anonymous hex labels eliminated (all zero remaining)
;; - 200+ documentation headers and inline comments added
;; - All calling conventions and register usage documented
;; - Memory-mapped I/O locations fully annotated
;; - Dead/unreachable code areas identified
;; - 26 major functional sections mapped
;;
;; ====================================================================

; z80dasm 1.1.0
; command line: z80dasm --origin=0 --sym-input=sysvarp3.sym -l -o mf3.asm original.rom

    org    00000h

SWAP:     equ 0x5b00
BANKM:    equ 0x5b5c
KSTATE:   equ 0x5c00
LASTK:    equ 0x5c08
CHANS:    equ 0x5c4f
PROG:     equ 0x5c53
ERRNR:    equ 0x5C3A
STRMS:    equ 0x5C10
RAMTOP:   equ 0x5CB2
BANK678:  equ 0x5b67
LODDRV:   equ 0x5b79
SAVDRV:   equ 0x5b7a
BANK1:    equ 0x7ffd  ; ZX 128K paging latch: RAM bank / shadow screen / ROM / lock
BANK2:    equ 0x1ffd  ; ZX Spectrum +3 secondary paging latch: special map / ROM1 / motor / printer
SCREEN:   equ 0x4000

    ;; ROM Subroutines

BEEPER:    equ 0x03b5
RECLAIM1:  equ 0x19e5
MAKEROOM:  equ 0x1655
CLSLOWER:  equ 0x0d6e
CHANOPEN:  equ 0x1601
SABYTES:   equ 0x04c2
LDBYTES:   equ 0x0556
INTTOFP:   equ 0x2D3B
FPTOBC:    equ 0x2DA2

    ;; +3 bank bits (0x1FFD)
PLUS3_SPECIAL:  equ 0x01  ; bit 0: special paging enable
ROM_SEL1:       equ 0x04  ; bit 2: select +3 ROM 1/2 inside special paging mode
PLUS3_MOTOR:    equ 0x08  ; bit 3: floppy motor on
PLUS3_PRINTER:  equ 0x10  ; bit 4: Centronics printer strobe (active low)

    ;; 128k bank bits (0x7FFD)
BANK1_SCREEN:   equ 0x08  ; bit 3: select shadow screen at 0xC000
ROM_SEL0:       equ 0x10  ; bit 4: select ROM 0/1
ROM_SEL0_BIT:   equ 4     ; bit index of ROM_SEL0
BANK1_DISABLE:  equ 0x20  ; bit 5: lock paging until reset

    ;; plus3dos calls used

DOS_OPEN:         equ 0x0106
DOS_CLOSE:        equ 0x0109
DOS_FREE_SPACE:   equ 0x0121
DOS_REF_HEAD:     equ 0x010f
DOS_READ:         equ 0x0112
DOS_WRITE:        equ 0x0115
DOS_CATALOG:      equ 0x011e
DOS_DELETE:       equ 0x0124
DOS_SET_MESSAGE:  equ 0x014e
    
    ;; scratch area in bank2. used to execute code with mf3 paged out.
RAMAREA:         equ 0x5fe0  ;; temp space to save ram in the MF3
MF3TEMP:         equ 025e1h  ;; location in the MF3 ram which stores the return routine.
MF3_RET_VECTOR:  equ 0x2027
    
    ;; Definitions for Multiface 3
    
P_MF3_IN:      equ 0x3f    ; MF3 decode: page the Multiface ROM in
P_MF3_OUT:     equ 0xbf    ; MF3 decode: page the Multiface ROM out
P_MF3_BUTTON:  equ 0x3f    ; same decode as P_MF3_IN, named for button/lockout paths

P_MF3_P7FFD:  equ 0x7f3f  ; MF3 mirror: read back last 0x7FFD write
P_MF3_P1FFD:  equ 0x1f3f  ; MF3 mirror: read back last 0x1FFD write

    ;; addresses of interop routines

CALL_IX:        equ RAMAREA + 116  ;
CALL_48ROM:     equ RAMAREA + 127  ;
CALL_ROM2:      equ 0x6047
CALL_BEEPKEY:   equ RAMAREA + 136  ;
CALL_RST10:     equ RAMAREA + 175  ; PRINT A
CALL_READROM:   equ RAMAREA + 202  ;
CALL_LDIR:      equ RAMAREA + 182  ;
CALL_RECLAIM:   equ RAMAREA + 188  ;
CALL_MAKEROOM:  equ RAMAREA + 195  ;
    
    ;; ZX Spectrum color constants (INK/PAPER/BORDER values 0-7)
BLACK:     equ 0
BLUE:      equ 1
RED:       equ 2
MAGENTA:   equ 3
GREEN:     equ 4
CYAN:      equ 5
YELLOW:    equ 6
WHITE:     equ 7
    ;; Sinclair attribute byte composites (FLASH: bit7, BRIGHT: bit6, PAPER: bits5-3, INK: bits2-0)
ATTR_BLACK_ON_YELLOW_BRIGHT: equ 0x70  ; black ink, bright yellow paper (toolkit CLS)
ATTR_BLUE_ON_BLUE:           equ 0x09  ; blue ink, blue paper (error screen fill)
ATTR_BLACK_ON_WHITE_BRIGHT:  equ 0x38  ; black ink, bright white paper (stream attrs)

    ;; MF3 Control/Status Region (0x2000-0x2020)
MF3_ENTRY_VECTOR:                        equ 0x2000  ; Entry point vector (2 bytes)
MF3_PAGE_CONTROL:                        equ 0x2002  ; Paging control (0=keep, 1=page out)
MF3_RUN_ADDR:                            equ 0x2003  ; RUN address marker
MF3_STATUS_FLAGS:                        equ 0x2006  ; Main status/control flags byte
MF3_STATUS_FLAGS_SKIP_MARKER_SCAN:       equ 0       ; Bypass compression marker search
MF3_STATUS_FLAGS_IM2_DETECTED:           equ 1       ; IM2 interrupt detected during NMI
MF3_STATUS_FLAGS_ENABLED:                equ 2       ; MF3 enabled/visible to Spectrum code
MF3_STATUS_FLAGS_ENABLED_MASK:           equ 0x04    ; 1<<MF3_STATUS_FLAGS_ENABLED
MF3_STATUS_FLAGS_48K_LOCKED:             equ 3       ; 48K lock active (no +3DOS access)
MF3_STATUS_FLAGS_SAVE_128K_LOCKED:       equ 4       ; Save menu 128K lock state
MF3_STATUS_FLAGS_SAVE_128K_LOCKED_MASK:  equ 0x10
MF3_STATUS_FLAGS_SAVE_COMPRESS:          equ 5       ; Save menu compression state
MF3_STATUS_FLAGS_SAVE_COMPRESS_MASK:     equ 0x20
MF3_STATUS_FLAGS_TOOL_HEX_MODE:          equ 6       ; Toolkit displays hex values
MF3_STATUS_FLAGS_TOOL_HEX_MODE_MASK:     equ 0x40
MF3_STATUS_FLAGS_TOOL_WRITE_MODE:        equ 7       ; Toolkit write/poke mode active
MF3_STATUS_FLAGS_TOOL_WRITE_MODE_MASK:   equ 0x80
MF3_STATUS_FLAGS_TOOL_MODE_MASK:         equ 0xC0    ; Preserve toolkit mode bits (6,7)

MF3_PRINT_MODE:           equ 0x2008  ; Print mode (B1/F1/00/F0)
MF3_PRINT_LEFT_MARGIN:    equ 0x200B  ; Left margin (0-255)
MF3_PRINT_BOTTOM_MARGIN:  equ 0x200C  ; Bottom margin (0-23)
MF3_PRINT_TOP_MARGIN:     equ 0x200D  ; Top margin (0-23)
MF3_PRINT_ESC_SEQ:        equ 0x200E  ; ESC sequence buffer (line spacing)
MF3_PRINT_GRAPHIC_ESC:    equ 0x2011  ; ESC sequence (graphic mode)
MF3_IX_SAVE:              equ 0x2018  ; IX register save area (2 bytes)
MF3_IX_SAVE2:             equ 0x201A  ; Second IX save area (2 bytes)
MF3_ADDR_PTR:             equ 0x201D  ; Address pointer / multi-purpose mode flags
MF3_ADDR_PTR_DOS_SUBMENU:              equ 0  ; DOS submenu is the active caller context
MF3_ADDR_PTR_CATALOG_PAGED:            equ 1  ; Catalog has paged forward; re-enter fetches next page
MF3_ADDR_PTR_CATALOG_DELETE_PENDING:  equ 2  ; Catalog item marked for deletion after confirmation
MF3_ADDR_PTR_PRINT_LINE_COUNT:         equ 3  ; Address-entry linefeed pending; gates printa line counting
MF3_ADDR_PTR_PRINT_EXIT_TO_TOOLKIT:    equ 4  ; Printer entered from toolkit; errors return to toolkit
MF3_ADDR_PTR_DISK_LOAD_RETRY:         equ 5  ; Disk load in progress; errors may retry or return to BASIC
MF3_ADDR_PTR_MAIN_MENU_CLEAR_MASK:     equ 0xCE  ; Mask: preserve bits 1,2,4,5,6,7 on main menu return
MF3_SAVED_BANKM_COPY:     equ 0x201E  ; Copy of saved 0x7FFD latch value for snapshot data

    ;; MF3 Workspace Region (0x5FE0-0x6020)
RAMAREA_TEXT_DISPLAY_MODE:    equ 5  ; Toolkit screen view: show bytes as printable chars instead of hex
RAMAREA_SCREEN_MEASURED:      equ 6  ; Screen area compression size has been measured
RAMAREA_SYSVARS_MEASURED:     equ 7  ; System variables area compression size has been measured
MF3_WORK_BUFFER:   equ 0x5FE2  ; Start of work buffer (filename etc)
MF3_SAVED_DE:      equ 0x5FEC  ; Saved DE register
MF3_SAVED_BC:      equ 0x5FEE  ; Saved BC register
MF3_SAVED_HL:      equ 0x5FF0  ; Saved HL register
MF3_PTR_5FF4:      equ 0x5FF4  ; Pointer/address storage
MF3_SAVED_HL2:     equ 0x5FF6  ; Second HL save area
MF3_CODE_5FF7:     equ 0x5FF7  ; Executable code area
MF3_SAVED_BC2:     equ 0x5FF8  ; Second BC save area
MF3_DISPLAY_ADDR:  equ 0x5FFA  ; Display/RAM address pointer
MF3_SCREEN_SIZE:   equ 0x5FFC  ; Screen size parameter
MF3_PTR_5FFE:      equ 0x5FFE  ; Pointer storage

    ;; Banking/DOS Workspace (0x6000-0x6100)
MF3_BANK_FLAGS:    equ 0x6000  ; UI/runtime flags (load/save mode, swapped screen, toolkit state)
MF3_CURRENT_BANK:  equ 0x6001  ; Current bank number
MF3_BANK_PTR:      equ 0x6003  ; Bank pointer
MF3_BANK_CONTROL:  equ 0x6005  ; Bank control byte
MF3_SAVED_BANKM:   equ 0x3FF6  ; Saved 0x7FFD (BANKM) latch: bank number + ROM/screen flags
MF3_RESTORE_FLAGS: equ 0x3FF8  ; Composite: upper nibble = saved 1FFD state, bits 2,0 = interrupt restore
MF3_RESTORE_FLAGS_IM2:             equ 0  ; Machine was in IM2 before NMI; restore IM2 on exit
MF3_RESTORE_FLAGS_EI:              equ 2  ; Interrupts were enabled before NMI; restore EI on exit
MF3_BANK_ADDR:     equ 0x6006  ; Bank address storage
MF3_DOS_PTR:       equ 0x6008  ; DOS pointer
MF3_BUFFER_6011:   equ 0x6011  ; Data buffer start
MF3_BANK_BUFFER:   equ 0x6018  ; Bank presence/compression flags for banks 1/3/4/6
MF3_BANK_BUFFER2:  equ 0x6019  ; Bank presence/compression flags for bank 7
MF3_BANK1_SIZE:    equ 0x601A  ; Bank 1 size (2 bytes)
MF3_BANK3_SIZE:    equ 0x601C  ; Bank 3 size (2 bytes)
MF3_BANK4_SIZE:    equ 0x601E  ; Bank 4 size (2 bytes)
MF3_BANK6_SIZE:    equ 0x6020  ; Bank 6 size (2 bytes)

    ;; MF3_BANK_FLAGS bit meanings
MF3_BANK_FLAGS_FILENAME_LOAD_MODE:        equ 0
MF3_BANK_FLAGS_ALT_SCREEN_SWAPPED:        equ 1
MF3_BANK_FLAGS_ALT_SCREEN_SWAPPED_MASK:   equ 0x02
MF3_BANK_FLAGS_HAS_128K_BANKS:            equ 2
MF3_BANK_FLAGS_TAPE_MODE:                 equ 5
MF3_BANK_FLAGS_TOOL_SCREEN_SAVED:         equ 6
MF3_BANK_FLAGS_SAVE_SCREEN_MODE:          equ 7

    ;; ZX Spectrum ROM System Variable Aliases
ZX_FLAGS:                   equ 0x5C3B  ; FLAGS system variable (IY+1 when IY=0x5C3A ERRNR)
ZX_FLAGS_KEY_READY_BIT:             equ 3  ; FLAGS bit 3: signals key event to ROM keyboard scanner
ZX_FLAGS_INT_DONE_BIT:              equ 5  ; FLAGS bit 5: set by ROM ISR when interrupt processing complete
ZX_SYSVAR_5C6A:             equ 0x5C6A  ; Beep/timer system variable (IY+0x30)
ZX_SYSVAR_5C6A_BEEP_SYNC_BIT:      equ 3  ; Bit 3 of 0x5C6A: beep/timer synchronization flag

    ;; Save Menu Attribute Areas
MF3_MENU_ATTR_BASE:            equ 0x5AE0  ; Save menu option attribute row (32 bytes)
MF3_MENU_ATTR_LOCKED_BASE:      equ 0x5AD7  ; Locked-option attribute area (Compression/Lock labels)

    ;; MF3_BANK_BUFFER bit meanings
MF3_BANK1_PRESENT_BIT:                    equ 0
MF3_BANK3_PRESENT_BIT:                    equ 1
MF3_BANK4_PRESENT_BIT:                    equ 2
MF3_BANK6_PRESENT_BIT:                    equ 3
MF3_BANK1_COMPRESSED_BIT:                 equ 4
MF3_BANK3_COMPRESSED_BIT:                 equ 5
MF3_BANK4_COMPRESSED_BIT:                 equ 6
MF3_BANK6_COMPRESSED_BIT:                 equ 7

    ;; MF3_BANK_BUFFER2 bit meanings
MF3_BANK7_PRESENT_BIT:                    equ 3
MF3_BANK7_COMPRESSED_BIT:                 equ 7

TAB equ 0x06

INK:    MACRO   x
    defb    0x10,x
        ENDM

PAPER:    MACRO   x
    defb    0x11,x
        ENDM
    
FLASH:    MACRO   x
    defb    0x12,x
        ENDM

BRIGHT:    MACRO   x
    defb    0x13,x
        ENDM

INVERSE:MACRO   x
    defb    0x14,x
        ENDM

OVER:    MACRO   x
    defb    0x15,x
        ENDM        

AT:        MACRO   x,y
    defb    0x16,x,y
        ENDM        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; start
    
rom_start:
    ;; RST 0 and several MF3-generated return paths land here.
    ;; Red-button entry does not start at 0x0000; the Z80 NMI vector enters at 0x0066.
    ;; ROM signature byte at 0x0000 must remain C3h (JP opcode); loader checks this.
    jp code_start

msg_abort:
    INVERSE 1
    defb "a"
    INVERSE 0
    defb "bort Erase:" , TAB, TAB
    INK WHITE
    PAPER RED
    BRIGHT 1

msg_enter_attrs:
    AT 1,7
msg_enter:    
    AT 1,27
    INVERSE 1
    defb "ENTER"
    INVERSE 0

set_m3_status_bit_one:
    push hl    
    ld hl,MF3_STATUS_FLAGS
    jr status_set_enabled_bit
RST30:    
    jp printa
    
    ;; implied ret
    nop    
jump_via_hl:
    jp (hl)    
status_set_enabled_bit:
    ;; Set IM2 detected flag (bit 1) at (HL)
    set MF3_STATUS_FLAGS_IM2_DETECTED,(hl)
status_pop_hl:
    ;; Restore HL after flag set
    pop hl
    
RST38:    ret  ; IM1
    
msg_checksum_title:
    ld b,006h
    ld de,0x1002
    rlca    
    ld d,000h
printer_esc_end_row:
    nop    
rom_unused_42:
    dec de    
    inc sp    
    rla    
    dec de    
    ld c,h    
    nop    
    inc bc    
    dec de    
    ld b,b    
rom_unused_4b:
    ld b,a    
    defb 0edh  ;next byte illegal after ed

msg_write_prot:    

    defb 0dbh
    defb "Write protected ",6

msg_clean_attrs:
    
    AT 0,22
    INK BLUE
    PAPER YELLOW

    ;; ================================================================
    ;; NMI Entry Point (0x0066)
    ;; Main entry when red button is pressed
    ;; Preserves all CPU state and banking configuration
    ;; ================================================================
NMI_ENTRY:

    nop
    nop
    jp nmi_save_state

rom_entry_nmi_exit:
    jp restore_exit_to_menu
rom_entry_restore_tape:
    jp restore_from_tape
rom_entry_restore_disc:
    jp restore_from_disc

    ;; ================================================================
    ;; Direct Jump Routine (0x0074)
    ;; Implements programmable jump feature
    ;; Memory layout: 0x2000-0x2001 = jump address
    ;;                0x2002 = paging control (0=keep paged, 1=page out)
    ;; ================================================================
JUMP:
    ;; scan the keyboard for break
    call brkscan

    ;; break is being pressed then dont do the jump
    jp nc,jumpabrt

    ;; address 8194 (0x2002) determines the paging status: if its is 0, the M3 RAM remains paged, 1 pages out the
    ;; RAM and any other value disables the jump command entirely.

    ld a,(MF3_PAGE_CONTROL)

    ;; get the jump address and put it in HL
    ld hl,(MF3_ENTRY_VECTOR)
jump_page_ctrl_test:
    ;; Decrement page control value; if 1, jump to HL
    dec a

    ;; if A was 1, just jump to the value in HL.
    jr nz,jump_via_hl

    ex de,hl    
    ld hl,03fe4h
    ld (hl),e    
    inc hl    
    ld (hl),d    
    rst 0    

;; ================================================================
;; Page MF3 Out
;; Pages out Multiface 3 and saves context
;; Prepares for context swap with specific memory regions
;; ----------------------------------------------------------------
;; Uses:   HL, BC, DE (passed to swp_buffers)
;; ================================================================
page_mf3_out:

    ld hl,060b3h
    ld bc,005b3h
    ld de,0202dh
    ;; really strange way to save rom space
    jr swp_buffers
    ;; implied ret

;; ================================================================
;; Swap Memory Context
;; Toggles bit 1 of memory control flag at 0x6000
;; Swaps screen memory with bank at 0xC000
;; ----------------------------------------------------------------
;; Uses:   A, BC, DE, HL
;; ================================================================
swap_memory_context:
    ld a,(MF3_BANK_FLAGS)
    xor MF3_BANK_FLAGS_ALT_SCREEN_SWAPPED_MASK
    ld (MF3_BANK_FLAGS),a

    ld de,0c000h
    ld hl,SCREEN
    ld bc,tool_hex_lower_nibble  ; BC=0x1B00 = 6912 (screen size; label also code addr)
    ;; Jumps to swap_buffers routine
    ;; ----------------------------------------------------------------
swp_buffers:
    jp swap_buffers
    ;; implied ret


    ;; ================================================================
    ;; Restore Menu Screen Area
    ;; Restores saved screen content from MF3 RAM back to display
    ;; Reverses the save_menu_screen operation
    ;; Restores both screen data and attributes
    ;; ================================================================
    ;; copy the lower part of the screen where the multiface menu
    ;; appears back from the mf3 ram to the screen ram.

restore_menu_screen:

    ld hl,MF3TEMP
    ld de,050c0h  ; start of the last (bottom) 16 lines on the screen.
    ld b,008h  ; 16 lines (8 * 512)

main_menu_display:
    push bc
    ld bc,00040h
    ldir

    ld e,0c0h
    pop bc
    djnz main_menu_display

    ;; restore the attributes for the lower 2 rows
restore_menu_attrs:
    ld de,05ac0h
    ld bc,00040h
    ldir
    ret    
    
    ld bc,decompress_inline_entry
    ld b,012h
    ld bc,swap_check_alt_screen
    nop
    
msg_file_too_big:
    defb "File too big", TAB, TAB

msg_page_question:    
    AT 1,31
    defb "?"

    ;; ================================================================
    ;; Cleanup Return Vector
    ;; Restores RAMAREA from MF3TEMP after ROM search
    ;; Reverses the save done by set_return_vector
    ;; ================================================================
    ;; restore the ram area after the set_return_vector call.
    ;; restores the ram stored in the mf3 to the main ram.

cleanup_return_vector:

    ld hl,MF3TEMP
    ld de,RAMAREA
    ld bc,42
    ldir

    ret

    ;; ================================================================
    ;; Clear 16K Bank
    ;; Clears a complete 16K RAM bank (0xC000-0xFFFF)
    ;; A register specifies which bank via BANK1 port value
    ;; ================================================================
    ;; clear a 16k bank of ram
    ;; a = the BANK1 value  to use.

clear_bank:

    call switch_bank

    ld bc,03fffh
    ld hl,0c000h

    ;; ================================================================
    ;; Clear RAM Block
    ;; Clears memory from HL for BC+1 bytes
    ;; Uses LDIR trick: (HL)=0, then copy to (HL+1)
    ;; ================================================================
    ;; clears a block of ram
    ;; hl = start of ram to be cleared.
    ;; bc = size of ram to  be cleare minus 1.

clear_ram:

    ld (hl),000h

    ld d,h
    ld e,l

    inc de
    ldir

    ret
    
    ret    
rom_fallthrough:
    ld hl,(rom_start)
    ;; ================================================================
    ;; NMI State Preservation (0x0102)
    ;; Saves complete CPU context including all registers and flags
    ;; ================================================================
nmi_save_state:
    ld (03ffeh),sp
    ld sp,03ffeh

    push af
    push hl
    ld a,i
    push af
    di  ;disable interrupts

    ld a,r
    push af    
nmi_push_de_bc:
    ;; Save DE, BC on stack during NMI entry
    push de    
    push bc    
nmi_preserve_state:
    ex af,af'    
    push af    
nmi_save_alt_regs:
    ;; Save alternate register set (AF', BC', DE', HL')
    ex af,af'    
    exx    
    push hl    
    push de    
    push bc    
    exx    
    push ix

    push iy
    ld iy,ERRNR

    ld sp,(03ffeh)
    ex (sp),hl    
    ld sp,03fe6h
    push hl    
    ld sp,(03ffeh)
    ex (sp),hl    
    ld sp,03fd4h
    call init_printer_config
    xor a    
    ld (MF3_ADDR_PTR),a
    ld (0200ah),a
    ld i,a
    
    ld a,010h
    ld (MF3_SAVED_BANKM),a
    
    ;; get the last io write to 7ffd
    
    ld bc,P_MF3_P7FFD
    in a,(c)
    and 00fh
    
    ld c,a    
    ld a,(MF3_SAVED_BANKM)
    or c    

    ld (MF3_SAVED_BANKM),a
    
    ;; get the last I/O write to 1FFD (+3 secondary paging latch)
    
    ld bc,P_MF3_P1FFD
    in a,(c)

    and 00fh
    
    ;; move the 1FFD nibble into the upper half of MF3_RESTORE_FLAGS.
    ;; MF3_RESTORE_FLAGS is a mixed restore byte: high nibble = saved 1FFD state,
    ;; low bits = extra restore flags used on NMI exit.
    
    rlca    
    rlca    
    rlca    
    rlca    
    
    ld c,a    
    ld a,(MF3_RESTORE_FLAGS)
    and 004h
    or c    

    ld (MF3_RESTORE_FLAGS),a
    ld hl,03fe3h
    ld de,00f10h
    ld c,0fdh
scan_ay_register_mirror_loop:
    ld b,0ffh
    out (c),d
    in a,(c)
    ld (hl),a    
    ld a,d    
    and 00ch
    cp 008h
    jr nz,scan_ay_register_mirror_next
    
    ld b,P_MF3_OUT
    xor a    
    out (c),a
scan_ay_register_mirror_next:
    dec hl    
    dec d    
    dec e    
    jr nz,scan_ay_register_mirror_loop

    ei  ; enable interruptss
    halt  ; wait for the horizontal refresh
    di  ; disable interrputs again.

    ld hl,MF3_STATUS_FLAGS
    ld a,(MF3_RESTORE_FLAGS)
    ;; Bit 0 of MF3_RESTORE_FLAGS preserves whether the interrupted machine should
    ;; resume in IM 2. The source for that decision is status flag bit 1.
    bit MF3_STATUS_FLAGS_IM2_DETECTED,(hl)
    jr z,nmi_skip_im2_flag
    set MF3_RESTORE_FLAGS_IM2,a
nmi_skip_im2_flag:
    ;; IM2 flag not set; store MF3_RESTORE_FLAGS without setting bit 0
    ld (MF3_RESTORE_FLAGS),a
    im 1
    ld hl,(MF3_RET_VECTOR)
    ld de,0260bh

    ;; reset flags
    and a    
    sbc hl,de
    add hl,de
    ;; z true if hl=de
    ld de,03fc3h

    jr c,check_48k_mode
nmi_retvec_in_range:
    ;; Return vector is within valid range; check upper bound
    and a    
    ex de,hl    
    sbc hl,de
    jr c,check_48k_mode
    ex de,hl    
    ld de,(02029h)
    ld (hl),e    
    inc hl    
    ld (hl),d    
check_48k_mode:
    call set_return_vector
    call restore_ay_registers
    call MF3_CODE_5FF7

    ld hl,MF3_SAVED_BANKM
    jr z,skip_bank_reset
    res ROM_SEL0_BIT,(hl)
skip_bank_reset:
    call reset_bank2_rom0
    call MF3_CODE_5FF7
    ex af,af'    
    call select_rom0_and_switch
    ex af,af'    
    ld a,(MF3_SAVED_BANKM)
    jr z,nmi_clear_48k_bit3
    ld hl,MF3_STATUS_FLAGS
    set MF3_STATUS_FLAGS_48K_LOCKED,(hl)
    jr nmi_store_bank2_val
nmi_clear_48k_bit3:
    ;; Not 48K mode; clear bit 3 of saved BANK2 state
    res 3,a
nmi_store_bank2_val:
    ;; Store restored BANK2 value to MF3_SAVED_BANKM and MF3_SAVED_BANKM_COPY
    ld (MF3_SAVED_BANKM),a
    ld (MF3_SAVED_BANKM_COPY),a

    ;; cleanup from the call to set_return_vector.
    ;; this restores the machine ram written during that call.

    call cleanup_return_vector

    ;; You can jump from the main menu, and you can also pre-program  Multiface to jump
    ;; directly upon pressing the red button and by-pass  M3 operating system entirely. To
    ;; program the "direct jump", POKE 8192-3 with the jump address as usual, and then also
    ;; 8195-7 with a special identification code word RUN (i. e. 82, 85, 87). Whenever you push
    ;; the button now, you will jump to the predefined address and not even see the M3 menu.
    ;; To return from your program to the program you stopped, use RST 0. To revert back to the
    ;; Multiface normal operation, press the red button and BREAK key simultaneously - this also
    ;; cancels the code word RUN. 

    ;; jump enable checking. Check for RUN at 0x2003-0x2005
    ;; this checking occurs when we press the red button
    
    ld hl,MF3_RUN_ADDR  ; the RUN address.

    ld a,(hl)    
    cp 052h  ; Compare withR
run_code_mismatch_r:
    ;; First char of RUN code is not 'R'; skip direct jump
    jr nz,nojump

    inc hl  ; move to 0x2004

    ld a,(hl)    
    cp 055h  ; Compare with U
    jr nz,nojump

    inc hl  ;move to 0x2005

    ld a,(hl)    
    cp 04eh  ; CompaN

    jp z, JUMP  ; jump is enabled.

    ;; ----------------------------------------------------------------
    ;; Jump Abort - Clear auto-jump signature
    ;; ----------------------------------------------------------------
jumpabrt:
    ld (hl),0000h  ;clear the auto jump. delete the N.

    ;; ----------------------------------------------------------------
    ;; No Jump - Continue normal NMI handling
    ;; Saves BASIC variables and initializes MF3 state
    ;; ----------------------------------------------------------------
nojump:
    ;; disable interrupts
    di    

    ;; take a copy of the basic variables
    ;; and copy them into the multiface ram
    
    ld hl,SWAP
    ld de,0202dh
    ld bc,005b3h
    ldir        
    
    ld hl,SWAP
    ld bc,00ffh  ; 256 bytes

    call clear_ram
    call check_48kmode

    jr z,cancel_run_code

    call reset_bank2_rom0

    ld hl,screen_save_data
    ld de,RAMAREA
    ld bc,0037h
    ldir
    
    call RAMAREA
    call set_bank0_rom3
cancel_run_code:
    call copy_interop
    call save_menu_screen
    jp bank_switch_routine

scan_ay_registers:
    call swap_plus3dos
    call swap_mf3_page1_4k

    ;; ================================================================
    ;; Save Menu Screen Area
    ;; Preserves bottom 16 lines of screen to MF3 RAM
    ;; Saves screen data (8 x 64 bytes) and attributes (64 bytes)
    ;; Allows menu to be overlaid without destroying screen content
    ;; ================================================================
    ;; this routine saves the screen for the menu area to the mf3 ram temp area.

save_menu_screen:

    call set_bank0_rom3
    call decompress_screen_if_needed

    ld hl,050c0h
    ld de,MF3TEMP


    ;; loop 8 times.
    ld b,008h

save_menu_loop:

    push bc
    ld bc,00040h
    ldir

    ld l,0c0h
    pop bc
    djnz save_menu_loop

    ;;  copy the attributes of the last 2 rows of the screen to the mf3 ram

    ld hl,05ac0h
    ld bc,00040h

    ;; ----------------------------------------------------------------
    ;; LDIR and Return
    ;; Common exit point - performs LDIR then returns
    ;; Saves ROM space by sharing this sequence
    ;; ----------------------------------------------------------------
ldir_ret:
    ldir
    ret    

    ;; ================================================================
    ;; Copy Interop Routines to Main RAM
    ;; Copies interop function block to RAMAREA (0x5FE0)
    ;; These allow calling Spectrum ROM with MF3 temporarily paged out
    ;; Size: 0xD3 (211) bytes
    ;; ================================================================

copy_interop:
    ;; 
    ld hl,interop_start
    ld de,RAMAREA
    ld bc,00d3h
    
    ;; relative jump to an ldir, ret sequence to save rom space

    jr ldir_ret
    ;; implied ret


    ;; ================================================================
    ;; Check 48K Mode Lock
    ;; Tests if system is locked in 48K mode (no +3DOS access)
    ;; Returns: Z flag set = locked, Z flag clear = unlocked
    ;; ================================================================

check_48kmode:    

    ld hl,MF3_STATUS_FLAGS
    bit MF3_STATUS_FLAGS_48K_LOCKED,(hl)
    ret

    ;; ================================================================
    ;; Set Return Vector Routine
    ;; Saves RAMAREA to MF3TEMP, copies search code to RAMAREA
    ;; Searches OS ROM for safe return sequence (POP AF, RET)
    ;; This is critical for returning control to Spectrum after MF3 exit
    ;; ================================================================

set_return_vector:
    
    ;; first save the destination area
    ld hl,RAMAREA
    push hl
    
    ld de,MF3TEMP
    ld bc,42
    push bc    

    ldir

    ;; now copy the search code
    ld hl, search_rom_return
    
    ;; restore the count
    pop bc    
    pop de
    ;; actually does the copy
    ldir             

    ;; search between these two areas.
    
    ld hl,0260bh
    ld de,03fc3h

    call RAMAREA

    ;;  get the result and subtract 4
    ld hl,(MF3_DOS_PTR)

    ;; now subtract 4
    dec hl    
    dec hl    
    dec hl    
    dec hl    

    ;; put the result in 2027
    ld (MF3_RET_VECTOR),hl
    
    ret    

    ;; ================================================================
    ;; ROM Return Vector Search Code
    ;; This code is copied to RAMAREA and executed with MF3 paged out
    ;; Searches Spectrum ROM for safe return point (POP AF, RET sequence)
    ;; Search range: HL (0x260B) to DE (0x3FC3)
    ;; Result stored at 0x6008, fallback is 0x4004
    ;; ================================================================
search_rom_return:

    ;; page out the MF3.
    in a,(P_MF3_OUT)

    ;; this code seems to search between HL -> DE for the byte sequence F1,C9 (pop AF, ret)
    ;; if it fails to find it it uses the address 0x4004h
    ;; the result is placed in 0x6008
    ;;
    ;; Bit of a nasty edge case here if the last entry in
    ;; the search range is F1 the loop end check will fail.
    
search_loop:
    
    and a
    
    sbc hl,de
    add hl,de    

    ;; if hl = de then fail.
    jr z,search_fail

    ld a,(hl)    
    inc hl
    
    cp 0f1h  ; look for 0xf1, pop AF
    jr nz, search_loop
    
    ld a,(hl)    
    inc hl    
    cp 0c9h  ; look for C9, ret
    
    jr nz,search_loop
    
search_found:
    
    ;; store the result location at
    ;; 0x6008
    
    ld (MF3_DOS_PTR),hl

    ;; page out the multiface
    in a,(P_MF3_OUT)

    ;; read the first byte in ram
    ld a,(rom_start+1)

    ;; compare it with 0xAF and save the flags
    cp 0afh
    ex af,af'    

    ;; page the MF3 in
    in a,(P_MF3_IN)

    ;; restore the flags and return to the MF3 rom
    ex af,af'    
    ret
    
search_fail:

    ;; GPT: original ROM falls back to 0x4004 here. That strongly suggests
    ;; a guaranteed writable screen-RAM stub location rather than a genuine
    ;; discovered return vector, but the exact historical intent is unclear.
    ld hl,04004h
    jr search_found
    nop    

screen_save_data:
    ;; page the MF3 out
    in a,(P_MF3_OUT)

    ;; Copy a small ROM workspace block into SWAP before seeding +3 system vars.
    ld hl,0x00BD
    ld de,SWAP
    ld bc,0052h
    ldir

    ;; GPT: 0xCF07 is copied verbatim from the original ROM. It appears to be a
    ;; canned +3 banking workspace seed, but the provenance of this exact value
    ;; is still unverified.
    ld hl,0cf07h

    ld (BANKM),hl

    ;; GPT: the `ld a,014h` is preserved from the original binary. Locally it
    ;; looks dead because HL, not A, is stored next. Keep it until the wider
    ;; screen-save path proves whether A is consumed indirectly.
    ld a,014h
    ld (BANK678),hl
    
    ;; Default both +3DOS drive selectors to drive A.
    ld a,'A'
    ld (LODDRV),a
    ld (SAVDRV),a

    ;; page the mf3 in again
    in a,(P_MF3_IN)

    ;; switch to the alternate screen

set_bank0_rom_x1:

    ;; ================================================================
    ;; Set Bank 0 with ROM Selection
    ;; Pages in bank 0 and selects ROM 0 (48K BASIC/Syntax ROM)
    ;; Actual ROM depends on bit 2 of BANK2 port
    ;; ================================================================
    ;; set bank 0 and ensure 48k basic/syntax rom is paged in.
    ;; the actual rom banked in depends on bit 2 of bank2
    ld a, ROM_SEL0

    ;; implied call to switch bank.

    ;; ================================================================
    ;; Switch Memory Bank
    ;; Outputs A to port 0x7FFD (BANK1) to change memory configuration
    ;; ================================================================
switch_bank:
    ld bc,BANK1

    ;; ----------------------------------------------------------------
    ;; OUT (C),A and Return
    ;; Common exit - outputs A to port C then returns
    ;; Saves ROM space by sharing this sequence
    ;; ----------------------------------------------------------------
out_c_ret:
    out (c),a
    ret
    
;; ================================================================
;; Save DOS Workspace
;; Prepares DOS environment and switches to bank 7
;; Activates ROM 1 and disk motor
;; ----------------------------------------------------------------
;; Uses:   A, BC (via called routines)
;; ================================================================
save_dos_workspace:
    call select_rom1_motor_on

    ;; set bank 7
    ld a,(BANKM)
    or 007h
    ld (BANKM),a

    jr switch_bank
    ;; implied ret

    ;; ================================================================
    ;; Set Bank 0 with ROM 3
    ;; Pages in bank 0 and +3 BASIC ROM (ROM 3)
    ;; Sets border to white (color 7)
    ;; ================================================================

set_bank0_rom3:
    ;; set the border white
    ld a,WHITE
    out (0feh),a

    call set_bank0_rom_x1
    ld (BANKM),a

;; ================================================================
;; Select ROM 1, Printer, and Bank 2
;; Configures system for ROM 1 with printer interface
;; Sets bank 2 paging mode
;; ----------------------------------------------------------------
;; Uses:   A, BC (via set_bank2)
;; ================================================================
select_rom1_printer_bank2:
    ld a, ROM_SEL1 | PLUS3_PRINTER
    ld (BANK678),a
    ;; set bank 2
    jr set_bank2
    ;; implied ret


    ;; ----------------------------------------------------------------
    ;; Select ROM 0 (Entry Point 1)
    ;; ----------------------------------------------------------------
select_rom0_and_switch:
    ld a, ROM_SEL0

    ;; ----------------------------------------------------------------
    ;; Select ROM and Switch Bank (Entry Point 2)
    ;; ----------------------------------------------------------------
switch_bank_restore_ay:
    call switch_bank

;; ================================================================
;; Restore AY Registers
;; Restores AY sound chip register state
;; Sets BANK2 to ROM 1 configuration (upper nibble)
;; ----------------------------------------------------------------
;; Uses:   A, BC (via set_bank2_fupper)
;; ================================================================
restore_ay_registers:
    ld a,ROM_SEL1<<4

    ;; ================================================================
    ;; Set BANK2 Control from Packed Upper Nibble
    ;; Moves A[7:4] -> A[3:0] for subsequent BANK2 write
    ;; ================================================================
    ;; Used when BANK2 state is stored in the upper nibble (e.g. MF3_RESTORE_FLAGS).
set_bank2_fupper:
    rrca  ; a >> 4
    rrca
    rrca
    rrca
    and 00fh

    ;; ================================================================
    ;; Set Bank 2 (0x1FFD port)
    ;; Writes BANK2 control bits (0-3) to 0x1FFD
    ;; Forces printer strobe bit high (inactive) on every write
    ;; ================================================================
    ;; Set BANK2 from A low nibble, preserving inactive printer strobe.
set_bank2:
    or PLUS3_PRINTER
    ld bc, BANK2
    jr out_c_ret
    ;; implied ret

    ;; ================================================================
    ;; Set ROM 0 and Screen Bank
    ;; Switches to ROM 0 and normal screen memory
    ;; ================================================================
    ;; set rom0 and screen to bank2
set_rom0:    
    ld a,(BANK678)

    ;; mask off bits 0-2
    ;; preserve other bits
    
    and ~0x07
    call set_bank2
    ld (BANK678),a

;; ================================================================
;; Set ROM 0 and Normal Screen
;; Selects ROM 0, normal screen memory (not shadow)
;; Clears ROM_SEL0 and BANK1_SCREEN bits in BANKM
;; ----------------------------------------------------------------
;; Uses:   A, BC (via switch_bank)
;; ================================================================
set_bank2_defaults:
    ld a,(BANKM)    

    ;; 
    and ~ROM_SEL0 & ~BANK1_SCREEN
    ld (BANKM),a
    jr switch_bank
    ;;  implied ret

;; ================================================================
;; Reset Bank 2 and ROM 0
;; Resets BANK2 port to 0 and switches to ROM 0
;; Clears all special +3 banking modes
;; ----------------------------------------------------------------
;; Uses:   A, BC (via set_bank2_fupper and switch_bank)
;; ================================================================
reset_bank2_rom0:
    xor a
    ;; not sure why set_bank2 isnt just called here?
    call set_bank2_fupper
    xor a
    jr switch_bank

;; ================================================================
;; Select ROM 1 and Motor On
;; Activates ROM 1, turns on disk motor, deactivates printer strobe
;; Used for +3DOS operations requiring disk access
;; ----------------------------------------------------------------
;; Uses:   A, BC (via set_bank2 and set_bank2_defaults)
;; ================================================================
    ;
select_rom1_motor_on:
    ld a,(BANK678)
    ;; motor on, printer strobe off (active low).
    or ROM_SEL1 | PLUS3_PRINTER 
    ld (BANK678),a

    call set_bank2
    jr set_bank2_defaults

;; ================================================================
;; Find ROM Return Vector
;; Searches compressed data for return vector information
;; Validates compression and stores decompression pointers
;; ----------------------------------------------------------------
;; Uses:   HL, DE, BC, A
;; ================================================================
find_rom_return_vector:
    ld hl,0c000h
    push hl    
    ld bc,rom_start

    call find_compression_marker

    pop de    
    push hl    
    and a    
    sbc hl,de
    ex de,hl    
    ld hl,SCREEN
    and a    
    sbc hl,de
    pop hl    
    jr z,measure_bank4
    push de    
    ex de,hl    
    ld hl,0c000h
    ld c,(hl)    
    inc hl    
    ld b,(hl)    
    ld l,c    
    ld h,b    
    ex de,hl    
    ld (hl),e    
    inc hl    
    ld (hl),d    
    pop hl    
    inc hl    
    inc hl    
    ex de,hl    
    ld hl,0c000h
    ld (hl),e    
    inc hl    
    ld (hl),d    
    xor a    
    inc a    
measure_bank1:
    scf    
    ccf    
    ret    
measure_bank3:
    xor a    
    jr measure_bank1
measure_bank4:
    ld hl,0c000h
measure_bank6:
    xor a    
    cp (hl)    
    jr nz,measure_bank3
    inc hl    
    ld a,l    
    or h    
    jr nz,measure_bank6
    scf
    ret

;; ================================================================
;; Measure Compressed Sizes
;; Calculates sizes of compressed screen and RAM data
;; Sets flags at RAMAREA bits 6 and 7 when measurements complete
;; Stores sizes at 0x5FFC (screen) and 0x5FFA (RAM)
;; ----------------------------------------------------------------
;; Uses:   HL, DE, BC, A
;; ================================================================
measure_compressed_sizes:
    ld a,(RAMAREA)
    bit RAMAREA_SCREEN_MEASURED,a
    jr nz,measure_done
    ld hl,SCREEN
    push hl    
    ld bc,SWAP
    call find_compression_marker
    pop de    
    and a    
    sbc hl,de
    ld (MF3_SCREEN_SIZE),hl
    ld hl,RAMAREA
    set RAMAREA_SCREEN_MEASURED,(hl)
measure_done:
    call set_bank0_rom_x1
    ld a,(RAMAREA)
    bit RAMAREA_SYSVARS_MEASURED,a
    ret nz    
    ld hl,060b3h
    ld (MF3_SAVED_HL2),hl
    ld bc,rom_start
    ld (MF3_SAVED_BC2),bc
    call find_compression_marker
    ld (MF3_PTR_5FFE),hl
    ld de,RAMAREA
    and a    
    sbc hl,de
    ld (MF3_DISPLAY_ADDR),hl
    ld hl,RAMAREA
    set RAMAREA_SYSVARS_MEASURED,(hl)
    ret
;; ----------------------------------------------------------------
pop_hl_ret:
    pop hl
    ret

;; ================================================================
;; Find Compression Marker
;; Searches memory for compression marker sequence (0x37, 0xED, 0xCB)
;; Returns pointer to compressed data or performs decompression
;; ----------------------------------------------------------------
;; Input:  HL = start address, BC = limit
;; Output: HL = marker location, Carry = found/not found
;; Uses:   HL, DE, BC, A, alternate registers
;; ================================================================
find_compression_marker:
    push bc    
    ld a,(MF3_STATUS_FLAGS)
find_marker_start:
    bit MF3_STATUS_FLAGS_SKIP_MARKER_SCAN,a
    jr nz, pop_hl_ret  ;implied ret

    push hl    
    dec hl    
    dec bc    
    dec bc    
marker_search_loop:
    inc hl    
    and a    
    sbc hl,bc
    jr nc,validate_bank_flags
    add hl,bc    
    ld a,(hl)    
    cp 037h
    jr nz,marker_search_loop
    inc hl    
    ld a,(hl)    
    cp 0edh
    jr nz,marker_search_loop
    inc hl    
    ld a,(hl)    
    cp 0cbh
    jr nz,marker_search_loop
    scf    
validate_bank_flags:
    pop hl    
    jr c, pop_hl_ret
    ;; Inline decompression entry: no compression marker found, do RLE copy/compress
decompress_inline_entry:
    exx    
    pop bc    
    push hl    
    exx    
    push hl    
    pop de    
    ;; Inner byte-compare loop: scan source against ROM template
decompress_inner_compare_loop:
    ld bc,rom_start
    ld a,(de)    
    ;; Byte-by-byte copy inner loop: copy matching bytes forward
decompress_byte_copy_loop:
    inc de    
    inc bc    
    push de    
    exx    
    pop hl    
    and a    
    sbc hl,bc
    exx    
    jr z,decompress_bytes_match
    ex de,hl    
    cp (hl)    
    ex de,hl    
    jr z,decompress_byte_copy_loop
    ;; Reached when source and ROM bytes match
decompress_bytes_match:
    push af    
    ex af,af'    
    ld a,c    
    and 0f8h
    or b    
    jr nz,decompress_marker_encode
    pop af    
    push af    
    ld b,c    
    ;; Literal-run copy loop: write matched bytes to output
decompress_literal_run_copy:
    ld (hl),a    
    inc hl    
    djnz decompress_literal_run_copy
    ;; Continue/exit check after literal copy
decompress_literal_exit_check:
    pop af    
    jr nz,decompress_inner_compare_loop
    exx    
    pop hl    
    exx    
    ret    
    ;; Marker-encode branch: write compressed marker 0x37,0xED,0xCB with RLE count
decompress_marker_encode:
    ld (hl),c    
    inc hl    
    ld (hl),b    
    inc hl    
    ex af,af'    
    ld (hl),a    
    inc hl    
    ld (hl),037h
    inc hl    
    ld (hl),0edh
    inc hl    
    ld (hl),0cbh
    inc hl
    jr decompress_literal_exit_check

;; ================================================================
;; Scan 128K Banks
;; Scans all 128K memory banks for compressed data
;; Validates each bank and stores decompression pointers
;; Sets flags for banks containing valid compressed data
;; ----------------------------------------------------------------
;; Uses:   HL, DE, BC, A (all registers via called routines)
;; ================================================================
scan_128k_banks:
    call clear_bank_flags
    call check_48kmode
    ret z  ;return if we are locked in 48k mode
    
    bit MF3_STATUS_FLAGS_SAVE_128K_LOCKED,(hl)
    ret nz    
    ld a,011h
    call validate_compressed_bank
    jr c,bank_scan_loop
    ld (MF3_BANK1_SIZE),de
    set MF3_BANK1_PRESENT_BIT,(hl)
    jr z,bank_scan_loop
    set MF3_BANK1_COMPRESSED_BIT,(hl)
bank_scan_loop:
    ld a,013h
    call validate_compressed_bank
    jr c,bank_scan_next
    ld (MF3_BANK3_SIZE),de
    set MF3_BANK3_PRESENT_BIT,(hl)
    jr z,bank_scan_next
    set MF3_BANK3_COMPRESSED_BIT,(hl)
bank_scan_next:
    ld a,014h
    call validate_compressed_bank
    jr c,bank_check_next
    ld (MF3_BANK4_SIZE),de
    set MF3_BANK4_PRESENT_BIT,(hl)
    jr z,bank_check_next
    set MF3_BANK4_COMPRESSED_BIT,(hl)
bank_check_next:
    ld a,016h
    call validate_compressed_bank
    jr c,bank_scan_bank7
    ld (MF3_BANK6_SIZE),de
    set MF3_BANK6_PRESENT_BIT,(hl)
    jr z,bank_scan_bank7
    set MF3_BANK6_COMPRESSED_BIT,(hl)
bank_scan_bank7:
    ld a,017h
    call validate_compressed_bank
    ld hl,MF3_BANK_BUFFER2
    jr c,scan_bank7_exit
    ld (06022h),de
    set MF3_BANK7_PRESENT_BIT,(hl)
    jr z,scan_bank7_exit
    set MF3_BANK7_COMPRESSED_BIT,(hl)
    ;; Exit point after bank 7 validation in scan_128k_banks
scan_bank7_exit:
    call set_bank0_rom_x1
    xor a
    inc a
    ret

;; ================================================================
;; Validate Compressed Bank
;; Checks if a memory bank contains valid compressed data
;; Validates compression header and decompresses if valid
;; ----------------------------------------------------------------
;; Input:  A = bank number to validate
;; Output: Carry = valid/invalid, DE = decompression pointer
;; Uses:   HL, DE, BC, A
;; ================================================================
validate_compressed_bank:
    call switch_bank
    call find_rom_return_vector
    jr z,validate_bank_zeroed_check
    ld a,(0c002h)
    or a    
    jr nz,decompress_next
    ld de,(0c000h)
    ld hl,0008h
    and a    
    sbc hl,de
    jr nz,decompress_next
    call decompress_bank_data
    ;; Bank is compressed/valid exit (carry set)
validate_bank_valid_exit:
    ld hl,MF3_BANK_BUFFER
    scf    
    ret    
    ;; Bank has no data (zeroed) - check if empty
validate_bank_zeroed_check:
    jr c,validate_bank_valid_exit
decompress_next:
    jr nz,validate_bank_not_present_exit
    ld de,SCREEN
    ;; Bank not present - carry clear exit
validate_bank_not_present_exit:
    ld hl,MF3_BANK_BUFFER
    scf
    ccf
    ret

;; ================================================================
;; Reset Compression State
;; Decompresses all compressed banks and clears compression flags
;; ----------------------------------------------------------------
;; Uses:   All registers (via decompress_all_banks)
;; ================================================================
reset_compression_state:
    call decompress_all_banks

;; ----------------------------------------------------------------
;; Clear Bank Flags - Clears 11 bytes of bank status flags
;; ----------------------------------------------------------------
clear_bank_flags:
    ld hl,MF3_BANK_BUFFER
    ld bc,000bh
    jp clear_ram

;; ================================================================
;; Decompress All Banks
;; Decompresses all compressed 128K banks in sequence
;; Processes banks 6, 4, 3, 1, and 7 if compressed
;; ----------------------------------------------------------------
;; Uses:   A, HL, BC (via decompress_from_bank)
;; ================================================================
decompress_all_banks:
    ld a,(MF3_BANK_BUFFER)
    rlca    
    jr c,decompress_bank6_start
decompress_bank1:
    rlca    
    jr c,decompress_bank4_start
decompress_bank3:
    rlca    
    jr c,decompress_bank3_start
decompress_bank4:
    rlca    
    jr nc,decompress_check_bank7
    ld a,011h
    call decompress_from_bank
    ;; Check and decompress bank 7 (after bank 6 is done)
decompress_check_bank7:
    ld a,(MF3_BANK_BUFFER2)
    bit MF3_BANK7_COMPRESSED_BIT,a
    jr z,decompress_all_banks_done
    ld a,017h
    call decompress_from_bank
    ;; Exit to set_bank0_rom_x1 after all banks decompressed
decompress_all_banks_done:
    jp set_bank0_rom_x1
    ;; Decompress bank 6, then fall through to bank 1
decompress_bank6_start:
    push af    
    ld a,016h
    call decompress_from_bank
    pop af    
    jr decompress_bank1
    ;; Decompress bank 4, then fall through to bank 3
decompress_bank4_start:
    push af    
    ld a,014h
    call decompress_from_bank
    pop af    
    jr decompress_bank3
    ;; Decompress bank 3, then fall through to bank 4
decompress_bank3_start:
    push af    
    ld a,013h
    call decompress_from_bank
    pop af
    jr decompress_bank4

;; ================================================================
;; Decompress From Bank
;; Switches to specified bank and decompresses its data
;; ----------------------------------------------------------------
;; Input:  A = bank number to decompress
;; Uses:   All registers (via decompress_bank_data)
;; ================================================================
decompress_from_bank:
    call switch_bank

;; ----------------------------------------------------------------
;; Decompress Bank Data - Decompresses data in current bank
;; ----------------------------------------------------------------
decompress_bank_data:
    ld hl,0c000h
    ld c,(hl)    
    inc hl    
    ld b,(hl)    

    dec bc    
    dec bc    
    dec hl    
    add hl,bc    

    push hl    

    ld e,(hl)    
    inc hl    

    ld d,(hl)    
    ld hl,0c000h
    ld (hl),e    
    inc hl    

    ld (hl),d    
    ld de,rom_start

    pop hl    

    ld ix,0c000h
    call decompress_to_ram
    xor a

    ret

;; ================================================================
;; Decompress To RAM
;; Core decompression routine - RLE decompressor
;; Decompresses data backwards from HL to DE
;; Handles marker sequence: 0x37, 0xED, 0xCB, count(2), value(1)
;; ----------------------------------------------------------------
;; Input:  HL = source end, DE = count, IX = dest end
;; Uses:   HL, DE, BC, IX, A, AF'
;; ================================================================
decompress_to_ram:
    dec hl    
    dec de    
    dec ix
    and a    
    sbc hl,de
    add hl,de    
    ret z    
    ;; Main RLE decompress loop: check remaining count
rle_main_loop:
    push hl    
    push de    
    push ix
    pop de    
    and a    
    sbc hl,de
    pop de    
    pop hl    
    ret z    
    ld a,(hl)    
    push af    
    cp 0cbh
    dec hl    
    jr z,rle_found_cb_check_ed
    ;; Literal byte copy (not part of RLE run)
rle_literal_byte_copy:
    pop af    
    ld (de),a    
    dec de    
    jr rle_main_loop
    ;; Found 0xCB, check for 0xED before it
rle_found_cb_check_ed:
    ld a,(hl)    
    cp 0edh
    jr nz,rle_literal_byte_copy
    dec hl    
    ld a,037h
    cp (hl)    
    jr z,rle_found_full_marker
    inc hl    
    jr rle_literal_byte_copy
    ;; Found full marker 0x37,0xED,0xCB - start RLE run
rle_found_full_marker:
    pop af    
    dec hl    
    ld a,(hl)    
    dec hl    
    ld b,(hl)    
    dec hl    
    ld c,(hl)    
    dec hl    
    ;; RLE run copy loop: repeat fill byte for BC count
rle_run_copy_loop:
    ld (de),a    
    dec de    
    dec bc    
    ex af,af'    
    ld a,b    
    or c    
    jr z,rle_main_loop
    ex af,af'
    jr rle_run_copy_loop

;; ================================================================
;; Decompress Screen If Needed
;; Checks if screen is compressed (bit 6 of RAMAREA)
;; If compressed, decompresses screen data
;; Then checks and decompresses system variables if needed
;; ----------------------------------------------------------------
;; Uses:   HL, DE, IX, A (all registers via called routines)
;; ================================================================
decompress_screen_if_needed:
    ld a,(RAMAREA)
    bit RAMAREA_SCREEN_MEASURED,a
    jr z,screen_not_compressed_check_sysvars

;; ----------------------------------------------------------------
;; Decompress Screen - Decompresses screen memory
;; ----------------------------------------------------------------
decompress_screen:
    ld ix,SCREEN
    push ix
    pop hl    
    ld de,(MF3_SCREEN_SIZE)
    add hl,de    
    ld de,SWAP
    call decompress_to_ram
    ld hl,RAMAREA
    res RAMAREA_SCREEN_MEASURED,(hl)
    ;; Screen not compressed, check sysvars flag for decompression
screen_not_compressed_check_sysvars:
    ld a,(RAMAREA)
    bit RAMAREA_SYSVARS_MEASURED,a
    ret z

;; ----------------------------------------------------------------
;; Decompress System Variables - Decompresses system vars area
;; ----------------------------------------------------------------
decompress_sysvars:
    call set_bank0_rom_x1
    ld hl,(MF3_PTR_5FFE)
    ld ix,(MF3_SAVED_HL2)
    ld de,rom_start
    call decompress_to_ram
    ld hl,RAMAREA
    res RAMAREA_SYSVARS_MEASURED,(hl)
    ret

;; ================================================================
;; Swap Bank to 0x8000
;; Swaps MF3 page 1 and current bank to address 0x8000
;; Composite operation combining two swap operations
;; ----------------------------------------------------------------
;; Uses:   All registers (via called swap routines)
;; ================================================================
swap_bank_to_8000h:
    call swap_mf3_page1_4k
    call swap_current_bank_to_8000h

    ;; ================================================================
    ;; Swap MF3 Page 1 (4KB)
    ;; Swaps 4KB block between Multiface RAM and Spectrum bank 1
    ;; ================================================================
swap_mf3_page1_4k:

    ;; swap 4k between mf3 ram and spectrum bank 1
    ld a,(BANKM)
    and 0f0h
    or 001h
    call switch_bank

    ld hl,0c000h
    ld de,02f1bh
    ld bc,0x1000
    jr swap_buffers
    ;; implied ret

;; ================================================================
;; Swap Bank 7 to 0x8000
;; Switches to bank 7 and swaps it with memory at 0x8000
;; ----------------------------------------------------------------
;; Uses:   A, BC, HL, DE (via switch_bank and swap)
;; ================================================================
swap_bank7_to_8000h:

    ;; mask off the lower 3 bits.
    ld a,(BANKM)
    or 007h  ; select bank7

    ;; bank switch
    
    call switch_bank

    ;; swap the current top page of ram into page 5 (0x8000)

;; ================================================================
;; Swap Current Bank to 0x8000
;; Swaps currently selected bank (at 0xC000) with memory at 0x8000
;; Swaps 16KB (0x4000 bytes)
;; ----------------------------------------------------------------
;; Uses:   HL, DE, BC, AF, AF' (via swap_buffers)
;; ================================================================
swap_current_bank_to_8000h:

    ld hl,0c000h
    ld de,08000h
    ld bc,SCREEN

    ;; ================================================================
    ;; Core Memory Swap Routine
    ;; Exchanges BC bytes between (HL) and (DE)
    ;; Uses alternate AF register for temporary storage
    ;; Corrupts: AF, AF'
    ;; ================================================================

swap_buffers:


    ;;  take a copy of (HL)
    ld a,(hl)
    ex af,af'

    ;; copy (DE) to (HL)
    ld a,(de)
    ld (hl),a

    ;; restore the original (HL) value and write to (DE)
    ex af,af'
    ld (de),a

    ;; increment the pointers
    inc hl
    inc de

    ;; decrement the counter
    dec bc

    ;; if we're not finished loop again
    ld a,b
    or c
    jr nz,swap_buffers

    ret

    ;; ================================================================
    ;; Swap +3DOS Workspace
    ;; Preserves/restores +3DOS system variables during context switch
    ;; Swaps 0xDB00 (0x939 bytes) and 0xE600 (256 bytes) with MF3 RAM
    ;; Call twice to restore (swap is reversible)
    ;; ================================================================

swap_plus3dos:

    ;; swap bank 7.

    ld a,(BANKM)
    and 0f0h
    or 007h

    ;; make the bank switch

    call switch_bank

    ;; save the current bank.

    ld (BANKM),a

    ;; copy the +3DOS variable area.

    ld hl,0db00h
    ld de,MF3TEMP
    ld bc,save_fn_punct_check  ; 0x0939 = +3DOS workspace size

    call swap_buffers

    ;; copy e600 into/outof the mf3 ram directly after the last copy

    ld hl,0e600h
    ld bc,rom_start+1
    jr swap_buffers
    ;; implied ret
    
nmi_restore_workspace:
    ;; Restore workspace before exit: copy 0x602A screen data to 0x3FE4
    ld hl,0602ah
    ld de,03fe4h
    ld bc,0001ch
    ldir

    ld hl,SCREEN
    ld de,SWAP
    ld bc,005b3h
    ldir

    ld hl,0202dh
    ld de,SCREEN
    ld bc,005b3h
    ldir
    
code_start:
    ld hl,MF3_STATUS_FLAGS
    set MF3_STATUS_FLAGS_ENABLED,(hl)
    jr nmi_exit_setup

nmi_restore_menu_exit:
    ;; Restore menu screen, swap screen if 128K, copy SWAP data
    call restore_menu_screen
    call swap_screen_if_128k
    ld hl,0202dh
    ld de,SWAP
    ld bc,005b3h
    ldir
nmi_exit_setup:
    ;; IM1 setup, load return vector, set SP, call delay
    im 1
    ld hl,(MF3_RET_VECTOR)
    ld e,(hl)    
    inc hl    
    ld d,(hl)    
    ld (02029h),de
    ld sp,03fd4h
    ld b,001h
    
    call delay_loop

    ld a,(MF3_SAVED_BANKM)
    call switch_bank

    ;; restore the 4 bits of BANK2. these are taken
    ;; from the upper 4 bits of MF3_RESTORE_FLAGS

    ld a,(MF3_RESTORE_FLAGS)
    call set_bank2_fupper

    ;; get the return vector
    call set_return_vector
    ;; cleanup after. restore ram.
    call cleanup_return_vector
    
    ld hl,03fe3h
    ld de,00f10h
    ld c,0fdh

ay_restore_write_loop:
    ;; Write back AY register values from saved buffer to sound chip
    ld b,0ffh
    out (c),d
    ld a,(hl)    
    ld b,P_MF3_OUT
    out (c),a
    dec hl    
    dec d    
    dec e    
    jr nz,ay_restore_write_loop
    ld sp,03fe4h
    pop de    
    ld hl,(03ffeh)
    ld (hl),e    
    inc hl    
    ld (hl),d    
    pop iy
    pop ix
    exx    
    pop bc    
    pop de    
    pop hl    
    exx    
    ex af,af'    
    pop af    
    ex af,af'    
    pop bc    
    pop de    
    pop af    
    ld r,a
    pop af    
    ld i,a
    ld a,(MF3_RESTORE_FLAGS)
    bit MF3_RESTORE_FLAGS_IM2,a
    jr z,nmi_write_return_jp
    im 2
    
nmi_write_return_jp:
    ;; Write JP instruction to return stub at 0x2026
    ld a,0c3h

    ;; writes a return program to the ret vector if the ram there is writeable.
    ld (02026h),a
    ld hl,(MF3_RET_VECTOR)

    ;; write a command to page out the mf3 in mf3 ram at the return vector.
    ;; command is IN a, MF3_OUT
    
    ld (hl),0dbh
    inc hl
    ld (hl),P_MF3_OUT

    ;; test if we are still in rom?
    ;; if not then after the mf3 is paged out
    ;; these bytes will be present in rom.
    ;; otherwise we need to write them ourselves.

    ld a,h        
    cp 040h
    jr nz, skip_because_rom 

    ;; corrupt the screen to allow the return to function.
    
    ;; write pop af;
    inc hl    
    ld (hl),0f1h
    ;; write ret 
    inc hl    
    ld (hl),0c9h
    
skip_because_rom:

    pop hl    
    pop af    

    ld sp,(03ffeh)
    push af    

    ld a,(MF3_STATUS_FLAGS)
    bit MF3_STATUS_FLAGS_ENABLED,a
    jr z,nmi_page_out_mf3

    ;; multiface3 lockout?
    out (P_MF3_BUTTON),a
    jr nmi_check_ei_flag
nmi_page_out_mf3:
    ;; Page out MF3 on enabled path
    ;; page out the multiface
    out (P_MF3_OUT),a
nmi_check_ei_flag:
    ;; Check MF3_RESTORE_FLAGS bit 2 (EI flag); if set, re-enable interrupts
    ld a,(MF3_RESTORE_FLAGS)
    bit MF3_RESTORE_FLAGS_EI,a
    jr z,nmi_jump_return_stub
    ei    
nmi_jump_return_stub:
    ;; Jump to return stub at 0x2026 to exit MF3
    jp 02026h
display_main_menu:
    
    call cls_lower

    ;; ================================================================
    ;; Print Main Menu
    ;; Displays Multiface 3 main menu with options
    ;; Shows different menu based on 48K lock status
    ;; Options: [r]eturn [s]ave [t]ool [l]oad [p]rint
    ;; ================================================================
print_mainmenu:

    ;; print the first part of the main menu
    ld hl, msg_mainmenu
    ld b,02ch
    call printf

    ;; do a 48k mode check
    call check_48kmode
    jr z, @print_locked

    ;; print the rest of the menu if
    ;; +3DOS is available.

    ld hl, msg_menu_unlocked
    ld b,019h

    ;; skip printing the locked message
    jr @skip_locked

@print_locked:

    ld hl, msg_locked
    ld b,00eh

@skip_locked:

    call printf

    ;; print the multiface  message with attributes.

    ld hl, msg_multiface_attrs
    ld b,024h
    call printf

    ld a,(MF3_STATUS_FLAGS)
    bit MF3_STATUS_FLAGS_ENABLED,a
    ret z

    ;; display the "[o]ff"  message on the main menu
    ld hl,msg_off
    ld b,005h

    ;; ================================================================
    ;; Print String
    ;; Prints B characters from address in HL
    ;; Uses RST 30h (which calls printa routine)
    ;; ================================================================
printf:  ;print routine?
    ld a,(hl)
    rst 30h
    inc hl
    djnz printf
    ret    

cls_print_header:
    call cls_lower
setup_display:
    ld hl,msg_mainmenu

    ;; print a message at HL with length 9
print_msg_len9:    

    ld b,009h
    jr printf

print_abort:

    call cls_print_header
    ld hl, msg_abort
    ld b,00ah
    jr printf

print_abort2:    

    call print_abort

print_two_spaces:

    ld a,006h
    rst 30h    
    ld a,006h
    rst 30h    

print_mf_attributes:

    ld hl,msg_multiface_attrs
    jr print_msg_len9
    
cls_print_msg:
    call print_mf_attributes

    ;; print the MULTIFACE  message
    
    ld hl,msg_multiface
    ld b,00ch
    call printf

    ;;  print the (C) Romantic Robot Ltd message

    ld hl,msg_copyright
    ld b,014h
    jr printf
    
print_tape_prompt:

    call print_abort2

    ld hl, msg_start_tape
    ld b,013h
    call printf

print_enter:    

    ld hl,msg_enter
    ld b,00ch
    jr printf

msg_locked:    

    defb TAB
    AT 0,22
    FLASH 1
    defb "locked"
    FLASH 0  ; locked message

msg_48k:
    defb " 4"

msg_off:    
    AT 1,30
    defb "ff"

msg_mainmenu:    
    AT 0,0
    BRIGHT 1
    PAPER YELLOW
    INK BLUE

    INVERSE 1
    defb "r"  ;return
    INVERSE 0
    defb "eturn"

    INVERSE 1  ;save
    defb "s"
    INVERSE 0
    defb "ave"

    INVERSE 1  ;tool
    defb "t"
    INVERSE 0
    defb "ool"
    
    INVERSE 1  ;print
    defb "p"
    INVERSE 0
    defb "rint"
    
msg_menu_unlocked:
    INVERSE 1  ;dos
    defb "d"
    INVERSE 0
    defb "os"

    INVERSE 1  ;alter
    defb "a"
    INVERSE 0
    defb "lter"
    
    INVERSE 1  ;clear
    defb "c"
    INVERSE 0
    defb "lear"

msg_multiface_attrs:
    AT 1,0
    INK WHITE      
    PAPER RED
    BRIGHT 1
msg_multiface:    
    defb "MULTIFACE 3 V3.C", 6  ; multiface string
msg_on:    
    AT 1,29
    FLASH 1
    defb "o"
    FLASH 0
    defb "n "
    
msg_copyright:    
    defb 07fh  ;copyright on spectrum
    defb " Romantic Robot Ltd"
msg_tape:
    INVERSE 1
    defb "t"
    INVERSE 0
    defb "ape", TAB, TAB
msg_disk:
    AT 0,11

    INVERSE 1
    defb "d"
    INVERSE 0
    defb "isk"

msg_screen_program:    
    defb "screen / program",6

msg_start_tape:    
    defb "Start tape & press", 6

msg_128k:    
    AT 0,22
    PAPER YELLOW
    INK BLUE
    defb TAB
    AT 0,28
    defb "128"
    FLASH 1
    defb "K"
    FLASH 0

msg_file_not_found:    
    defb "File not found", TAB, TAB
msg_drive_not_ready:    
    defb "Drive not ready", TAB, TAB
msg_ioerror:    
    defb "I/O error -"
msg_any_key:    
    defb " press any key", TAB
    AT 0,0

;; ================================================================
;; Print I/O Error
;; Displays I/O error message and error code
;; ----------------------------------------------------------------
;; Uses:   HL, B, A
;; ================================================================
print_io_error:

    call cls_print_header
    call print_two_spaces

    ld hl, msg_ioerror
    ld b,01dh

    call printf

    ld a,(02007h)
    jp print_byte

    ;; routine to actually display the dos menu

;; ================================================================
;; Print DOS Menu
;; Displays DOS operation menu (load/erase options)
;; ----------------------------------------------------------------
;; Uses:   HL, B
;; ================================================================
print_dosmenu:

    call print_abort
    
    ld hl, msg_dosmenu

    ld b,015h
    jp printf

    ;; dos menu
msg_dosmenu:
    
    INVERSE 1
    defb "l"
    INVERSE 0
    defb "oad "

    INVERSE 1
    defb "e"
    INVERSE 0
    defb "rase ", TAB, TAB, 0

;; ================================================================
;; Print Save Menu
;; Displays save menu options including screen/program and 128K status
;; Handles attribute highlighting for menu options
;; ----------------------------------------------------------------
;; Uses:   HL, B, A
;; ================================================================
print_save_menu:
    call print_mf_attributes

    ld hl,msg_screen_program
    ld b,011h
    call printf
    
    call check_48kmode
    jr z,@skip_48kmode
    
    ld hl,msg_128k
    ld b,013h
    call printf

    ld a,(MF3_STATUS_FLAGS)
    bit MF3_STATUS_FLAGS_SAVE_128K_LOCKED,a
    jr z,@skip_48kmode
    ld hl,msg_locked+1

    ld b,00fh
    call printf
    
    ld hl,MF3_MENU_ATTR_LOCKED_BASE
    ld a,(MF3_STATUS_FLAGS)
    bit MF3_STATUS_FLAGS_SAVE_COMPRESS,a
    jr nz,@skip_48kmode

    ld b,005h
@loop:
    res 7,(hl)
    inc hl    
    djnz @loop
    
@skip_48kmode:
    ld hl,MF3_MENU_ATTR_BASE
    ld a,(MF3_BANK_FLAGS)
    bit MF3_BANK_FLAGS_SAVE_SCREEN_MODE,a
    ld a,009h
    jr nz,save_menu_highlight_screen
    set 7,(hl)
    ld b,007h
    add a,l    
    ld l,a    
save_menu_highlight_span:
    set 7,(hl)
    inc hl    
    djnz save_menu_highlight_span
    ret    
save_menu_highlight_screen:
    ld b,006h
    call save_menu_highlight_span
    inc hl    
    inc hl    
    inc hl    
    set 7,(hl)
    ret    

save_filename_entry:
    call cls_print_msg
    call print_filename_prompt
    call init_filename_buffer
save_filename_redraw:
    call print_filename
    call print_filename_at_cursor
save_filename_keyloop:
    call brkscan

    jr nc,save_filename_abort
    call get_key
    jp z,save_fn_enter_confirmed
    cp 00ch
save_fn_punct_check:    ; branch target when key < ':' (punctuation handled separately)
    jr z,save_filename_backspace
    cp '0'  ; 0
    jr c,save_filename_keyloop
    cp 05bh  ; [
    jr nc,save_filename_keyloop
    cp ':'             
    jr c,save_fn_char_store
    cp 'A'  ; A
    jr c,save_filename_keyloop
save_fn_char_store:    ; alphanumeric character accepted, store in buffer
    ld hl,(MF3_PTR_5FF4)
    ld (hl),a    
    inc hl    
    ld (MF3_PTR_5FF4),hl
    ld a,(MF3_BANK_CONTROL)
    inc a    
    ld (MF3_BANK_CONTROL),a
    call calc_filename_space
    jp c,save_fn_buf_full
    jr save_filename_redraw

    ;; ================================================================
    ;; Get Key with Wait
    ;; Waits for key to be released before getting next keypress
    ;; ================================================================
get_key_wait:
    call wait_key

    ;; ================================================================
    ;; Get Key
    ;; Reads keyboard using beeper feedback
    ;; Returns: A = key code, Z flag set if ENTER (0x0D)
    ;; ================================================================
get_key:
    call CALL_BEEPKEY
    cp 00dh
    ret    

save_filename_backspace:
    ld hl,(MF3_PTR_5FF4)
    dec hl    
    push hl    
    ld de,MF3_WORK_BUFFER
    and a    
    sbc hl,de
    pop hl    
    jr c,save_filename_keyloop
    ld a,020h
    ld (hl),a    
    ld (MF3_PTR_5FF4),hl
    ld a,(MF3_BANK_CONTROL)
    dec a    
    ld (MF3_BANK_CONTROL),a
    jr save_filename_redraw

;; ----------------------------------------------------------------
;; Calculate Filename Space - Returns remaining space in filename buffer
;; Output: HL = space remaining
;; ----------------------------------------------------------------
calc_filename_space:
    ld de,(MF3_PTR_5FF4)
    ld hl,05fe8h
    and a
    sbc hl,de
    ret

save_filename_abort:
    ld a,(MF3_BANK_FLAGS)
    bit MF3_BANK_FLAGS_FILENAME_LOAD_MODE,a
    jp nz,check_space_key
    jp bank_switch_routine

;; ================================================================
;; Initialize Filename Buffer
;; Clears filename buffer to spaces and resets pointers
;; Sets up 10-byte buffer at 0x5FE2 for filename entry
;; ----------------------------------------------------------------
;; Uses:   HL, B, A
;; ================================================================
init_filename_buffer:
    ld hl,MF3_WORK_BUFFER
    ld b,00ah
init_fn_clear_loop:    ; loop target for clearing filename buffer (space fill)
    ld (hl),020h
    inc hl    
    djnz init_fn_clear_loop
    ld hl,MF3_BANK_CONTROL
    ld a,016h
    ld (hl),a    
    ld hl,MF3_WORK_BUFFER
    ld (MF3_PTR_5FF4),hl
    ret    
save_fn_buf_full:    ; buffer full, prompt for confirmation
    call print_confirm_prompt
save_fn_confirm_loop:    ; confirmation keyloop (y/n/retry/break)
    call get_key_wait
    jr z,save_fn_enter_confirmed
    cp 04eh
    jp z,save_filename_entry
    cp 059h
    jr z,save_fn_enter_confirmed
    call brkscan
    jr nc,save_filename_abort
    jr save_fn_confirm_loop
save_fn_enter_confirmed:    ; ENTER pressed - confirm filename
    call cls_print_msg
    ld a,(MF3_BANK_CONTROL)
    cp 016h
    jr nz,save_fn_dispatch
    call set_default_filename
    ld a,01ah
    ld (MF3_BANK_CONTROL),a
    call print_filename
    jr save_fn_buf_full
save_fn_dispatch:    ; check if custom or default filename, dispatch to load/save
    ld a,(MF3_BANK_CONTROL)
    ld c,016h
    sub c
    cp 008h
    jr z,save_fn_buf_full
    ld a,(MF3_BANK_FLAGS)
    bit MF3_BANK_FLAGS_FILENAME_LOAD_MODE,a
    jp nz,disk_load_entry
    jp save_options_entry

;; ================================================================
;; Set Default Filename
;; Copies default filename to buffer at 0x5FE2
;; Default is "snapshot" (8 chars from l1ba0h)
;; ----------------------------------------------------------------
;; Uses:   HL, DE, BC
;; ================================================================
set_default_filename:
    ld de,MF3_WORK_BUFFER
    ld hl,msg_default_filename
    ld bc,0008h
    ldir

;; ----------------------------------------------------------------
;; Print Filename - Displays current filename from buffer
;; ----------------------------------------------------------------
print_filename:
    ld hl,msg_clean_attrs
    ld b,007h
    call printf
    ld hl,MF3_WORK_BUFFER
    ld b,007h
print_fn_char_loop:    ; print filename character loop
    ld a,(hl)    
    call filter_printable_char
    rst 30h    
    inc hl    
    djnz print_fn_char_loop
    ret

;; ----------------------------------------------------------------
;; Print Filename at Cursor - Shows cursor position in filename entry
;; ----------------------------------------------------------------
print_filename_at_cursor:
    ld a,016h
    rst 30h    
    xor a    
    rst 30h    
    ld a,(MF3_BANK_CONTROL)
    rst 30h    
    ld hl,000cah
    ld b,005h
    jr printf_entry

;; ----------------------------------------------------------------
;; Print Filename Prompt - Displays filename entry prompt
;; ----------------------------------------------------------------
print_filename_prompt:
    call setup_display
    ld hl, msg_filename
    ld b,016h
printf_entry:    ; print string via printf (shared entry)
    jp printf

;; ----------------------------------------------------------------
;; Print Confirm Prompt - Displays Y/N confirmation prompt
;; ----------------------------------------------------------------
print_confirm_prompt:
    call print_filename
    call print_mf_attributes
    ld hl,00a5fh
    ld b,00dh
    call printf
print_yn_prompt:    ; print y/n confirmation prompt
    ld hl, msg_y_n
    ld b,006h
    jr printf_entry

msg_filename:    
    defb "File name (7 chars.):", TAB, TAB
    AT 1,22
    FLASH 1
    defb "OK ?"
    FLASH 0
msg_y_n:    
    AT 1,28
    defb "y/n"

save_start_tape:
    call print_tape_prompt
@tape_keyloop:
    call get_key_wait
    jr z,save_proceed_tape
    cp 041h
    jp z,reenter_save_options
    jr @tape_keyloop
save_start_disk:
    call restore_menu_screen
    jr save_proceed_common
save_proceed_tape:
    call restore_menu_screen
    ld hl,(0202bh)
    inc hl    
    ld (hl),020h
    ld hl,MF3_BANK_FLAGS
    set MF3_BANK_FLAGS_TAPE_MODE,(hl)
save_proceed_common:
    ld hl,MF3_BANK_FLAGS
    bit MF3_BANK_FLAGS_SAVE_SCREEN_MODE,(hl)

    jp nz,save_screen_main

    ld de,0602ah
    ld hl,03fe4h
    ld bc,0001ch
    ldir

    call scan_128k_banks
    ld hl,MF3_BANK_FLAGS
    res MF3_BANK_FLAGS_HAS_128K_BANKS,(hl)
    ld a,(MF3_BANK_BUFFER)
    and 00fh
    jp nz,save_set_128k_flag
    ld a,(MF3_BANK_BUFFER2)
    bit MF3_BANK7_PRESENT_BIT,a
    jr z,save_no_bank7
save_set_128k_flag:    ; set 128K banks present flag
    set MF3_BANK_FLAGS_HAS_128K_BANKS,(hl)
save_no_bank7:    ; no bank 7 present, continue
    call measure_compressed_sizes
    call make_room_233bytes

    ;; ================================================================
    ;; Save/Load System - Main Save Routine
    ;; Universal backup system for programs
    ;; Saves to tape or disk depending on mode
    ;; Includes: RAM, screen, BASIC vars, optional 128K banks
    ;; ================================================================

    ld de,(MF3_DISPLAY_ADDR)
    ld hl,mf3_loader

    call store_loader_address

    ;; Save BASIC system variables
    ld de,05d4fh
    ld hl,03fd4h
    ld bc,0010h
    ldir

    ;; Check if disk or tape operation
    call check_tape_or_disk
    jp nz,save_basic_block_start
    ld de,rom_entry_restore_disc
    ld hl,00fbbh
    call store_loader_address
    ld hl,(0202bh)
    ld (hl),043h
    ld de,05d34h
    call copy_filename_9bytes

    ;; Build save file structure
    ld hl,rom_start
    ld bc,rom_start
    ld de,(MF3_DISPLAY_ADDR)  ; Main RAM address
    call write_file_header  ; Write block header
    ld de,(MF3_SCREEN_SIZE)
    call write_file_header
    ld de,005b3h
    call write_file_header
    inc hl    
    inc hl    
    ld de,(MF3_BANK1_SIZE)
    call write_file_header
    ld de,(MF3_BANK3_SIZE)
    call write_file_header
    ld de,(MF3_BANK4_SIZE)
    call write_file_header
    ld de,(MF3_BANK6_SIZE)
    call write_file_header
    ld de,(06022h)
    call write_file_header
    ld d,b    
    ld e,c    
    ld bc,rom_start
    call write_file_header
    ld a,c    
    or b    
    jr z,save_check_extra_hdr
    inc hl    
save_check_extra_hdr:    ; check if additional data needed for tape header
    call perform_file_write
    jp nc,error_handler
    ;; ================================================================
    ;; Save Program Header and Main Blocks
    ;; Writes BASIC program area and main memory blocks
    ;; ================================================================
save_basic_block_start:    ; start saving BASIC program block
    ld hl,(0202bh)
    ld (hl),020h
    ld ix,(PROG)
    ld de,000e9h
    ld bc,rom_start+1
    call write_with_verify  ; Write BASIC program
    di
    jp nc,error_handler  ; Error handler
    call delay_if_disk

    ld hl,(0202bh)
    ld (hl),043h
    ld ix,RAMAREA
    ld de,(MF3_DISPLAY_ADDR)

    call check_tape_or_disk  ; Check disk/tape mode
    jr nz,tape_mode

    ;; Disk mode - use +3DOS file operations
    call save_dos_workspace

    ld hl,MF3_WORK_BUFFER
    ld b,003h
    ld c,003h
    ld d,002h
    ld e,004h

    call file_open_read
    jp nc,error_handler
    ld hl,RAMAREA
    ld de,(MF3_DISPLAY_ADDR)
    ld b,003h
    ld c,000h
    call file_write
    jp nc,error_handler
    jr save_screen_basic

    ;; Tape mode - use ROM tape routines
tape_mode:
    call write_to_tape
    call delay_if_disk

    ;; ================================================================
    ;; Save Screen and BASIC Variables
    ;; Writes screen memory and BASIC variable area
    ;; ================================================================
save_screen_basic:
    ld ix,SCREEN
    ld de,(MF3_SCREEN_SIZE)  ; Screen size
    ld c,000h
    call write_to_tape
    jr nc,save_error_check
    call delay_if_disk
    call page_mf3_out  ; Swap buffers
    call delay_if_disk
    ld ix,060b3h
    ld de,005b3h  ; BASIC variables size
    ld c,000h
    call write_to_tape
    push af
    call page_mf3_out
    pop af
    jr nc,save_error_check
    call delay_if_disk
    ;; ================================================================
    ;; Save Additional 128K Banks (Optional)
    ;; Saves banks 1, 3, 4, 6, 7 if 128K mode is active
    ;; Checks flags to determine which banks to save
    ;; ================================================================
    ld a,(MF3_BANK_FLAGS)
    bit MF3_BANK_FLAGS_HAS_128K_BANKS,a
    jp z,save_final_close_check
    ld a,(MF3_BANK_BUFFER)
    bit MF3_BANK1_PRESENT_BIT,a
    jr z,save_check_bank3
    call check_tape_or_disk
    jr z,save_bank1_disk

    ;; Save Bank 1 (tape mode)
    ld a,011h
    ld de,(MF3_BANK1_SIZE)  ; Bank 1 size
    call save_memory_bank
    call delay_if_disk
    jr save_check_bank3

    ;; Save Bank 1 (disk mode)
save_bank1_disk:    ; save bank 1 via disk (swap + file write)
    call swap_bank_to_8000h
    call save_dos_workspace
    ld a,007h
    ld c,a
    ld hl,08000h
    ld de,(MF3_BANK1_SIZE)
    ld b,003h
    call file_write
    push af
    call swap_bank_to_8000h
    pop af
    jr nc,save_error_check

    ;; Save Bank 3
save_check_bank3:    ; check/save bank 3
    ld a,(MF3_BANK_BUFFER)
    bit MF3_BANK3_PRESENT_BIT,a
    jr z,save_check_bank4
    ld a,013h
    ld de,(MF3_BANK3_SIZE)  ; Bank 3 size
    call save_memory_bank
save_error_check:
    jp nc,error_handler  ; Error handler
    call delay_if_disk

    ;; Save Bank 4
save_check_bank4:    ; check/save bank 4
    ld a,(MF3_BANK_BUFFER)
    bit MF3_BANK4_PRESENT_BIT,a
    jr z,save_check_bank6
    ld a,014h
    ld de,(MF3_BANK4_SIZE)  ; Bank 4 size
    call save_memory_bank
    jr nc,save_error_check
    call delay_if_disk

    ;; Save Bank 6
save_check_bank6:    ; check/save bank 6
    ld a,(MF3_BANK_BUFFER)
    bit MF3_BANK6_PRESENT_BIT,a
    jr z,save_check_bank7
    ld a,016h
    ld de,(MF3_BANK6_SIZE)  ; Bank 6 size
    call save_memory_bank
    jr nc,save_error_check
    call delay_if_disk

    ;; Save Bank 7
save_check_bank7:    ; check/save bank 7
    ld a,(MF3_BANK_BUFFER2)
    bit MF3_BANK7_PRESENT_BIT,a
    jr z,save_banks_all_done
    call check_tape_or_disk
    jr z,save_bank7_disk
    ld a,017h
    ld de,(06022h)
    call save_memory_bank
    call delay_if_disk
    jr save_banks_all_done
save_bank7_disk:    ; save bank 7 via disk mode
    call prepare_bank7_save
    call save_dos_workspace

    ld a,007h
    ld c,a    

    ld hl,08000h
    ld de,(06022h)
    ld b,003h
    call file_write
    
    jr nc,save_bank7_error
    ld b,003h

    call dos_close_file
    call swap_plus3dos
    call swap_bank7_to_8000h
    
    jr save_swap_mf3_page
save_banks_all_done:    ; all banks done, restore bank 0
    call set_bank0_rom_x1
save_final_close_check:    ; check tape/disk for final close
    call check_tape_or_disk
    jr nz,save_menu_and_finish
    call save_dos_workspace
    ld b,003h
    call dos_close_file
    di    
    call swap_plus3dos
save_swap_mf3_page:    ; swap mf3 page after disk save
    call swap_mf3_page1_4k
save_menu_and_finish:    ; save menu screen and finish
    call save_menu_screen
    call reset_compression_state
    jp bank_switch_routine
save_bank7_error:    ; error preparing bank 7
    call prepare_bank7_save
    jp error_handler

;; ================================================================
;; Prepare Bank 7 Save
;; Swaps +3DOS workspace and bank 7 to prepare for saving
;; Performs three swaps to align memory correctly
;; ----------------------------------------------------------------
;; Uses:   All registers (via swap routines)
;; ================================================================
prepare_bank7_save:
    call swap_plus3dos
    call swap_bank7_to_8000h
    jp swap_plus3dos


;; ----------------------------------------------------------------
;; Check Tape or Disk - Tests operation mode
;; Output: Z flag set = disk mode, clear = tape mode
;; ----------------------------------------------------------------
check_tape_or_disk:
    ld a,(MF3_BANK_FLAGS)
    bit MF3_BANK_FLAGS_TAPE_MODE,a
    ret

;; ================================================================
;; Write File Header
;; Builds file header data structure for save operations
;; Calculates offsets and sizes for file blocks
;; ----------------------------------------------------------------
;; Input:  HL = current offset, BC = accumulator, DE = block size
;; Output: HL = new offset, BC = updated accumulator
;; Uses:   HL, DE, BC
;; ================================================================
write_file_header:
    push de
    srl d
    ld e,d
    ld d,000h
    add hl,de
    pop de
    push hl
    ld h,000h
    ld l,e
    add hl,bc
    ld b,h
    ld c,l
    pop hl
    ret

;; ================================================================
;; Perform File Write
;; Checks disk space and prepares DOS environment for writing
;; Verifies sufficient free space before proceeding
;; ----------------------------------------------------------------
;; Input:  HL = required space
;; Output: Carry = success/failure
;; Uses:   HL, DE, IX, A, BC
;; ================================================================
perform_file_write:
    push hl    
    call prepare_dos_environment

    ld ix, DOS_FREE_SPACE
    ld a, 'A'  ; drive A? hardcoded :/
    call CALL_ROM2
    
    pop de    
    ret nc    

    ld (02022h),hl
    sla l
    rl h
    and a    
    sbc hl,de
    ccf    
    ld (02024h),de
    ld a,022h

    ret

;; ================================================================
;; Save Memory Bank
;; Saves a 128K memory bank to tape or disk
;; Switches to specified bank and writes its contents
;; ----------------------------------------------------------------
;; Input:  A = bank number, DE = size to save
;; Uses:   IX, HL, DE, BC, A
;; ================================================================
save_memory_bank:
    ld ix,0c000h
    push af    
    call check_tape_or_disk
    
    jr z,save_membank_disk
    pop af    
    call switch_bank
    jr tape_save_entry
save_membank_disk:    ; disk path for save_memory_bank
    call save_dos_workspace
    pop af    
    and 007h
    ld c,a
    
disk_write_entry:    ; shared disk write setup (HL from IX, B=3)
    push ix
    pop hl    
    ld b,003h

    ;; file write

;; ----------------------------------------------------------------
;; File Write - Writes data to disk using +3DOS
;; Input: HL = data address, DE = size, B = file handle, C = bank
;; ----------------------------------------------------------------
file_write:
    ld ix,DOS_WRITE
    jp CALL_ROM2

;; ----------------------------------------------------------------
;; Write to Tape - Writes data to tape or disk
;; Checks mode and branches to appropriate routine
;; ----------------------------------------------------------------
write_to_tape:
    call check_tape_or_disk
    jr z,disk_write_entry
    jr tape_save_entry

;; ================================================================
;; Write with Verify
;; Writes data block with verification
;; Handles both tape and disk operations
;; ----------------------------------------------------------------
;; Input:  IX = data address, DE = length, BC = type
;; Output: Carry = success/failure
;; Uses:   All registers
;; ================================================================
write_with_verify:
    call check_tape_or_disk
    jr z,disk_save_entry
    xor a    
    push de    
    pop hl
    
tape_save_block:
    ld (05fe1h),a
    ld (MF3_SAVED_DE),de
    ld (MF3_SAVED_BC),bc
    ld (MF3_SAVED_HL),hl
    push de    
    push ix
    ld ix,05fe1h
    ld de,0011h
    xor a    
    call call_rom_sabytes
    call delay_4x65535
    pop ix
    pop de    
tape_save_entry:    ; tape save entry, sets A=0xFF
    ld a,0ffh
    
call_rom_sabytes:
    ld hl,SABYTES
    call CALL_48ROM
    scf    
    ret
    
disk_save_entry:
    ;; Disk save entry: setup DOS params and open file for writing
    xor a    

    call setup_dos_params
    call copy_filename_to_dos
    call save_dos_workspace
    
    ld hl,MF3_DOS_PTR
    ld b,003h
    ld c,003h
    ld d,001h
    ld e,004h
    call file_open_read
    ret nc    
    ld b,003h
    call refresh_file_header
    ret nc    
    push ix
    pop de    
    ld hl,MF3_BUFFER_6011
    ld bc,0007h
    ldir
    ld b,003h
    ld c,007h
    ld hl,(PROG)
    ld de,(06012h)
    call file_write
    ret nc    
    ld b,003h
dos_close_file:
    ld ix,DOS_CLOSE
    jp CALL_ROM2
setup_dos_params:
    ld (MF3_BUFFER_6011),a
    ld (06012h),de
    ld (06014h),bc
    ld (06016h),de
    ret    
copy_filename_to_dos:
    ld de,MF3_DOS_PTR
copy_filename_9bytes:
    ld hl,MF3_WORK_BUFFER
    ld bc,0009h
    ldir
    ret
    
store_loader_address:
    push de    
    push hl    
    ld hl,04d92h
    pop de    
    add hl,de    
    pop de    
    ld (hl),e    
    inc hl    
    ld (hl),d    
    ret
    
delay_if_disk:
    call check_tape_or_disk
    ret z    
delay_4x65535:
    ld b,004h
delay_loop:
    ld de,0ffffh
delay_inner_loop:    ; inner delay loop countdown
    dec de    
    ld a,d    
    or e    
    jr nz,delay_inner_loop
    djnz delay_loop
    ret    
;; ================================================================
;; Restore Snapshot System (0x0D08-0x0F2C)
;; Restores saved snapshots from tape or disk
;; Loads: BASIC program, screen memory, system variables, 128K banks
;; Tape path: restores bank 1 directly, banks 3/4/6 via rotation loop
;; Disk path: uses swap+read for bank 1, direct read for banks 3/4/6
;; Bank 7 requires special +3DOS workspace swapping on both paths
;; ================================================================

restore_from_tape:
    call copy_interop
    call set_bank0_rom3

    ld hl,MF3_BANK_FLAGS
    set MF3_BANK_FLAGS_TAPE_MODE,(hl)
    jr restore_load_main
restore_from_disc:
    call copy_interop
    call delay_4x65535

    ld hl,MF3_BANK_FLAGS
    res MF3_BANK_FLAGS_TAPE_MODE,(hl)

    call save_dos_workspace
    call init_dos_workspace
    
    ld bc,00069h
    ld hl,(PROG)
    add hl,bc    

    ld b,003h
    ld c,001h
    ld d,000h
    ld e,002h
    call file_open_read

    jp nc,error_handler
restore_load_main:    ; common restore entry: load main blocks (BASIC, screen, vars)
    ld ix,RAMAREA
    ld de,rom_entry_restore_disc+1
    ld hl,(PROG)
    add hl,de    
    push bc    
    push hl    
    ld de,0012h
    add hl,de    
    ld de,03fd4h
    ld bc,0010h
    ldir
    pop hl    
    pop bc    
    ld e,(hl)    
    inc hl    
    ld d,(hl)    
    ld c,000h
    call load_block_tape_or_disk
    ld ix,SCREEN
    ld de,(MF3_SCREEN_SIZE)
    ld c,000h

    call load_block_tape_or_disk
    call decompress_sysvars
    call decompress_screen
    ld hl,SCREEN
    
    push hl    
    ld de,0202dh
    ld bc,005b3h
    push bc    
    ldir
    pop de    
    pop ix
    ld c,000h
    call load_block_tape_or_disk
    ld a,(MF3_BANK_FLAGS)
    bit MF3_BANK_FLAGS_HAS_128K_BANKS,a
    jr z,restore_skip_128k
    call set_rom0
    ld hl,rom_start+1
    call CALL_READROM
    ld a,c    
    cp 0afh
restore_skip_128k:    ; skip 128K bank restore if 48K snapshot
    jp z,nmi_restore_workspace
    ld a,(MF3_BANK_BUFFER)
    bit MF3_BANK1_PRESENT_BIT,a
    jr z,restore_banks_364_loop
    call check_tape_or_disk
    jr nz,restore_bank1_tape
    call swap_bank_to_8000h
    ld ix,08000h
    ld de,(MF3_BANK1_SIZE)
    ld c,001h
    call load_block_tape_or_disk
    call swap_bank_to_8000h
    jr restore_banks_364_loop
restore_bank1_tape:    ; restore bank 1 via tape
    call select_rom1_printer_bank2
    ld ix,0c000h
    ld de,(MF3_BANK1_SIZE)
    ld c,001h
    call load_block_tape_or_disk
restore_banks_364_loop:    ; bank 3/4/6 restore via bit-rotation loop
    ;; Uses RRC to cycle through bank-present bits in E (banks 3,4,6)
    ;; and D (bank 7). B counts down from 3, selecting banks via 0x0C2 table:
    ;;   B=3 -> offset 5 -> bank 3; B=2 -> offset 6 -> bank 4; B=1 -> offset 7 -> bank 6
    ld b,003h
    ld a,(MF3_BANK_BUFFER)
    ld e,a    
    rrc e
    ld a,(MF3_BANK_BUFFER2)
    ld d,a    
restore_bank_iter:    ; bank iteration loop target (banks 3, 4, 6)
    push bc    
    rrc e
    jr nc,restore_bank_skip
    push de    
    ld a,008h
    sub b    
    ld hl,000c2h
    ld e,a    
    ld d,000h
    add hl,de    
    ld a,(BANKM)
    and 0f0h
    ld c,(hl)    
    or c    
    ld (BANKM),a
    call switch_bank

    and 007h
    ld c,a    
    ld ix,0c000h
    ld a,e    
    sub 004h
    add a,a    
    ld e,a    
    ld hl,MF3_BANK1_SIZE
    add hl,de    
    ld e,(hl)    
    inc hl    
    ld d,(hl)    
    call load_block_tape_or_disk
    pop de    
restore_bank_skip:    ; skip current bank in rotation loop
    pop bc    
    djnz restore_bank_iter
    bit 3,d
    jr z,restore_post_bank7
    call check_tape_or_disk
    jr nz,restore_bank7_tape
    call prepare_bank7_save
    ld ix,08000h
    ld de,(06022h)
    ld c,007h
    call load_block_tape_or_disk
    call swap_plus3dos
    
    ;; swap the bank in A into 8000h
    call swap_bank7_to_8000h
    jr restore_post_bank7
restore_bank7_tape:    ; restore bank 7 via tape mode
    ld ix,0c000h
    ld de,(06022h)
    ld c,007h
    call load_block_tape_or_disk
restore_post_bank7:    ; after bank 7 restore, check tape/disk for mf3 swap
    call check_tape_or_disk
    jr nz,restore_decompress_exit
    call swap_mf3_page1_4k
restore_decompress_exit:    ; restore bank 0, decompress all banks, finish
    ld a,(BANKM)
    and 0f0h
    call switch_bank
    ld (BANKM),a
    call decompress_all_banks
    jp nmi_restore_workspace

;; ================================================================
;; Load Block - Tape or Disk
;; Dispatches data loading based on current mode (tape or disk)
;; Tape: pages in target bank, uses ROM LDBYTES routine
;; Disk: pages in DOS workspace, uses +3DOS file read
;; ----------------------------------------------------------------
;; Input:  IX = data address, DE = size, C = bank number (0=screen)
;; Uses:   All registers (via called routines)
;; ================================================================
load_block_tape_or_disk:
    call check_tape_or_disk
    jr z,disk_load_block
    ld a,c    
    or ROM_SEL0
    call switch_bank_restore_ay
    
    ld a,0ffh
    scf    

    ld hl,LDBYTES
    jp CALL_48ROM
    
disk_load_block:    ; disk load block entry in load_block_tape_or_disk
    push ix
    pop hl    
    push bc    
    call save_dos_workspace
    
    pop bc    
    ld b,003h

    jp file_read
snapshot_loader_basic_rem:
    defb 0x00, 0x01  ; BASIC Line 1 (Big Endian)
    defw 0x0091  ; Line length 145 bytes
    defb 0xEA  ; REM token
    
loader_machine_code_start:
    di  ; Disable interrupts
    ld bc, 0082h  ; Offset to Tape/Disc flag (006Eh / 0071h)
    ld hl,(PROG)
    add hl,bc    
    ld c,(hl)    
    inc hl    
    ld b,(hl)    
    ld a,0c3h
    ld (05c7fh),a
    ld (05c80h),bc
    ld bc,BANK1
    ld a,010h
    out (c),a
    ld a,(BANK678)
    or 004h
    ld b,01fh
    out (c),a
    ld a,0feh
    call CHANOPEN
    
loader_check_mf3_present:
    in a,(P_MF3_IN)  ; Page in Multiface 3
    ld a,(rom_start)  ; Read first byte of ROM
    cp 0c3h  ; Check if it's C3 (JP instruction)
    jr z,loader_mf3_found  ; If found, proceed to load
    
    ;; MF3 not found: Print warning string
    ld bc,0074h
    ld hl,(PROG)
    add hl,bc  ; Point to warning string
    ld b,00eh  ; 14 characters
loader_warn_print_loop:
    ld a,(hl)    
    rst 10h  ; Print character
    inc hl    
    djnz loader_warn_print_loop
    jr loader_check_mf3_present  ; Infinite loop until MF3 is inserted

loader_mf3_found:
    in a,(P_MF3_OUT)  ; Page MF3 out
    ld hl,(PROG)
    push hl    
    ld bc,0074h
    add hl,bc  ; Point to first loading string part
    ld b,007h
loader_print_loading_msg1:
    ld a,(hl)    
    rst 10h    
    inc hl    
    djnz loader_print_loading_msg1
    pop hl    
    ld bc,000bch
    add hl,bc  ; Point to second loading string part
    ld b,007h
loader_print_loading_msg2:
    ld a,(hl)    
    rst 10h    
    inc hl    
    djnz loader_print_loading_msg2
    
    in a,(P_MF3_IN)  ; Page MF3 back in
    jp 05c7fh  ; Jump to loader continuation

    ;; Offset 0E9h: Filename of datafile (disc only), ASCII, terminated by 0FFh.
    ;; 8 spaces followed by 0FFh terminator. Unused for tape.
    defb "        ", 0xFF

mf3_loader:
    ;; Offset 0F2h: Length of first data block (tape headerless file).
    ;; Default length is 0A020h (40992 bytes, typical RAM snapshot size).
    defw 0xA020  ; Emits 0x20, 0xA0

    ;; Warning String (Located exactly at PROG+0x74)
    ;; Format: AT 20, 12; FLASH 1; PAPER 6; "NOT ON!"
    defb 0x16, 20, 12  ; AT 20, 12
    defb 0x12, 1  ; FLASH 1
    defb 0x11, YELLOW  ; PAPER YELLOW
    defb "NOT ON!"  ; Text
    
    ;; Offset 102h: A word which is 006Eh for tape or 0071h for disc.
    defw rom_entry_restore_tape
    
    ;; Offset 104h: The 16 AY-3-8912 sound chip register values (initialised to 0)
    defb 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    defb 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    defb 0x0D  ; Carriage Return (end of Line 1)
    
    defb 0x00  ; Padding/filler byte
    defb 0x00, 0x02  ; Line number 2 (Big Endian)
    defw 0x004F  ; Line length 79 bytes (Little Endian)

    ;; BORDER SGN PI: INK SGN PI: PAPER SGN PI
    defb 0xE7, 0xBC, 0xA7  ; BORDER SGN PI
    defb 0x3A, 0xD9, 0xBC, 0xA7  ; : INK SGN PI
    defb 0x3A, 0xDA, 0xBC, 0xA7  ; : PAPER SGN PI
    
    ;; : CLEAR VAL "24543"
    defb 0x3A, 0xFD, 0xB0  ; : CLEAR VAL
    defb "\"24543\""  ; "24543"
    
    ;; : PRINT "AT 20, 9; FLASH 1; PAPER 6; M3 LOADING"
    defb 0x3A, 0xF5, "\"", 0x16, 20, 9    ; : PRINT " AT 20, 9
    defb 0x12, 1, 0x11, YELLOW  ; FLASH 1, PAPER YELLOW
    defb "M3 LOADING"  ; Text
    defb "\""                ; "
    
    ;; : RANDOMIZE USR (VAL "256" * PEEK VAL "23636" + PEEK VAL "23635" + VAL "5")
    defb 0x3A, 0xF9, 0xC0  ; : RANDOMIZE USR
    defb 0x28, 0xB0, "\"256\""  ; ( VAL "256"
    defb 0x2A, 0xBE, 0xB0, "\"23636\""  ; * PEEK VAL "23636"
    defb 0x2B, 0xBE, 0xB0, "\"23635\""  ; + PEEK VAL "23635"
    defb 0x2B, 0xB0, "\"5\""  ; + VAL "5"
    defb 0x29  ; )
    
    defb 0x0D  ; Carriage Return (end of Line 2)
    nop    

prepare_dos_environment:
    call swap_plus3dos
    call swap_mf3_page1_4k

    call save_dos_workspace
    call init_dos_workspace

    ;; disable error messages 
    ld ix,DOS_SET_MESSAGE
    ;; set A=0
    xor a
    
    jp CALL_ROM2
    ;; implied ret
    
save_screen_main:
    call check_tape_or_disk
    jp nz,save_screen_tape
    ld hl,000eh
    call perform_file_write
    jr nc,@error
    ld de,tool_hex_lower_nibble  ; DE=0x1B00 = screen size (6912 bytes)
    ld bc,SCREEN
    ld a,003h
    call setup_dos_params
    ld hl,MF3_WORK_BUFFER
    ld b,003h
    ld c,003h
    ld d,001h
    ld e,004h
    call file_open_read
    jr nc,@error
    ld b,003h
    call refresh_file_header
    jr nc,@error
    push ix
    pop de    
    ld hl,MF3_BUFFER_6011
    ld bc,0007h
    ldir
    ld b,003h
    ld c,007h
    ld hl,SCREEN
    ld de,tool_hex_lower_nibble  ; DE=0x1B00 = screen size (6912 bytes)
    call file_write
@error:
    jp nc,error_handler
    ld b,003h
    call dos_close_file
    call scan_ay_registers
save_exit_to_menu:
    jp bank_switch_routine
save_screen_tape:
    ld a,003h
    ld bc,SCREEN
    push bc    
    pop ix
    ld de,tool_hex_lower_nibble  ; DE=0x1B00 = screen size (6912 bytes)
    ld hl,0ffffh
    call tape_save_block
    call save_menu_screen
    jr save_exit_to_menu

;; +3DOS workspace layout in bank 7 (initialized by init_dos_workspace)
DOS_WS_BASE:                   equ 0xdb00
DOS_WS_SIZE:                   equ 0x0938
DOS_WS_DEF_DRV:                equ 0xdf94
DOS_WS_UNIT0_DRV:              equ 0xe3ea
DOS_WS_BCBS_SEED_DST:          equ 0xdfe8
DOS_WS_XDPB_A_EXT_DST:         equ 0xe2db
DOS_WS_XDPB_M_DST:             equ 0xe39a
DOS_WS_SPEC_M_TAIL_DST:        equ 0xe3f1
DOS_WS_TM_MTRON_DST:           equ 0xe428
DOS_WS_BCB_FREE:               equ 0xe294
DOS_WS_XDPB_PTRS:              equ 0xe2a0
DOS_WS_XDPB_PTR_A:             equ DOS_WS_XDPB_PTRS+0x00
DOS_WS_XDPB_PTR_M:             equ DOS_WS_XDPB_PTRS+0x18
DOS_WS_BCB8_BUF_HI_BANK:       equ 0xe041
DOS_WS_BCB8_BUF_HI_BANK_INIT:  equ 0x01ce
;; BCB pool: bcbs=0xDFE0, 9 slots (0..8), 0x0B bytes each.
;; Seed starts at slot 0 ix+8 (= DOS_WS_BCBS_SEED_DST); no separate header.
DOS_WS_BCB_REC_SIZE:   equ 0x0b
DOS_WS_BCB_REC_COUNT:  equ 0x09

init_dos_workspace:
    ;; Clear full +3DOS workspace area in bank 7.
    ld hl,DOS_WS_BASE
    ld bc,DOS_WS_SIZE
    call clear_ram

    ;; Seed +3DOS drive defaults: DEF_DRV='A' and UNIT0 mapping='A'.
    ld a,041h
    ld (DOS_WS_DEF_DRV),a
    ld (DOS_WS_UNIT0_DRV),a

    ;; Seed block 1: descriptor/header table used by +3DOS internals.
    ld hl,dos_ws_seed_block1
    ld de,DOS_WS_BCBS_SEED_DST
    ld bc,0052h
    ldir
    
    ;; Wire block/pointer graph consumed by +3DOS ROM routines.
    ;;
    ;; DOS_WS_BCB_FREE = bcb_free (ROM2 workspace: 0xE294).
    ;;   Seeded with address of BCB slot 8 = head of free chain (empty-buffer sentinel).
    ;;   ROM2 reads (0xE294 / DOS_WS_BCB_FREE) = 0xE038 as the current free-head BCB address.
    ld hl,0e038h
    ld (DOS_WS_BCB_FREE),hl
    ;;
    ;; DOS_WS_XDPB_PTR_A (0xE2A0) is xdpb_ptrs[A] in ROM2 workspace.
    ;;   It points at xdpb_a base (0xE2C0).
    ;;   xdpb_ptrs[B] at 0xE2A2 remains zero after clear until mapped.
    ld hl,0e2c0h
    ld (DOS_WS_XDPB_PTR_A),hl
    ;;
    ;; DOS_WS_XDPB_PTR_M (0xE2B8) is xdpb_ptrs[M].
    ;;   Point it at xdpb_m base, seeded at DOS_WS_XDPB_M_DST (0xE39A).
    ld hl,DOS_WS_XDPB_M_DST
    ld (DOS_WS_XDPB_PTR_M),hl

    ;; Seed block 2: xdpb_a +0x1B extended drive record (A:).
    ld hl,dos_ws_seed_block2
    ld de,DOS_WS_XDPB_A_EXT_DST
    ld bc,0015h
    ldir

    ;; Seed block 3: xdpb_m base record (M: RAMdisk XDPB).
    ld hl,dos_ws_seed_block3
    ld de,DOS_WS_XDPB_M_DST
    ld bc,0011h
    ldir

    ;; Seed block 4: small geometry/stride tuple used by DOS logic.
    ld hl,dos_ws_seed_block4
    ld de,DOS_WS_SPEC_M_TAIL_DST
    ld bc,0004h+1
    ldir

    ;; Seed block 5: additional DOS parameter tuple.
    ld hl,dos_ws_seed_block5
    ld de,DOS_WS_TM_MTRON_DST
    ld bc,000ah
    ldir

    ;; Complete BCB slot 8 buffer tuple:
    ;; [0xE041]=0xCE (buffer address high), [0xE042]=0x01 (buffer bank).
    ld hl,DOS_WS_BCB8_BUF_HI_BANK_INIT
    ld (DOS_WS_BCB8_BUF_HI_BANK),hl
    ret    

    ;; Seed data copied into the +3DOS workspace during init_dos_workspace.
    ;; These are data tables, not executable code.
dos_ws_seed_block1:
    ;; 0x52 bytes at DOS_WS_BCBS_SEED_DST (0xDFE8).

    ;; slot 0 
    ;; implied bytes before the seed start:
    ;;   defw 0x0000 at [0xDFE0] slot 0 link field
    ;;   defb 0x00        ; flags
    ;;   defw 0x0000    ; FCB pointer
    ;;   defb 0x00        ; drive
    ;;   defw 0x0000    ; absolute sector
    defw 0xE090  ; buffer address
    defb 0x07  ; buffer bank (slot 0)

    ;; slot 1
    defw 0xDFE0  ; at [0xDFEB] slot 1 link field
    defb 0x00  ; flags
    defw 0x0000  ; FCB pointer
    defb 0x00  ; drive
    defw 0x0000  ; absolute sector
    defw 0xC000  ; buffer address
    defb 0x01  ; buffer bank

    ;; slot 2
    defw 0xDFEB  ; at [0xDFF6] slot 2 link field
    defb 0x00  ; flags
    defw 0x0000  ; FCB pointer
    defb 0x00  ; drive
    defw 0x0000  ; absolute sector
    defw 0xC200  ; buffer address
    defb 0x01  ; buffer bank

    ;; slot 3 link (ix+0, ix+1)
    defw 0xDFF6  ; at [0xE001] slot 3 link field
    defb 0x00  ; flags
    defw 0x0000  ; FCB pointer
    defb 0x00  ; drive
    defw 0x0000  ; absolute sector
    defw 0xC400  ; buffer address
    defb 0x01  ; buffer bank

    ;; slot 4 link (ix+0, ix+1)
    defw 0xE001  ; at [0xE00C] slot 4 link field
    defb 0x00  ; flags
    defw 0x0000  ; FCB pointer
    defb 0x00  ; drive
    defw 0x0000  ; absolute sector
    defw 0xC600  ; buffer address
    defb 0x01  ; buffer bank

    ;; slot 5 link (ix+0, ix+1)
    defw 0xE00C  ; at [0xE017] slot 5 link field
    defb 0x00  ; flags
    defw 0x0000  ; FCB pointer
    defb 0x00  ; drive
    defw 0x0000  ; absolute sector
    defw 0xC800  ; buffer address
    defb 0x01  ; buffer bank

    ;; slot 6 link (ix+0, ix+1)
    defw 0xE017  ; at [0xE022] slot 6 link field
    defb 0x00  ; flags
    defw 0x0000  ; FCB pointer
    defb 0x00  ; drive
    defw 0x0000  ; absolute sector
    defw 0xCA00  ; buffer address
    defb 0x01  ; buffer bank

    ;; slot 7 link (ix+0, ix+1)
    defw 0xE022  ; at [0xE02D] slot 7 link field
    defb 0x00  ; flags
    defw 0x0000  ; FCB pointer
    defb 0x00  ; drive
    defw 0x0000  ; absolute sector
    defw 0xCC00  ; buffer address
    defb 0x01  ; buffer bank

    ;; slot 8 link (ix+0, ix+1)
    defw 0xE02D  ; at [0xE038] slot 8 link field
    ;; implied trailing bytes due to cleared mem region.
    ;;   defb 0x00        ; flags
    ;;   defw 0x0000    ; FCB pointer
    ;;   defb 0x00        ; drive
    ;;   defw 0x0000    ; absolute sector
    ;;   defw 0x0000    ; buffer address
    ;;   defb 0x00        ; buffer bank

dos_ws_seed_block2:
    ;; 0x15 bytes at DOS_WS_XDPB_A_EXT_DST (xdpb_a +0x1B).
    ;; Extended XDPB info for drive A: (flags, drive/unit, access state, pointers, hooks).
    defb 0x04  ; flags
    defb 0x41  ; drive ('A')
    defb 0x00  ; unit
    defb 0x00, 0x00, 0x00  ; last-access FRAMES (24-bit)
    defb 0x00  ; files open
    defw 0x0000  ; free directory entries
    defw 0x0000  ; last used directory entry
    defw 0xE2F0  ; checksum vector pointer (chksm_a)
    defw 0xE300  ; allocation bitmap pointer (alloc_a)
    defw 0x1988  ; low-level login routine
    defw 0x197C  ; low-level read-sector routine
    defw 0x1982  ; low-level write-sector routine

dos_ws_seed_block3:
    ;; 0x11 bytes at DOS_WS_XDPB_M_DST.
    ;; M: RAMdisk XDPB base fields (DPB core at offsets +0x00..+0x10).
    defw 0x0004  ; SPT: records per track
    defb 0x03  ; BSH: block shift factor
    defb 0x07  ; BLM: block mask
    defb 0x00  ; EXM: extent mask
    defw 0x003B  ; DSM: last block number
    defw 0x003F  ; DRM: last directory entry
    defw 0x00C0  ; AL0/AL1: directory bitmap
    defb 0x00  ; CKS: checksum vector size (low/placeholder byte)
    defw 0x0880  ; OFF: reserved tracks
    defb 0x00  ; CKS/DPB trailing byte
    defb 0x02  ; PSH: physical sector shift
    defb 0x03  ; PHM: physical sector mask

dos_ws_seed_block4:
    ;; 0x05 bytes at DOS_WS_SPEC_M_TAIL_DST (0xE3F1 = spec_m+2).
    ;; This is the tail of RAMdisk disk-spec (spec_m), with spec_m+0/+1 implied zero.
    defb 0x80  ; spec_m+2: last track/buffer + 1
    defb 0x01  ; spec_m+3: sectors per track
    defb 0x02  ; spec_m+4: sector-size shift (512-byte sectors)
    defb 0x08  ; spec_m+5: first RAMdisk track/buffer
    defb 0x03  ; spec_m+6: block-size shift

dos_ws_seed_block5:
    ;; 0x0A bytes at DOS_WS_TM_MTRON_DST (0xE428 = tm_mtron).
    ;; Seeds DD setup/runtime state spanning tm_* timings, retry count, and fdc_res head.
    defb 0x0A  ; tm_mtron: motor-on timeout
    defb 0x32  ; tm_mtroff: motor-off timeout
    defb 0xAF  ; tm_wrtoff: write-off timeout
    defb 0x1E  ; tm_hdsetl: head-settle time
    defb 0x0C  ; tm_step: step rate
    defb 0x0F  ; retry_cnt: retry count
    defb 0x00  ; reserved/cleared runtime byte
    defb 0x00  ; reserved/cleared runtime byte
    defb 0x01  ; fdc_res+0: initial status byte
    defb 0x01  ; fdc_res+1: initial status byte


reenter_save_options:
    ld hl,save_options_entry
    jr enter_basic_dispatch
restore_exit_to_menu:
    ld sp,(05c3dh)
    ld de,06056h
    pop hl    
    push de    
    ld a,(iy+000h)
    cp 0ffh
    jp nz,error_handler
bank_switch_routine:
    call set_bank0_rom3
    ld hl,main_menu_entry
enter_basic_dispatch:
    ld (MF3_BANK_ADDR),hl
    ld hl,KSTATE
    ld bc,find_marker_start
    ld d,h    
    ld e,l    
    inc de    
    ld (hl),000h
    ldir
    inc hl    
    ld sp,hl    
    ld hl,03e00h
    push hl    
    ld hl,06056h
    push hl    
    ld (05c3dh),sp
    call init_basic_sysvars
    ld hl,(MF3_BANK_ADDR)
    jp (hl)    
main_menu_entry:
    call decompress_screen_if_needed
main_menu_redraw:
    call display_main_menu
    ld hl,MF3_BANK_FLAGS
    res MF3_BANK_FLAGS_FILENAME_LOAD_MODE,(hl)
    res MF3_BANK_FLAGS_TAPE_MODE,(hl)
    ld a,(MF3_ADDR_PTR)
    and MF3_ADDR_PTR_MAIN_MENU_CLEAR_MASK
    ld (MF3_ADDR_PTR),a

    ;; ================================================================
    ;; Main Menu Keyboard Handler
    ;; Processes user input from main Multiface menu
    ;; Commands: R)eturn S)ave T)oolkit P)rint O)ptions
    ;;           D)OS A)lternate screen C)lear banks
    ;; ================================================================
main_keyloop:
    call get_key_wait

    cp 'R'
    jp z,nmi_restore_menu_exit  ; Return to interrupted program

    cp 'S'
    jp z,save_filename_entry  ; Enter save filename flow

    cp 'T'
    jp z,toolkit_entry  ; Enter toolkit mode

    cp 0e2h
    jp z,memory_checksum_utility  ; Memory checksum

    cp 'P'
    jr z,main_cmd_print_screen  ; Print screen

    cp 'O'
    jr z,main_cmd_toggle_options  ; Toggle options display

    call check_48kmode
    jr z,main_keyloop  ; Skip DOS commands if locked

    cp 'D'
    jp z,check_space_key  ; DOS operations

    cp 'A'
    jr z,main_cmd_alternate_screen  ; Alternate screen

    cp 'C'
    jr z,main_cmd_clear_banks  ; Clear RAM banks

    jr main_keyloop
main_cmd_alternate_screen:
    call swap_screen_buffers
    jr main_menu_redraw

swap_screen_if_128k:
    call check_48kmode
    ret z    
swap_load_bank_flags:
    ;; Load bank flags for alternate screen swap check
    ld a,(MF3_BANK_FLAGS)
swap_check_alt_screen:
    ;; Test if alternate screen is currently swapped
    bit MF3_BANK_FLAGS_ALT_SCREEN_SWAPPED,a
    ret z    
swap_screen_buffers:
    call restore_menu_screen
    ld a,017h
    call switch_bank
    call swap_memory_context
    jp save_menu_screen
    ;; ================================================================
    ;; Toggle Options Display
    ;; Toggles the "[o]ff" indicator on main menu
    ;; ================================================================
main_cmd_toggle_options:
    ld a,(MF3_STATUS_FLAGS)
    xor MF3_STATUS_FLAGS_ENABLED_MASK
    ld (MF3_STATUS_FLAGS),a
main_menu_refresh:
    call print_mainmenu
    jr main_keyloop

    ;; ================================================================
    ;; Clear RAM Banks
    ;; Wipes banks 0x16, 0x14, 0x13, 0x11
    ;; Used for "Clean" operation to free memory
    ;; Only available when +3DOS is accessible
    ;; ================================================================
main_cmd_clear_banks:
    call check_48kmode
    jr z,main_keyloop

    call restore_menu_screen
    ld a,016h
    call clear_bank
clear_bank_continue_14:
    ;; Continue clearing banks: 0x14, 0x13, 0x11
    ld a,014h
    call clear_bank
    dec a
    call clear_bank
    ld a,011h
    call clear_bank
    call set_bank0_rom_x1
    jr main_menu_refresh

    ;; ================================================================
    ;; Screen Print Entry
    ;; Prepares and initiates screen dump to printer
    ;; Sets border to red and calls screen dump routine
    ;; ================================================================
main_cmd_print_screen:
    call restore_menu_screen
    ld a,RED
    out (0feh),a
    call screen_print_main
    jp bank_switch_routine
    
set_filename_cursor_pos:
    ld a,(MF3_BANK_CONTROL)
    sub 016h
    ld d,000h
    ld e,a    
    ld hl,MF3_WORK_BUFFER
    add hl,de    
    ld (hl),020h
    ld (0202bh),hl
    inc hl    
    ld (hl),0ffh
    ret    
save_options_entry:
    out (P_MF3_OUT),a
    call set_bank0_rom3

    call print_abort

    ;; print the tape message on the menu.
    
    ld hl,msg_tape
    ld b,00ah
    call printf

    ;; if locked in 48k mode then dont print the disk
    ;; menu item.
    
    call check_48kmode
    jr z, @skip_disk
    
    ld hl,msg_disk
    ld b,00bh
    call printf
    
@skip_disk:
    
    ld hl,MF3_MENU_ATTR_BASE
    ld b,020h
save_clear_flash_loop:
    ;; Clear flash (inverse) bits for save menu attribute rows
    res 7,(hl)
    inc hl    
    djnz save_clear_flash_loop
save_options_menu_loop:
    call print_save_menu
    ld hl,MF3_BANK_FLAGS
    res MF3_BANK_FLAGS_TAPE_MODE,(hl)
    ld sp,(05c3dh)
    ld de,06056h
    pop hl    
    push de    
    ld (iy+000h),0ffh
    call set_filename_cursor_pos

save_options_keyloop:
    call get_key_wait
    cp 054h
    jp z,save_start_tape
    cp 041h
    jp z,bank_switch_routine
    
    cp 053h
    jr z,save_options_set_screen_mode
    cp 050h
    jr z,save_options_set_program_mode
    call check_48kmode
    jr z,save_options_keyloop

    cp 044h
    jp z,save_start_disk

    cp 04bh
    jr z,save_options_toggle_mode

    ld hl,MF3_STATUS_FLAGS
    bit MF3_STATUS_FLAGS_SAVE_128K_LOCKED,(hl)
    jr z,save_options_keyloop
    cp 04ch
    jr z,save_options_toggle_compress
    jr save_options_keyloop
save_options_toggle_mode:
    ld a,(MF3_STATUS_FLAGS)
    xor MF3_STATUS_FLAGS_SAVE_128K_LOCKED_MASK
    ld (MF3_STATUS_FLAGS),a
    bit MF3_STATUS_FLAGS_SAVE_128K_LOCKED,a
    jr nz,save_options_menu_loop
    res MF3_STATUS_FLAGS_SAVE_COMPRESS,a
    jr save_options_update_status
save_options_toggle_compress:
    ld a,(MF3_STATUS_FLAGS)
    xor MF3_STATUS_FLAGS_SAVE_COMPRESS_MASK
save_options_update_status:
    ld (MF3_STATUS_FLAGS),a
    bit MF3_STATUS_FLAGS_SAVE_COMPRESS,a
    jr z,@clear_comp_reg
    ld a,030h
    jr @set_comp_reg
@clear_comp_reg:
    ld a,(MF3_SAVED_BANKM_COPY)
@set_comp_reg:
    ld (MF3_SAVED_BANKM),a
    jr save_options_menu_loop
save_options_set_screen_mode:
    ld hl,MF3_BANK_FLAGS
    set MF3_BANK_FLAGS_SAVE_SCREEN_MODE,(hl)
    jr save_options_menu_loop
save_options_set_program_mode:
    ld hl,MF3_BANK_FLAGS
    res MF3_BANK_FLAGS_SAVE_SCREEN_MODE,(hl)
    jr save_options_menu_loop

;; ================================================================
;; Wait for Key Release
;; Waits until all keys are released
;; Sets flag when ready for next key
;; ----------------------------------------------------------------
;; Uses:   A, IY
;; ================================================================
    ;; wait for a key?
wait_key:
    xor a    
    in a,(0feh)
    and 01fh
    cp 01fh
    jr nz,wait_key
    set ZX_FLAGS_KEY_READY_BIT,(iy+001h)
    ret    

    ;; ================================================================
    ;; Disk Load Entry
    ;; Called after DOS-menu filename entry to open the selected snapshot,
    ;; stage its header into BASIC workspace, and then hand off to ROM code.
    ;; ================================================================
disk_load_entry:
    ld hl,MF3_ADDR_PTR
    set MF3_ADDR_PTR_DISK_LOAD_RETRY,(hl)
    call restore_menu_screen
    call set_filename_cursor_pos
    call make_room_233bytes
    call prepare_dos_environment
    ld hl,MF3_WORK_BUFFER
    ld b,003h
    ld c,001h
    ld d,000h
    ld e,001h
    call file_open_read
    jr nc,snapshot_load_error
    ld hl,(PROG)
    ld de,000e9h
    ld b,003h
    call file_read
    jr nc,snapshot_load_error
    ld b,003h
    call dos_close_file
snapshot_load_error:
    ;; File open/read failed; jump to error handler if carry
    jp nc,error_handler
    ld hl,05800h
    ld d,h    
    ld e,l    
    inc de    
    ld bc,002ffh
    ld (hl),ATTR_BLUE_ON_BLUE
    ldir

    ;; set the border colour
    ld a,BLUE
    out (0feh),a

    ;; this isnt in the rom. 
    ld hl,05cd0h
    jp CALL_48ROM

    ;; ================================================================
    ;; Scan for BREAK Key
    ;; Checks if BREAK (SHIFT + SPACE) is being pressed
    ;; Returns: Carry clear if BREAK pressed, carry set if not
    ;; ================================================================
    ;; brkscan. nc  = true if break is pressed (break = shift + space)

brkscan:
    ld a,0feh
    in a,(0feh)
    rra
    ret c

    ;; ================================================================
    ;; Scan for SPACE Key
    ;; Checks if SPACE key is being pressed
    ;; Returns: Carry clear if SPACE pressed, carry set if not
    ;; ================================================================
    ;; spcscan. nc  = true if space is pressed

spcscan:
    ld a,07fh
    in a,(0feh)
    rra
    ret    
    
check_space_key:
    call swap_screen_if_128k
    ld hl,MF3_ADDR_PTR
    ;; bit 0 marks the DOS submenu as the active caller; bit 5 is reserved
    ;; for the later disk-load retry path.
    set MF3_ADDR_PTR_DOS_SUBMENU,(hl)
    res MF3_ADDR_PTR_DISK_LOAD_RETRY,(hl)
    call set_bank0_rom3
    call print_dosmenu
    call cls_print_msg
    out (P_MF3_OUT),a
    ld sp,(05c3dh)
    ld de,06056h
    pop hl    
    push de    
dos_menu_keyloop:
    call get_key_wait
    cp 041h
    jp z,bank_switch_routine
    cp 04ch
    jr z,dos_menu_load_file
    cp 045h
    jr z,dos_catalog_entry
    jr dos_menu_keyloop
dos_menu_load_file:
    ld hl,MF3_BANK_FLAGS
    set MF3_BANK_FLAGS_FILENAME_LOAD_MODE,(hl)
    jp save_filename_entry

    ;; ================================================================
    ;; +3DOS Catalog / Erase Browser
    ;; Enumerates directory entries, allows page-by-page review, and can
    ;; delete the currently highlighted file.
    ;; ================================================================
dos_catalog_entry:
    call restore_menu_screen
    ld hl,MF3_ADDR_PTR
    res MF3_ADDR_PTR_CATALOG_PAGED,(hl)
    res MF3_ADDR_PTR_CATALOG_DELETE_PENDING,(hl)
    ld bc,nmi_retvec_in_range
    call reclaim_make_room
    ld hl,(PROG)
    push hl    
    ld bc,001ach
    call clear_ram
    call prepare_dos_environment
    pop de    
    push de    
    ld hl,060afh
    ld bc,0004h
    ldir
dos_catalog_fetch_loop:
    ld hl,060afh
    ld b,021h
    ld c,001h
    pop de
    
    ld ix,DOS_CATALOG
    call CALL_ROM2
    jp nc,error_handler
    
    push bc    
    call scan_ay_registers

    pop bc    
    dec b    
    jp z,check_space_key
dos_catalog_delete_prompt:
    push bc    
    call print_toolkit_confirm
    pop bc    
    push bc    
    ld c,b    
dos_catalog_item_loop:
    push bc
    
    ;; not sure what this does.. turns on flash i think.??
    ld hl,msg_enter_attrs
    ld b,003h
    call printf

dos_catalog_show_item:
    pop bc    
    push bc    
    ld a,c    
    sub b    
    ld b,a    
    or a    
    ld hl,0000dh
    jr z,catalog_row_done
    ld de,0000dh
catalog_add_row_offset:
    ;; Add DE to HL for each catalog row to compute target address
    add hl,de    
    djnz catalog_add_row_offset
catalog_row_done:
    ;; Finished computing row offset; use HL as catalog entry pointer
    ld d,h    
    ld e,l    
    ld hl,(PROG)
    add hl,de    
    ld (02022h),hl
    ld b,008h
    call print_fn_char_loop
    ld a,02eh
    rst 30h    
    ld b,003h
    call print_fn_char_loop
    ld a,020h
    rst 30h    
    ld e,(hl)    
    inc hl    
    ld d,(hl)    
    ld h,d    
    ld l,e    
    call 01d4eh
    ld a,04bh
    rst 30h    
dos_catalog_delete_keyloop:
    call CALL_BEEPKEY
    cp 020h
    jr z,dos_catalog_skip_delete
    cp 041h
    jr z,dos_catalog_exit_to_menu
    cp 059h
    jr z,dos_catalog_delete_selected
    cp 04eh
    jr z,dos_catalog_skip_delete
    jr dos_catalog_delete_keyloop
dos_catalog_skip_delete:
    pop bc    
dos_catalog_next_item:
    djnz dos_catalog_item_loop
dos_catalog_finish_page:
    pop bc    
dos_catalog_after_page:
    ld a,b    
dos_catalog_page_state:
    cp 020h
    jr nc,dos_catalog_next_page
    ld a,(MF3_ADDR_PTR)
    bit MF3_ADDR_PTR_CATALOG_PAGED,a
    jp nz,dos_catalog_entry
    bit MF3_ADDR_PTR_CATALOG_DELETE_PENDING,a
    jp nz,dos_catalog_entry
    jp dos_catalog_delete_prompt
dos_catalog_next_page:
    ld hl,MF3_ADDR_PTR
    set MF3_ADDR_PTR_CATALOG_PAGED,(hl)
    ld de,05ccbh
    push de    
    ld hl,05e6bh
    ld bc,000bh
    ldir
    ld a,0ffh
    ld (de),a    
    pop hl    
    push hl    
    ld de,0000dh
    add hl,de    
    ld bc,0019fh
    call clear_ram
    call restore_menu_screen
    call prepare_dos_environment
    jp dos_catalog_fetch_loop
dos_catalog_exit_to_menu:
    pop bc    
    pop bc    
    jp check_space_key
dos_catalog_delete_selected:
    call restore_menu_screen
    ld hl,(02022h)
    ld de,MF3_WORK_BUFFER
    push de    
    ld bc,0008h
    ldir
    ld a,02eh
    ld (de),a    
    inc de    
    ld bc,0003h
    ldir
    ld a,0ffh
    ld (de),a    
    call prepare_dos_environment
    pop hl    
    call file_delete
    
    jp nc,error_handler
    call scan_ay_registers
    call print_toolkit_confirm
    ld hl,MF3_ADDR_PTR
    set MF3_ADDR_PTR_CATALOG_DELETE_PENDING,(hl)
    jp dos_catalog_skip_delete
print_toolkit_confirm:
    call set_bank0_rom3
    call print_abort
    ld a,006h
    rst 30h    
    ld a,006h
    rst 30h    
    call print_mf_attributes
    ld hl,0000dh
    ld b,00eh
    call printf
    jp print_yn_prompt
make_room_233bytes:
    ld bc,000e9h
reclaim_make_room:
    push bc    
    ld de,05ccbh
    ld hl,(05c59h)
    dec hl
    
    call CALL_RECLAIM
    pop bc    
    push bc    
    call CALL_MAKEROOM
    inc hl    
    pop bc    
    add hl,bc    
    ld (05c4bh),hl

    ld de,05ccbh
    ld bc,000e9h
    ld hl,snapshot_loader_basic_rem
    ldir
    
    ret    
init_basic_sysvars:
    ld hl,05fdfh
    ld (RAMTOP),hl
    ld (05cb4h),hl
    ld iy,ERRNR
    ld hl,03c00h
    ld (05c36h),hl
    ld hl,00040h
    ld (05c38h),hl

    ld hl,05cb6h
    ld (CHANS),hl        
    ld de,015afh
    ex de,hl    
    ld bc,0015h

    call CALL_LDIR

    ex de,hl    
    dec hl    
    ld (05c57h),hl
    inc hl    
    ld (PROG),hl
    ld (05c4bh),hl
    ld (hl),080h
    inc hl    
    ld (05c59h),hl
    ld (hl),00dh
    inc hl    
    ld (hl),080h
    inc hl    
    ld (05c61h),hl
    ld (05c63h),hl
    ld (05c65h),hl

    ld hl,015c6h
    ld de,STRMS
    ld bc,000eh
    call CALL_LDIR
    
    ld a,ATTR_BLACK_ON_WHITE_BRIGHT
    ld (05c8dh),a
    ld (05c8fh),a
    ld (05c48h),a
    ld hl,00123h
    ld (05c09h),hl
    dec (iy-03ah)
    ld (iy+031h),002h
    ret    
    ;; ================================================================
    ;; Toolkit Entry Point
    ;; Enters memory editor/toolkit mode
    ;; Initializes display at 0x2000, sets bank to 0x10
    ;; Provides memory viewing, editing, and debugging features
    ;; ================================================================
toolkit_entry:
    call swap_screen_if_128k
    ld hl,MF3_ENTRY_VECTOR
    ld (MF3_DISPLAY_ADDR),hl  ; Set initial address to 0x2000
    ld a,010h
    ld (MF3_CURRENT_BANK),a  ; Set bank to 0x10
    call switch_bank
toolkit_reset_state:
    call init_toolkit_buffers
    ld hl,MF3_ADDR_PTR
    res MF3_ADDR_PTR_PRINT_LINE_COUNT,(hl)
    res MF3_ADDR_PTR_PRINT_EXIT_TO_TOOLKIT,(hl)
    jp toolkit_resume
tool_cmd_print:
    ld hl,MF3_STATUS_FLAGS
    set MF3_STATUS_FLAGS_TOOL_WRITE_MODE,(hl)
    ld hl,MF3_ADDR_PTR
    set MF3_ADDR_PTR_PRINT_LINE_COUNT,(hl)
    set MF3_ADDR_PTR_PRINT_EXIT_TO_TOOLKIT,(hl)
    ld hl,rom_start
    ld (MF3_SAVED_DE),hl
    ld a,RED
    out (0feh),a
    jp toolkit_screen_view
    ;; implied ret
    
tool_addr_entry:
    call init_toolkit_buffers
tool_display_refresh:
    ld hl,MF3_ADDR_PTR
    bit MF3_ADDR_PTR_PRINT_LINE_COUNT,(hl)
    res MF3_ADDR_PTR_PRINT_LINE_COUNT,(hl)
    jr z,tool_keyloop
    call print_linefeed
    ld a,WHITE
    out (0feh),a

    ;; ================================================================
    ;; Toolkit Keyboard Handler
    ;; Main loop for memory editor/toolkit
    ;; Commands: Q)uit H)ex R)egisters W)rite T)ext P)rint S)elect bank
    ;;           SPACE=address ENTER=poke Arrows=navigate N/M=page 0-F=hex
    ;; ================================================================
tool_keyloop:
    call get_key
    jp z,tool_commit_input

    ;; Arrow key navigation
    cp 009h
    jp z,tool_nav_right  ; Right arrow
    cp 008h
    jp z,tool_nav_left  ; Left arrow
    cp 00bh
    jp z,tool_nav_up  ; Up arrow
    cp 00ah
    jp z,tool_nav_down  ; Down arrow

    ;; ENTER key - execute POKE
    cp 00ch
    jp z,tool_enter_poke

    cp ' '
    jr z,tool_addr_entry  ; SPACE - enter address mode
    cp '0'
    jr c,tool_keyloop
    cp ':'
    jp c,tool_input_digit  ; 0-9 hex digit
    cp 'N'
    jp z,tool_nav_next_page  ; N - next page
    cp 'M'
    jp z,tool_nav_prev_page  ; M - previous page
    cp 'H'
    jr z,tool_toggle_hex  ; H - toggle hex/decimal
    cp 'R'
    jr z,tool_cmd_registers  ; R - jump to registers
    cp 'W'
    jr z,tool_cmd_write_toggle  ; W - toggle write mode
    cp 'T'
    jr z,tool_cmd_text_toggle  ; T - toggle text mode
    cp 'P'
    jp z,tool_cmd_print  ; P - printer mode
    cp 'S'
    jr z,tool_cmd_select_bank  ; S - select bank
    cp 'Q'
    jr z,tool_quit  ; Q - quit toolkit
    cp 'A'
    jr c,tool_keyloop

    cp 'G'
    jp c,tool_input_hex_alpha  ; A-F hex digit
    jr tool_keyloop

    ;; ================================================================
    ;; Toolkit Quit
    ;; Exits toolkit and returns to main menu
    ;; ================================================================
tool_quit:
    call restore_toolkit_screen
    jp bank_switch_routine

    ;; ================================================================
    ;; Toggle Hex/Decimal Display
    ;; Switches between hexadecimal and decimal display modes
    ;; Controlled by bit 6 of memory location 0x2006
    ;; ================================================================
tool_toggle_hex:
    ld a,(MF3_STATUS_FLAGS)
    xor MF3_STATUS_FLAGS_TOOL_HEX_MODE_MASK
    ld (MF3_STATUS_FLAGS),a
    jr toolkit_nav_entry

    ;; ----------------------------------------------------------------
    ;; Register Display Entry (toolkit 'R' command)
    ;; Sets display address to 0x3FE4 - CPU register storage area
    ;; Register order (INTEL format - low byte, high byte):
    ;;   PC IY IX BC' DE' HL' AF' BC DE R- I- HL AF SP
    ;; Total 28 bytes from 0x3FE4 to 0x3FFF
    ;; ----------------------------------------------------------------
tool_cmd_registers:
    ld hl,03fe4h  ; Point to register storage area
    ld (MF3_DISPLAY_ADDR),hl  ; Set as current display address
    jr toolkit_nav_entry
tool_cmd_write_toggle:
    ld a,(MF3_STATUS_FLAGS)
    xor MF3_STATUS_FLAGS_TOOL_WRITE_MODE_MASK
    ld (MF3_STATUS_FLAGS),a
    bit MF3_STATUS_FLAGS_TOOL_WRITE_MODE,a
    call z,restore_toolkit_screen
toolkit_resume:
    ld hl,rom_start
    ld (MF3_SAVED_DE),hl
toolkit_nav_entry:
    jp toolkit_nav
tool_cmd_text_toggle:
    ld a,(RAMAREA)
    xor 020h
    ld (RAMAREA),a
    jr toolkit_resume

tool_cmd_select_bank:
    call check_48kmode
    jr z,tool_input_hex_common

    ;; print ? and wait for keypress
    ld hl,msg_page_question
    ld b,004h
    
    call printf
numloop:
    call CALL_BEEPKEY

    ;; if ((key > 0x30) || (key < 0x38)) 
    cp 030h
    jr c,numloop
    cp 038h
    jr nc,numloop

    ;; keep the 3 LSB
    and 007h
    or 010h
    
    ;; switch ram bank to the new value
    ld (MF3_CURRENT_BANK),a
    call switch_bank

    ;;  print the string
    push af    
    ld hl,tool_msg_bank_num_attrs
    ld b,007h
    call printf
    pop af

    ;; print the number between 0 and 7
    
    and 007h
    or 030h
    rst 30h  ;print a single char
    
    jr toolkit_resume
    
init_toolkit_buffers:
    call print_toolkit_header
    ld hl,MF3_PTR_5FF4
    ld (MF3_BANK_PTR),hl
    ld b,006h
    call fill_quotes
    ld hl,MF3_SCREEN_SIZE
    ld b,004h
    jr fill_quotes
print_and_fill_3bytes:
    ld hl,MF3_SCREEN_SIZE
    push hl    
    ld b,003h
    call printf
    pop hl    
fill_3_quotes:
    ld b,003h
fill_quotes:
    ld (hl),022h
    inc hl    
    djnz fill_quotes
    ret    
tool_enter_poke:
    call compare_pos_to_buffer_end
    jp nc,toolkit_nav
    jp tool_addr_entry
tool_input_hex_alpha:
    ld hl,MF3_STATUS_FLAGS
    bit MF3_STATUS_FLAGS_TOOL_HEX_MODE,(hl)
tool_input_hex_common:
    jr z,tool_loop_back
    res 5,a
tool_input_digit:
    ld c,a    
    ld hl,MF3_PTR_5FFE
    ld a,(MF3_STATUS_FLAGS)
    bit MF3_STATUS_FLAGS_TOOL_HEX_MODE,a
    jr z,tool_input_hex_store
    dec hl    
    ld a,048h
    ld (MF3_PTR_5FFE),a
tool_input_hex_store:  ; store digit into input buffer, check bounds
    ld de,(MF3_BANK_PTR)
    and a    
    sbc hl,de
    jr c,tool_loop_back
    ld a,c    
    push af    
    rst 30h    
    pop af    
    ld hl,(MF3_BANK_PTR)
    ld (hl),a    
    inc hl    
    ld (MF3_BANK_PTR),hl
    ld de,05ff9h
    ld a,(MF3_STATUS_FLAGS)
    bit MF3_STATUS_FLAGS_TOOL_HEX_MODE,a
    jr z,tool_input_hex_check_full
    dec de    
    ld a,048h
    ld (MF3_SAVED_BC2),a
tool_input_hex_check_full:  ; check if input buffer is full after storing
    and a    
    sbc hl,de
    jp z,tool_input_buffer_full
tool_loop_back:
    jp tool_display_refresh

    ;; ================================================================
    ;; Memory Address Validator and Translator
    ;; Validates and translates memory addresses for toolkit viewing
    ;; Handles special memory regions:
    ;; - 0x5B00-0x60B3: System variables ??? MF3 RAM (SWAP area)
    ;; - 0x50C0+: Screen area ??? MF3TEMP preserved screen
    ;; - 0xC000-0xFFFF: Banked RAM (bank selection at 0x6001)
    ;; Returns: Carry clear if invalid, Carry set + HL translated if valid
    ;; ================================================================
validate_mem_addr:
    ld hl,(MF3_DISPLAY_ADDR)
translate_mem_addr:
    bit 7,h
    jr z,tool_addr_skip_bank_xlat
    bit 6,h
    jr z,tool_addr_skip_bank_xlat
    ld a,(MF3_CURRENT_BANK)
    and 007h
    cp 005h
    jr nz,tool_addr_skip_bank_xlat
    res 7,h
tool_addr_skip_bank_xlat:  ; address below 0x4000 or bank 5: skip bank xlat
    ld bc,SWAP
    and a    
    sbc hl,bc
    push hl    
    pop de    
    add hl,bc    
    jr c,tool_addr_check_screen
    ld bc,060b3h
    and a    
    sbc hl,bc
    add hl,bc    
    ret nc    
    ld hl,0202dh
    add hl,de    
tool_addr_invalid_ret:  ; return carry clear (address invalid)
    scf    
    ccf    
    ret    
tool_addr_check_screen:  ; check if address is in screen area (0x50C0+)
    ld bc,050c0h
    and a    
    sbc hl,bc
    push hl    
    pop de    
    add hl,bc    
    jr c,tool_addr_check_mf3ram
    ld a,h    
    bit 3,a
    jr z,tool_addr_screen_row_ok
    cp 05ah
    jr nz,tool_addr_check_mf3ram
tool_addr_screen_row_ok:  ; screen row valid (0x5A), check line offset
    ld a,l    
    and 0c0h
    cp 0c0h
    jr nz,tool_addr_check_mf3ram
    ld hl,MF3TEMP
    ld b,d    
    xor a    
    ld d,a    
    or b    
    jr z,tool_addr_screen_add_offset
    push de    
    cp 00ah
    jr nz,tool_addr_screen_calc_offset
    dec b    
    dec b    
tool_addr_screen_calc_offset:  ; calc MF3TEMP offset for non-line-10 rows
    ld e,040h
tool_addr_screen_add_line:  ; loop: add 64 bytes per screen line
    add hl,de    
    djnz tool_addr_screen_add_line
    pop de    
tool_addr_screen_add_offset:  ; add final column offset to MF3TEMP base
    add hl,de    
    jr tool_addr_invalid_ret
tool_addr_check_mf3ram:  ; check main screen / MF3 RAM area validity
    ld a,(MF3_BANK_FLAGS)
    bit MF3_BANK_FLAGS_TOOL_SCREEN_SAVED,a
    jr z,tool_addr_check_entry_vector
    ld a,h    
    and 0f0h
    cp 040h
    jr z,tool_addr_translate_main_screen
    ld a,h    
    cp 058h
    jr z,tool_addr_translate_attrs
    cp 059h
    jr z,tool_addr_translate_attrs
tool_addr_check_entry_vector:  ; validate against MF3 entry vector range
    push hl    
    ld de,MF3_ENTRY_VECTOR
    and a    
    sbc hl,de
    pop hl    
    ret    
tool_addr_translate_main_screen:  ; map 0x4000-0x57FF to saved screen at 0x2821
    ld de,0e821h
tool_addr_translate_screen_valid:  ; apply screen translation offset, return valid
    add hl,de    
    jr tool_addr_invalid_ret
tool_addr_translate_attrs:  ; map 0x5800-0x59FF to saved attrs at 0x3821
    ld de,0e021h
    jr tool_addr_translate_screen_valid
tool_nav_prev_page:
    ld hl,0ff80h
    jr toolkit_update
tool_nav_up:
    ld hl,0fff8h
toolkit_update:
    ld bc,(MF3_DISPLAY_ADDR)
    add hl,bc    
    push hl    
    pop bc    
    ld hl,MF3_SCREEN_SIZE
    ld (MF3_BANK_PTR),hl
    jr tool_set_display_addr
tool_nav_next_page:
    ld hl,128
    jr toolkit_update
tool_nav_down:
    ld hl,8
    jr toolkit_update
tool_nav_left:
    ld hl,0ffffh
    jr toolkit_update
tool_nav_right:
    ld hl,1
    jr toolkit_update
compare_pos_to_buffer_end:
    ld hl,(MF3_BANK_PTR)
    ld de,MF3_SCREEN_SIZE
    and a    
    sbc hl,de
    ret    
tool_commit_input:
    ld hl,(MF3_BANK_PTR)
    ld de,MF3_PTR_5FF4
    and a    
    sbc hl,de
    jr z,toolkit_nav
    call compare_pos_to_buffer_end
    jr c,tool_input_buffer_full
    jp z,tool_commit_parse_addr
    call parse_3byte_hex_input
    xor a    
    or b    
    jr nz,toolkit_nav
    push bc    
    call validate_mem_addr
    pop bc    
    ld (hl),c    
    jr toolkit_nav
tool_commit_parse_addr:  ; buffer at start - parse full 5-digit hex address
    call parse_5byte_hex_input
    inc bc    
    jr tool_commit_check_addr
tool_input_buffer_full:  ; buffer full - parse as hex address
    call parse_5byte_hex_input
tool_commit_check_addr:  ; validate parsed address, jump to entry if valid
    jp c,tool_addr_entry
tool_set_display_addr:
    ld (MF3_DISPLAY_ADDR),bc
toolkit_nav:
    ld hl,(MF3_DISPLAY_ADDR)
    ld de,MF3_PTR_5FF4
    call convert_to_decimal_p1
    call display_buffer_or_addr
    ld hl,MF3_SCREEN_SIZE
    ld (MF3_BANK_PTR),hl
    ld a,03dh
    rst 30h    
    call validate_mem_addr
    jr c,tool_display_read_rom
    ld a,(hl)    
    jr tool_display_got_byte
tool_display_read_rom:  ; address is ROM - read via interop call
    call CALL_READROM
    ld a,c    
tool_display_got_byte:  ; have byte in A - choose hex/decimal display
    ld hl,MF3_STATUS_FLAGS
    bit MF3_STATUS_FLAGS_TOOL_HEX_MODE,(hl)
    jr nz,tool_display_hex_mode
    ld l,a    
    xor a    
    ld h,a    
    ld de,MF3_SCREEN_SIZE
    call convert_to_decimal_p2
    call print_and_fill_3bytes
    jr tool_display_after_value
tool_display_hex_mode:  ; display byte as hex with 'H' suffix
    call print_byte
    ld a,048h
    rst 30h    
    ld hl,MF3_SCREEN_SIZE
    call fill_3_quotes
tool_display_after_value:  ; continue display after value - show address info
    ld a,020h
    rst 30h    
    ld de,(MF3_DISPLAY_ADDR)
    ld hl,03fffh
    and a    
    sbc hl,de
    jr c,tool_display_badaddr
    add hl,de    
    ex de,hl    
    ld de,03fe4h
    and a    
    sbc hl,de
    jr c,tool_display_badaddr
    push hl    
    add hl,hl    
    ld de,tool_data_table_base
    add hl,de    
tool_display_print_data:  ; print data string and poke indicator
    ld b,002h
    call printf
    ld hl,msg_poke
    ld b,015h
    call printf
    ld hl,MF3_STATUS_FLAGS
    bit MF3_STATUS_FLAGS_TOOL_WRITE_MODE,(hl)
    jr nz,toolkit_screen_view
    jp tool_display_refresh
tool_display_bad_data:  ; bad address - show placeholder instead of data
tool_display_badaddr:
    ld hl,msg_two_spaces
    jr tool_display_print_data
toolkit_screen_view:
    ld hl,MF3_BANK_FLAGS
    bit MF3_BANK_FLAGS_TOOL_SCREEN_SAVED,(hl)
    call z,tools_cls
    
    ld a,002h
    call open_channel

    ld hl,tool_msg_screen_view_header
    call print_msg_len9

    ld hl,(MF3_DISPLAY_ADDR)
    ld a,l    
    and 0f8h
    ld l,a    
    ld de,0020h
    and a    
    sbc hl,de
    ld de,(MF3_SAVED_DE)
    and a    
    sbc hl,de
    add hl,de    
    jp z,01992h
    ld (MF3_SAVED_DE),hl
    call print_4_spaces
    call print_space_addr_colon
    call 019e3h
    ld a,020h
    rst 30h    
    call display_toolkit_cursor
    call print_n_spaces
tool_screen_close_footer:  ; close screen channel and print footer string
    ld a,0fdh
    call open_channel
    ld hl,0193fh
    ld b,006h
    call printf
    jp tool_display_refresh

    ;; ----------------------------------------------------------------
    ;; Toolkit Menu Message
    ;; Displays command help at top of screen
    ;; Shows: [q]uit [ENT]poke [SPC]addr [r]eg [w]in [h]x [t]xt [p]r
    ;; ----------------------------------------------------------------
msg_toolmenu:

    INVERSE 1
    defb "q"
    INVERSE 0
    defb "uit"

    INVERSE 1
    defb "ENT"
    INVERSE 0
    defb "poke"

    INVERSE 1
    defb "SPC"
    INVERSE 0
    defb "addr"

    INVERSE 1
    defb "r"
    INVERSE 0
    defb "eg"

    INVERSE 1
    defb "w"
    INVERSE 0
    defb "in"

    INVERSE 1
    defb "h"
    INVERSE 0
    defb "x"

    INVERSE 1
    defb "t"
    INVERSE 0
    defb "xt"

    INVERSE 1
    defb "p"
    INVERSE 0
    defb "r "

    AT 1,0
    PAPER RED
    INK WHITE
    BRIGHT 1

    defb "Address:"
    defb TAB,TAB
tool_msg_bank_header_attrs:  ; bank display header (19 bytes: AT/INK/PAPER codes)
    ld d,000h
    rra    
    ld de,01006h
    ld bc,nmi_preserve_state
    ld (hl),e    
    inc d    
    nop    

tool_msg_bank_num_attrs:  ; bank number display positioning attributes
    AT 1,31
    INK WHITE
    PAPER RED

    AT 1,8
    INK WHITE
    PAPER RED
    BRIGHT 1

    ;; ----------------------------------------------------------------
    ;; POKE Prompt Message
    ;; Shows address and current value for editing
    ;; Displayed when write mode is active
    ;; ----------------------------------------------------------------
msg_poke:
    AT 1,20
    defb " Poke:"

    ;; flashing cursor?
    FLASH 1
    defb " "
    FLASH 0
    defb "    "
    AT 1,26
tool_data_table_base:  ; BCD conversion data table (code/data overlap)
    ld (hl),b    
    ld b,e    
    ld d,b    
    ld h,e    
    ld e,c    
    jr nz,$+75
    ld a,c    
    ld e,b    
    jr nz,display_toolkit_cursor
    ld a,b    
    ld b,e    
    daa    
    ld b,d    
    daa    
    ld b,l    
    daa    
    ld b,h    
    daa    
    ld c,h    
    daa    
    ld c,b    
    daa    
    ld b,(hl)    
    daa    
    ld b,c    
tool_data_bcd_mid1:  ; BCD conversion midpoint 1
    daa    
    ld b,e    
tool_data_bcd_mid2:  ; BCD conversion midpoint 2
    jr nz,tool_cursor_set_attr
    jr nz,$+71
    jr nz,tool_cursor_set_next_attr
    jr nz,tool_print_space_loop
    ld h,d    
    ld d,d    
    jr nz,tool_print_space_hex
    ld l,c    
    ld c,c    
    jr nz,tool_print_space_char
    jr nz,$+74
    jr nz,$+72
    jr nz,$+67
    jr nz,tool_screen_print_char
    ld d,b    
    ld d,e    
    jr nz,$+44
    jp m,07d5fh
    and 0f8h
    ld l,a    
    ld a,016h
    rst 30h    
    ld a,004h
    rst 30h    
    ld a,007h
    rst 30h    
    call 019e3h
    ld a,020h
    rst 30h    
    call display_toolkit_cursor
    jp tool_screen_close_footer
display_toolkit_cursor:
    push hl    
    ld hl,(MF3_DISPLAY_ADDR)
    ld a,l    
    and 007h
    ld c,a    
    add a,a    
    add a,c    
    ld c,a    
    ld b,000h
tool_cursor_set_attr:  ; set inverse attribute at cursor column position
    ld hl,05888h
    add hl,bc    
    ld (hl),0f1h
tool_cursor_set_next_attr:  ; set inverse attribute at next column
    inc hl    
    ld (hl),0f1h
    pop hl    
    ret    
print_4_spaces:
    ld b,004h
    jr tool_print_space_loop
print_n_spaces:
    ld b,00bh
tool_print_space_loop:  ; loop: print space, hex addr, space per display line
    push bc    
    call print_space_addr_colon
tool_print_space_hex:  ; print hex address in space loop
    call 019e3h
tool_print_space_char:  ; print space character in loop
    ld a,020h
    rst 30h    
    pop bc    
    djnz tool_print_space_loop
    ret    
tool_msg_screen_view_header:  ; 9-byte screen view row header (positioning data)
    ld d,000h
    nop    
    djnz tool_screen_init_row
tool_screen_init_row:  ; initialize screen row parameters (DE=offset, BC=06/08h)
    ld de,save_options_update_status+2
    ld bc,00806h
tool_screen_row_loop:  ; main loop: display one byte per iteration
    push bc    
    ld a,020h
    rst 30h    
    push hl    
    call translate_mem_addr
    jr c,tool_screen_read_rom_byte
    ld a,(hl)    
    jr tool_screen_have_byte

tool_screen_read_rom_byte:  ; byte in ROM area - read via interop call
    call CALL_READROM
    ld a,c    
tool_screen_have_byte:  ; have byte in A - branch to hex or text mode
    ld hl,RAMAREA
    bit RAMAREA_TEXT_DISPLAY_MODE,(hl)
    jr nz,tool_screen_print_char
    call print_byte
    jr tool_screen_next_byte
tool_screen_print_char:  ; display byte as filtered printable character
    call filter_printable_char
    rst 30h    
    ld a,020h
    rst 30h    
tool_screen_next_byte:  ; advance HL to next byte in screen row
    pop hl    
    inc hl    
    pop bc    
    djnz tool_screen_row_loop
    ret

;; ================================================================
;; Filter Printable Character
;; Converts non-printable characters to '.'
;; Accepts characters in range 0x20-0x7F
;; ----------------------------------------------------------------
;; Input:  A = character
;; Output: A = character or '.' if non-printable
;; ================================================================
filter_printable_char:
    cp 020h
    jr nc,tool_filter_check_upper
tool_filter_dot:  ; return '.' (0x2E) for non-printable character
    ld a,02eh
    ret    
tool_filter_check_upper:  ; check if char exceeds 0x7F (non-printable)
    cp 080h
    ret c    
    jr tool_filter_dot
    ;; impled ret

;; ----------------------------------------------------------------
;; Print Space, Address, Colon
;; Prints space, 4-digit hex address, colon
;; Used in toolkit memory display
;; ----------------------------------------------------------------
print_space_addr_colon:
    ld a,020h
    rst 30h    
    call print_hex_word_suffix
    ld a,03ah
    rst 30h    
    ret

    ;; ================================================================
    ;; Toolkit Clear Screen
    ;; Clears toolkit display area
    ;; Saves screen to 0x2821 and attributes to 0x3821
    ;; Then clears screen area and sets attributes to ATTR_BLACK_ON_YELLOW_BRIGHT (black ink, bright yellow paper)
    ;; ================================================================
tools_cls:
    set MF3_BANK_FLAGS_TOOL_SCREEN_SAVED,(hl)

    ld hl,SCREEN
    ld de,02821h
    ld bc,0x1000
    ldir

    ld hl,05800h
    ld de,03821h
    ld bc,01ffh+1
    ldir

    ld hl,SCREEN
    ld bc,0x0FFF
    call clear_ram

    ld hl,05800h
    ld (hl),ATTR_BLACK_ON_YELLOW_BRIGHT
    
    push hl    
    pop de    
    inc de    

    ld bc,01ffh
    ldir

    ret    

restore_toolkit_screen:
    ld hl,MF3_BANK_FLAGS
    bit MF3_BANK_FLAGS_TOOL_SCREEN_SAVED,(hl)
    res MF3_BANK_FLAGS_TOOL_SCREEN_SAVED,(hl)
    ret z    

    ld hl,02821h
    ld de,SCREEN
    ld bc,0x1000
    ldir
    
    ld de,05800h
    ld hl,03821h
    ld bc,01ffh+1
    ldir
    ret    

display_buffer_or_addr:
    ld hl,0193ch
    ld b,009h
    call printf
    ld hl,MF3_STATUS_FLAGS
    bit MF3_STATUS_FLAGS_TOOL_HEX_MODE,(hl)
    jr nz,tool_dispaddr_hex
    ld b,005h
    ld hl,MF3_PTR_5FF4
    jp print_number_init
tool_dispaddr_hex:  ; display current address as 4-digit hex
    ld hl,(MF3_DISPLAY_ADDR)
print_hex_word_suffix:
    push hl    
    ld a,h    
    call print_byte
    pop hl    
    ld a,l    
    call print_byte
    ld a,048h
    jr tool_hex_suffix_print
print_toolkit_header:
    call cls_print_header
    ld hl,msg_toolmenu
    ld b,053h
    call printf
    call check_48kmode
    jr z,tool_header_skip_bank

    ld hl,tool_msg_bank_header_attrs
    ld b,013h
    call printf

    ld a,(MF3_CURRENT_BANK)
    and 007h
    or 030h
    rst 30h    
tool_header_skip_bank:  ; 48K mode: skip bank number display
    ld hl,0193ch
    ld b,009h
    jp printf

;; ================================================================
;; Convert to Decimal (Part 1)
;; Converts binary value in HL to decimal ASCII digits
;; Handles 10000s and 1000s places
;; ----------------------------------------------------------------
;; Input:  HL = binary value
;; Output: DE = buffer filled with ASCII digits
;; Uses:   HL, BC, A, DE
;; ================================================================
convert_to_decimal_p1:
    ld bc,0d8f0h
    call extract_decimal_digit
    ld bc,0fc18h
    call extract_decimal_digit

;; ----------------------------------------------------------------
;; Convert to Decimal (Part 2) - Handles 100s, 10s, 1s places
;; ----------------------------------------------------------------
convert_to_decimal_p2:
    ld bc,0ff9ch
    call extract_decimal_digit
    ld bc,0fff6h
    call extract_decimal_digit
    ld a,l    
tool_dec_store_digit:  ; store final decimal digit as ASCII and advance DE
    add a,030h
    ld (de),a
    inc de
    ret

;; ================================================================
;; Extract Decimal Digit
;; Extracts one decimal digit by repeated subtraction
;; Uses negative BC value (complement) for efficient subtraction
;; ----------------------------------------------------------------
;; Input:  HL = value, BC = negative divisor
;; Output: A = digit (ASCII), HL = remainder, DE updated
;; Uses:   HL, BC, A
;; ================================================================
extract_decimal_digit:
    xor a    
tool_dec_subtract_loop:  ; repeated addition of negative divisor to extract digit
    add hl,bc    
    inc a    
    jr c,tool_dec_subtract_loop
    sbc hl,bc
    dec a
    jr tool_dec_store_digit

;; ================================================================
;; Print Hex Byte
;; Converts and prints byte in A as 2-digit hexadecimal
;; Prints both nibbles (upper then lower)
;; ----------------------------------------------------------------
;; Input:  A = byte to print
;; Uses:   A
;; ================================================================
print_byte:
    push af    
    srl a
    srl a
    srl a
    srl a
    add a,030h
    cp 03ah
    jr c,tool_hex_nibble_no_adj
    add a,007h
tool_hex_nibble_no_adj:  ; upper nibble < 0x0A, no alpha adjustment needed
    rst 30h    
    pop af    
    and 00fh
tool_hex_lower_nibble:  ; process lower nibble (code addr 0x1B00 = screen size 6912)
    add a,030h
    cp 03ah
    jr c,tool_hex_suffix_print
    add a,007h
tool_hex_suffix_print:  ; print hex suffix 'H' or final lower nibble
    rst 30h
    ret

;; ----------------------------------------------------------------
;; Calculate Buffer Offset
;; Calculates offset into input buffer
;; Input: DE = buffer address, A = offset | Output: HL, B
;; ----------------------------------------------------------------
calc_buffer_offset:
    ld hl,(MF3_BANK_PTR)
    and a    
    sbc hl,de
    ex de,hl    
    ld b,e
    ret

;; ================================================================
;; Parse 3-Byte Hex Input
;; Parses 3 bytes of hexadecimal input from buffer
;; Supports both hex (with 'H') and decimal formats
;; ----------------------------------------------------------------
;; Output: BC = parsed value, Carry clear = success
;; Uses:   HL, DE, BC, A
;; ================================================================
parse_3byte_hex_input:
    ld de,MF3_SCREEN_SIZE
    ld a,003h
    call calc_buffer_offset
    ld a,022h
    ld (05fffh),a
    ld a,(MF3_PTR_5FFE)
    cp 048h
    jr nz,tool_parse_decimal
    jr tool_parse_hex_format

;; ================================================================
;; Parse 5-Byte Hex Input
;; Parses 5 bytes of hexadecimal input from buffer
;; Supports both hex (with 'H') and decimal formats
;; ----------------------------------------------------------------
;; Output: BC = parsed value, Carry clear = success
;; Uses:   HL, DE, BC, A
;; ================================================================
parse_5byte_hex_input:
    ld de,MF3_PTR_5FF4
    ld a,005h
    call calc_buffer_offset
    ld a,022h
    ld (05ff9h),a
    ld a,(MF3_SAVED_BC2)
    cp 048h
    jr nz,tool_parse_decimal
tool_parse_hex_format:  ; parse input as hex (H terminator found)
    ld de,rom_start
tool_parse_hex_char_loop:  ; loop: convert each hex char to nibble
    ld a,(hl)    
    cp 048h
    jr z,tool_parse_hex_done
    call hex_char_to_nibble
    inc hl    
    djnz tool_parse_hex_char_loop
tool_parse_hex_done:  ; hex parse complete or terminated by non-hex char
    push de    
    pop bc    
    scf    
    ccf    
    ret    

tool_parse_decimal:  ; parse input as decimal via ROM INTTOFP/FPTOBC
    ld (05c5dh),hl
    ld a,(hl)    
    ld hl,INTTOFP
    call CALL_48ROM
    ld hl,FPTOBC
    jp CALL_48ROM

;; ================================================================
;; Hex Char to Nibble
;; Converts ASCII hex character ('0'-'9', 'A'-'F') to 4-bit value
;; Shifts result into DE register pair (builds multi-byte value)
;; ----------------------------------------------------------------
;; Input:  A = ASCII hex char
;; Output: DE = shifted result with new nibble
;; Uses:   A, B, DE
;; ================================================================
hex_char_to_nibble:
    push bc    
    sub 030h
    bit 4,a
    jr z,tool_hex_digit_ok
    sub 007h
tool_hex_digit_ok:  ; digit 0-9 - no alpha subtraction needed
    rlca    
    rlca    
    rlca    
    rlca    
    ld b,004h
tool_hex_shift_nibble:  ; rotate nibble into DE result (4-bit shift loop)
    rlca    
    rl e
    rl d
    djnz tool_hex_shift_nibble
    pop bc    
    ret    

    ;; ================================================================
    ;; INTEROP ROUTINES BLOCK START
    ;; These routines are copied to RAMAREA (0x5FE0) during initialization
    ;; They provide a bridge to call Spectrum ROM functions while
    ;; the Multiface ROM is temporarily paged out
    ;; Total size: 0xD3 (211) bytes
    ;; ================================================================
interop_start:
    ;; ================================================================
    ;; Default Workspace Variables & Metadata (49 bytes)
    ;; This block initializes the Multiface 3 workspace in RAMAREA (0x5FE0).
    ;; It contains the initial variable states and default disk filename.
    ;; ================================================================
    defb 0x00, 0x03
    defb "        "  ; 8 spaces
msg_two_spaces:
    defb "  "  ; 2 spaces (historically l1b82h)
    defb 0x00, 0x1B, 0x00, 0x40, 0xFF, 0xFF
    defb 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    defb " "
    defb 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    defb 0x16, 0x00, 0x00
msg_default_filename:
    defb "DISK    "  ; Default disk filename (historically l1ba0h)
    defb 0xFF
    
    ;; ================================================================
    ;; Zero-Padding (54 bytes)
    ;; Pads the workspace area to align the following execution helpers
    ;; to exactly RAMAREA + 103 and onwards.
    ;; ================================================================
    defb 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    defb 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    defb 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    defb 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    defb 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    defb 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    defb 0x00, 0x00, 0x00, 0x00, 0x00, 0x00    

    ;; 

    push af
    in a,(P_MF3_OUT)
    pop af
    call CALL_IX  ; int_call_ix

    ;; ================================================================
    ;; Page In MF3
    ;; Common exit routine for interop functions
    ;; Pages Multiface back in and returns
    ;; ================================================================
page_in_mf3:

    di
    push af
    in a,(P_MF3_IN)
    pop af
    ret

    ;; ================================================================
    ;; Interop: Call via IX Register
    ;; Calls routine pointed to by IX with MF3 paged out
    ;; Located at CALL_IX (RAMAREA + 116)
    ;; ================================================================
int_call_ix:
    jp (ix)
    ;; implied ret

    ld hl,rom_entry_nmi_exit

    di    
    push af    
    in a,(P_MF3_IN)
    pop af    

    ;; 0x605e
    jp (hl)    

    ;; ================================================================
    ;; Interop: Call 48K ROM Routine
    ;; Calls routine at address in HL with MF3 paged out
    ;; Preserves A and F, but HL is used for call address
    ;; Located at CALL_48ROM (RAMAREA + 127)
    ;; ================================================================
int_call_48rom:
    push af
    in a,(P_MF3_OUT)
    pop af

    ;; call the location in HL
    call 0605eh
    jr page_in_mf3

    ;; ================================================================
    ;; Interop: Beeper and Keyboard Read
    ;; Waits for keypress with beep feedback
    ;; Uses Spectrum keyboard scanning and beeper routine
    ;; Returns keypress in A
    ;; Located at CALL_BEEPKEY (RAMAREA + 136)
    ;; ================================================================
int_call_beep:
    ;; page the multiface out
    ;; this code runs in main ram.

    in a,(P_MF3_OUT)

    res ZX_FLAGS_INT_DONE_BIT,(iy+001h)
    set ZX_FLAGS_KEY_READY_BIT,(iy+001h)
    set ZX_SYSVAR_5C6A_BEEP_SYNC_BIT,(iy+030h)

    ei  ; enable interruptss
    halt  ; wait for the horizontal refresh
    di  ; disable interrputs again.

    bit ZX_FLAGS_INT_DONE_BIT,(iy+001h)
    jr z,int_call_beep

    ld de,0022h
    ld hl,000c8h

    call BEEPER  ; call the 48k rom beeper routine

    ;;  page the mf3 back in and return
    di
    in a,(P_MF3_IN)
    ld a,(LASTK)
    ret
    
    ;; ================================================================
    ;; Interop: Print Character (RST 10h)
    ;; Prints character in A register using Spectrum ROM
    ;; Located at CALL_RST10 (RAMAREA + 175)
    ;; ================================================================
int_call_rst10:
    push af
    in a,(P_MF3_OUT)
    pop af
    rst 10h
    jr page_in_mf3
    ;; implied ret

    ;; ================================================================
    ;; Interop: LDIR with MF3 Paged Out
    ;; Performs block copy with Multiface paged out
    ;; Uses: HL=source, DE=dest, BC=count
    ;; Located at CALL_LDIR (RAMAREA + 182)
    ;; ================================================================
int_call_ldir:
    ;; perform a ldir with the mf3 paged out.
    in a,(P_MF3_OUT)
    ldir
    jr page_in_mf3
    ;; implied ret

    ;; ================================================================
    ;; Interop: BASIC Reclaim Memory
    ;; Calls Spectrum BASIC reclaim routine
    ;; Located at CALL_RECLAIM (RAMAREA + 188)
    ;; ================================================================
int_call_reclaim:
    ;; call the basic reclaim function
    in a,(P_MF3_OUT)
    call RECLAIM1
    jr page_in_mf3
    ;; implied ret

    ;; ================================================================
    ;; Interop: BASIC Make Room
    ;; Calls Spectrum BASIC make room routine
    ;; Located at CALL_MAKEROOM (RAMAREA + 195)
    ;; ================================================================
int_call_makeroom:
    ;; call the basic makeroom funtion
    in a,(P_MF3_OUT)
    call MAKEROOM  ; call make room?
    jr page_in_mf3
    ;; implied ret

    ;; ================================================================
    ;; Interop: Read ROM Byte
    ;; Reads byte from ROM at address in HL, returns in C
    ;; Located at CALL_READROM (RAMAREA + 202)
    ;; ================================================================
int_read_rom:
    ;; read a byte from rom. HL = location, result in C.
    in a,(P_MF3_OUT)
    ld c,(hl)
    jr page_in_mf3
    ;; implied ret

    ld hl,(02a2eh)
    rst 38h    
    nop    

    ;; ================================================================
    ;; END OF INTEROP ROUTINES BLOCK
    ;; ================================================================
interop_end:

    ;; ================================================================
    ;; +3DOS File Operations
    ;; Wrappers for +3DOS ROM 2 functions
    ;; ================================================================

    ;; Open file for reading
file_open_read:
    ld ix,DOS_OPEN
dos_call_dispatch:
    ;; Shared +3DOS call entry: jumps to CALL_ROM2 with IX set to DOS function
    jp CALL_ROM2

    ;; Read from file
file_read:
    ld ix,DOS_READ
    jr dos_call_dispatch
    ;; implied ret

    ;; Delete file
file_delete:
    ld ix,DOS_DELETE
    jr dos_call_dispatch
    ;; implied ret

    ;; Refresh file header
refresh_file_header:
    ld ix,DOS_REF_HEAD
    jr dos_call_dispatch
    ;; implied ret

    ;; ================================================================
    ;; Clear Lower Screen (bottom 2 lines)
    ;; Calls Spectrum ROM routine via interop
    ;; ================================================================
cls_lower:
    xor a
    ld hl,CLSLOWER
call48rom:
    jp CALL_48ROM
    ;; implied ret
    
open_channel:
    ld hl,CHANOPEN
    jr call48rom
    ;; implied ret
    
    ;; ================================================================
    ;; Error Handler (0x1C72)
    ;; Routes disk/tape I/O errors by code stored at 0x2007
    ;; Error codes: 0x22=file too big, 0x01=write protect, 0x17=not found,
    ;;              0x05=drive not ready, 0x00=no error, 0x0C=tape save error
    ;; Restores compression state, displays message, waits for key,
    ;; then dispatches to appropriate recovery routine.
    ;; ================================================================
error_handler:
    di    
    ld (02007h),a    ; Store error code at 0x2007
    push af    
    call set_bank0_rom3
    call decompress_screen_if_needed
    call check_tape_or_disk
    jr nz,eh_post_tape_check
    call scan_ay_registers
eh_post_tape_check:
    ;; Entry point after tape/disk mode check;
    ;; reset compression state and pop error code
    call reset_compression_state
    pop af    
    ld hl,MF3_BANK_FLAGS
    bit MF3_BANK_FLAGS_TAPE_MODE,(hl)
    jr z,eh_disk_error_dispatch
    cp 00ch
    jr z,eh_reenter_save
    jr eh_show_generic_error
eh_disk_error_dispatch:
    ;; Dispatch on disk error codes: 0x22, 0x01, 0x17, 0x05, 0x00
    cp 022h
    jr z,eh_file_too_big
    cp 001h
    jr z,eh_write_protected
    cp 017h
    jr z,eh_file_not_found
    cp 005h
    jr z,eh_drive_not_ready
    or a    
    jr z,eh_drive_not_ready
eh_show_generic_error:
    ;; Display generic I/O error message with beep
    call set_bank0_rom3
    call cls_lower
    call print_io_error
    call CALL_BEEPKEY
eh_check_recovery_action:
    ;; Check ADDR_PTR bit 0 (DOS_SUBMENU): if set, wait for SPACE key; else reenter save
    ld a,(MF3_ADDR_PTR)
    bit MF3_ADDR_PTR_DOS_SUBMENU,a
    jp nz,check_space_key
eh_reenter_save:
    ;; Tape error 0x0C or fallback: return to save/load options menu
    jp reenter_save_options

eh_file_not_found:
    ;; Error 0x17: display "file not found" message
    call print_abort2
    ld hl,msg_file_not_found
    ld b,010h
    jr printf_wenter

eh_drive_not_ready:
    ;; Error 0x05 or 0x00: display "drive not ready" message
    call print_abort2
    ld hl,msg_drive_not_ready
    ld b,011h
    jr printf_wenter
    
eh_write_protected:
    ;; Error 0x01: display "Write protected" message
    call print_abort2
    ld hl,msg_write_prot+1
    ld b,011h
    
printf_wenter:    

    call printf
    call print_enter

eh_wait_for_key:
    ;; Wait for keypress: no key=continue, 'A'=abort to recovery check
    call get_key_wait
    jr z,eh_dispatch_by_context
    cp 041h
    jp z,eh_check_recovery_action
    jr eh_wait_for_key
eh_dispatch_by_context:
    ;; Choose recovery target based on ADDR_PTR bits (bit 5=disk load, bit 0=catalog)
    ld hl,disk_load_entry
    ld a,(MF3_ADDR_PTR)
    bit MF3_ADDR_PTR_DISK_LOAD_RETRY,a
    jr nz,eh_enter_basic
    ld hl,dos_catalog_entry
    bit MF3_ADDR_PTR_DOS_SUBMENU,a
    jr nz,eh_catalog_delete_recovery
    ld hl,save_start_disk
eh_enter_basic:
    ;; Default recovery: return to BASIC interpreter
    jp enter_basic_dispatch
eh_catalog_delete_recovery:
    ;; Catalog mode: if error was 0x00, go to BASIC; else attempt delete
    ld a,(02007h)
    or a    
    jr z,eh_enter_basic
    jp dos_catalog_delete_selected
eh_file_too_big:
    ;; Error 0x22: display " K 1" prefix, KB values, and "File too big" message
    call print_abort
    ld a,020h
    rst 30h    
    ld a,020h
    rst 30h    
    ld a,012h
    rst 30h    
    ld a,001h
    rst 30h    
    ld hl,(02024h)
    srl h
    rr l
    jr nc,eh_print_kb_value
    inc hl    
eh_print_kb_value:
    ;; Print the free-space KB value (rounded up) using decimal converter
    call 01d4eh
    ld hl,print_number_format
    ld b,005h
    call printf
    ld hl,(02022h)
    call 01d4eh
    ld a,04bh
    rst 30h    
    ld a,012h
    rst 30h    
    xor a    
    rst 30h    
    call print_two_spaces
    ld hl,msg_file_too_big
    ld b,00eh
    jr printf_wenter
print_number_format:
    ;; Printf format data for KB display; also contains number-print subroutines.
    ;; Passed as HL parameter to printf for the file-too-big message.
    ld c,e    
    jr nz,printer_copy_esc_loop
    ld l,a    
    jr nz,print_num_advance
    rra    
    jr nz,$-41
    call convert_to_decimal_p2
    pop hl    
    ld b,003h
print_number_init:
    ;; Initialize leading-zero counter C=0; also called from toolkit display
    ld c,000h
print_num_check_digit:
    ;; Load next ASCII digit; check if it is '0' (0x30) for leading-zero suppression
    ld a,(hl)    
    cp 030h
    jr z,print_num_leading_zero
print_num_output_char:
    ;; Output the character in A via RST 30h (printa)
    rst 30h    
print_num_advance:
    ;; Advance to next digit; loop until B digits processed
    inc hl    
    inc c    
    djnz print_num_check_digit
    ret    
print_num_last_zero_check:
    ;; If this is the last digit position, must print '0' instead of suppressing
    ld a,001h
    cp b    
    jr z,print_num_zero_digit
    ld a,020h
    jr print_num_output_char
print_num_leading_zero:
    ;; Leading zero detected: undo counter increment and decide whether to suppress
    dec c    
    ld a,0ffh
    cp c    
    jr z,print_num_last_zero_check
    inc c    
print_num_zero_digit:
    ;; Break leading-zero suppression: output ASCII '0' (0x30)
    ld a,030h
    jr print_num_output_char

    ;; ================================================================
    ;; Memory Checksum Utility
    ;; XORs all bytes from 0x2000 down to 0x0000
    ;; Displays result for verification purposes
    ;; Press SPACE to exit
    ;; ================================================================
memory_checksum_utility:
    call cls_print_msg
    call setup_display
    ld hl,msg_checksum_title
    call print_msg_len9
    ld hl,MF3_ENTRY_VECTOR
    xor a
    ex af,af'
checksum_loop:
    ;; XOR accumulation loop: walks HL from 0x2001 down to 0x0000
    ex af,af'
    dec hl
    xor (hl)  ; XOR accumulate
    ex af,af'
    ld a,l
    or h
    jr nz,checksum_loop
    ex af,af'
    call print_byte  ; Display result

checksum_wait_space:
    ;; Poll for SPACE key to exit checksum display
    call spcscan
    jp nc,bank_switch_routine  ; Exit on SPACE press

    jr checksum_wait_space
init_printer_config:
    ld a,(MF3_STATUS_FLAGS)
    and MF3_STATUS_FLAGS_TOOL_MODE_MASK
    ld (MF3_STATUS_FLAGS),a
    ld hl,(02015h)
    ld bc,(0x004B)
    and a    
    sbc hl,bc
    jr nz,printer_config_defaults
    ld a,(02017h)
    ld hl,msg_write_prot
    cp (hl)    
    ret z    
    ;; ================================================================
    ;; Printer Configuration Initialization
    ;; Sets up EPSON-compatible printer control parameters
    ;; Configurable via POKE from toolkit (see manual section 4.1)
    ;; Memory locations:
    ;;   0x2008 - Print mode (01=Large+CRLF, F1=Shaded+CRLF, 00=Large+CR, F0=Shaded+CR)
    ;;   0x200B - Left margin (0-255)
    ;;   0x200C - Bottom margin (0-23)
    ;;   0x200D - Top margin (0-23)
    ;;   0x200E-0x2010 - ESC sequence for line spacing (27,51,23 = ESC "3" n)
    ;;   0x2011-0x2014 - ESC sequence for graphic mode (27,76,0,3 = ESC "L" n m)
    ;; ================================================================
printer_config_defaults:
    ;; Initialize printer config from ROM defaults (0x004B ESC sequences)
    ld hl,0x004B
    ld de,02015h
printer_copy_esc_loop:
    ;; Copy 3-byte ESC sequence from ROM to printer config area (LDIR)
    ld bc,0003h
    ldir
    ld a,001h
    ld (MF3_PRINT_MODE),a  ; Set to Large Copy + CR+LF
    xor a
    ld (MF3_STATUS_FLAGS),a  ; Clear status flags
    ld (MF3_PRINT_TOP_MARGIN),a  ; Top margin = 0
    ld a,008h
    ld (MF3_PRINT_LEFT_MARGIN),a  ; Left margin = 8
    ld a,017h
    ld (MF3_PRINT_BOTTOM_MARGIN),a  ; Bottom margin = 23
    ld a,020h
    ld (02009h),a
    ld hl,0x0042
    ld de,MF3_PRINT_ESC_SEQ  ; Copy ESC sequences for printer
    ld bc,0007h
    ldir
    ret    
    ;; ================================================================
    ;; Screen Print/Dump System
    ;; Dumps Spectrum screen to printer in hi-res format
    ;; Processes screen row by row with pixel conversion
    ;; Compatible with ZX Printer and compatible devices
    ;; ================================================================
screen_print_main:
    call print_prep
    call print_linefeed
    call print_top_margin
    ld c,000h
screen_print_row_loop:
    ;; Main loop: process one screen row at a time
    push bc
    call print_prep
    call print_row_spacing  ; Process row
    call print_row_data  ; Output row
    pop bc
pixel_proc_outer:
    ;; Outer loop: iterate over pixel columns (C=0..255)
    push bc
    ld d,004h
pixel_4group_inner:
    ;; Inner loop: process 4-pixel groups within a column
    push bc
    push de
    ld a,b
    call calc_screen_addr  ; Process pixels
    pop de
    ld b,a
    inc b
    ld a,(hl)
pixel_bit_rotate:
    ;; Bit rotation loop: rotate 8 times to extract pixel data
    rlca
    djnz pixel_bit_rotate
    push af
    rl e
    pop af
    rl e
    pop bc
    inc b
    dec d    
    jr nz,pixel_4group_inner
    ld a,(MF3_PRINT_MODE)
    and 0f0h
    cp 0f0h
    jr nz,text_mode_triple_char
    ld a,e    
    call print_graphics_byte
    jr print_next_column
text_mode_triple_char:
    ;; Text mode: output character 3 times for triple-width effect
    ld a,e    
    call print_char_to_printer
    call print_char_to_printer
    call print_char_to_printer
print_next_column:
    ;; Advance to next print column; wrap at column 256
    inc c    
    ld a,c    
    jr z,print_row_done
    pop bc    
    ld c,a    
    jr pixel_proc_outer
print_row_done:
    ;; End of row: send linefeed, check if bottom margin reached
    pop de    
    call print_linefeed
    call calc_row_height
    cp b    
    jr c,print_end_row
    jr screen_print_row_loop
    ;; ----------------------------------------------------------------
    ;; print_prep - Screen Print Preparation
    ;; Initializes printer control codes
    ;; HL = 0x200E, B = 3
    ;; ----------------------------------------------------------------
print_prep:
    ld hl,MF3_PRINT_ESC_SEQ
    ld b,003h
    jr esc_send_loop

    ;; ----------------------------------------------------------------
    ;; print_row_data - Print Row Data
    ;; Outputs row data to printer
    ;; HL = 0x2011, B = 4
    ;; ----------------------------------------------------------------
print_row_data:
    ld hl,MF3_PRINT_GRAPHIC_ESC
    ld b,004h
    jr esc_send_loop
print_end_row:
    ;; All rows printed: send end-of-row codes and terminate print
    ld c,008h
    ld b,002h
    ld hl,printer_esc_end_row
    push bc    
    ld b,000h
    add hl,bc    
    pop bc    
esc_send_loop:
    ;; Send B ESC-sequence bytes from HL to the printer
    ld a,(hl)    
    call print_char_to_printer
    inc hl    
    djnz esc_send_loop
    ret    
    ;; ----------------------------------------------------------------
    ;; calc_screen_addr - Pixel Processing
    ;; Calculates screen memory address from pixel coordinates
    ;; Input: A = row, C = column
    ;; Output: HL = screen address, A = bit position
    ;; Uses Spectrum screen layout formula
    ;; ----------------------------------------------------------------
calc_screen_addr:
    ld b,a
    and a
    rra
    scf
    rra
    and a
    rra
    xor b
    and 0f8h
    xor b
    ld h,a
    ld a,c
    rlca
    rlca
    rlca
    xor b
    and 0c7h
    xor b
    rlca
    rlca
    ld l,a
    ld a,c
    and 007h
    ret

    ;; ----------------------------------------------------------------
    ;; print_row_spacing - Row Processing
    ;; Outputs spacing before row data
    ;; Reads count from 0x200B and outputs spaces
    ;; ----------------------------------------------------------------
print_row_spacing:
    ld a,(MF3_PRINT_LEFT_MARGIN)
    cp 000h
    ret z
    push bc
    ld b,a
print_margin_space_loop:
    ;; Output left margin spaces (B iterations)
    ld a,020h
    call print_char_to_printer
    djnz print_margin_space_loop
    pop bc
    ret

    ;; ----------------------------------------------------------------
    ;; calc_row_height - Calculate Row Height
    ;; Multiplies value at 0x200C by 7
    ;; Used for printer row spacing calculations
    ;; ----------------------------------------------------------------
calc_row_height:
    ld a,(MF3_PRINT_BOTTOM_MARGIN)
    inc a

MULT_BY7:  ;multiply by 7
    add a,a
    add a,a
    add a,a
    dec a
    ret
    
print_top_margin:
    ld a,(MF3_PRINT_TOP_MARGIN)

    or a    
    ret z
    
    call MULT_BY7

    inc a        
    ld b,a    

    ret    
printa:
    push af
    
    ;; call RST10 in the spectrum rom via interop
    call CALL_RST10

    ld a,(MF3_ADDR_PTR)

    bit MF3_ADDR_PTR_PRINT_LINE_COUNT,a
    jr nz,printa_line_counting
    pop af    

    ret
    
printa_line_counting:
    ;; Printer echo mode: count characters per line, auto-linefeed
    pop af    
    cp 00dh
    jr z,print_linefeed
    cp 020h
    ret c    
    cp 07fh
    ret nc    
    push af    
    push hl    
    ld hl,0200ah
    ld a,(hl)    
    dec hl    
    cp (hl)    
    call nc,print_linefeed
    inc hl    
    inc (hl)    
    pop hl    
    pop af    
    jr print_char_to_printer
print_linefeed:
    xor a    
    ld (0200ah),a
    ld a,00dh
    call print_char_to_printer
    ld a,(MF3_PRINT_MODE)
    bit 0,a
    ret z    
    ld a,00ah
    
print_char_to_printer:
    push bc    
    push af    
    ld bc,BANK2
    ld a,(BANK678)
    or 010h
    out (c),a
    ld (BANK678),a
    ld bc,00ffdh
printer_wait_ready:
    ;; Poll printer BUSY flag (bit 0); also check for BREAK key
    ;; check for the break  key
    call brkscan
    jr nc,printer_break_abort

    in a,(c)
    bit 0,a
    jr nz,printer_wait_ready
    pop af    
    push af    
    out (c),a
    ld bc,BANK2
    ld a,(BANK678)
    and 0efh
    out (c),a
    or 010h
    out (c),a
    pop af    
    pop bc    
    ret    
printer_break_abort:
    ;; BREAK pressed during print: display error, then return to menu or toolkit
    pop af    
    pop bc    

    ld hl,MF3_ADDR_PTR
    res MF3_ADDR_PTR_PRINT_LINE_COUNT,(hl)
    xor a    
    ld (0200ah),a
    push hl    

    call cls_lower
    call print_mf_attributes

    ld hl,msg_printer_err
    ld b,00fh
    call printf
    
    ld hl,msg_any_key
    ld b,00fh

    call printf
    call get_key_wait

    pop hl    
    bit MF3_ADDR_PTR_PRINT_EXIT_TO_TOOLKIT,(hl)
    jp z,bank_switch_routine
    jp toolkit_reset_state

msg_printer_err:
    defb "Printer Error -"

print_graphics_byte:
    ld a,e    
    call calc_screen_address
    push bc    
    push ix
    ld b,003h
graphics_3pass_loop:
    ;; Outer loop: 3 passes per pixel byte (attribute colors)
    push bc    
    push af    
    ld b,004h
graphics_4bit_loop:
    ;; Inner loop: process 4 bits per pass, expanding each to 2 output bits
    push bc    
    rla    
    jr c,graphics_alt_pattern
    ld ix,(MF3_IX_SAVE)
graphics_pattern_shift:
    ;; Shift pattern table bits into output byte E (2 bits per source bit)
    ld d,(ix+000h)
    rl d
    rl e
    rla    
    rl d
    rl e
    pop bc    
    djnz graphics_4bit_loop
    ld a,e    
    call print_char_to_printer
    pop af    
    pop bc    
    ld ix,(MF3_IX_SAVE)
    inc ix
    ld (MF3_IX_SAVE),ix
    ld ix,(MF3_IX_SAVE2)
    inc ix
    ld (MF3_IX_SAVE2),ix
    djnz graphics_3pass_loop
    pop ix
    pop bc    
    ret    
graphics_alt_pattern:
    ;; Use alternate pattern table entry (from IX_SAVE2) for inverse shading
    ld ix,(MF3_IX_SAVE2)
    jr graphics_pattern_shift
calc_screen_address:
    push af    
    push bc    
    dec b    
    srl b
    srl b
    srl b
    ld a,b    
    srl c
    srl c
    srl c
    ld b,c    
    rrc a
    rrc a
    rrc a
    ld c,a    
    and 0e0h
    xor b    
    ld l,a    
    ld a,c    
    and 003h
    xor 058h
    ld h,a    
    ld a,(hl)    
    push af    
    srl a
    srl a
    srl a
    and 007h
    call get_pattern_table_ptr
    ld (MF3_IX_SAVE),hl
    pop af    
    and 007h
    call get_pattern_table_ptr
    ld (MF3_IX_SAVE2),hl
    pop bc    
    pop af    
    ret    
get_pattern_table_ptr:
    ld hl,pattern_table_3byte
    ld c,a    
    add a,a    
    add a,c    
    ld c,a    
    ld b,000h
    add hl,bc    
    ret    
pattern_table_3byte:
    defb 0c0h,0c0h,0c0h
    defb 080h,0c0h,080h
    defb 080h,080h,080h
    defb 000h,0c0h,000h
    defb 040h,080h,000h
    defb 000h,080h,080h
    defb 000h,080h,000h
    defb 000h,000h,000h

ztest:  equ int_call_48rom-interop_start
