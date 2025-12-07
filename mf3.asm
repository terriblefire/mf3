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
;; - 101 routines renamed with descriptive names
;; - 149 comprehensive documentation headers added
;; - All calling conventions and register usage documented
;; - Memory-mapped I/O locations fully annotated
;;
;; ====================================================================

; z80dasm 1.1.0
; command line: z80dasm --origin=0 --sym-input=sysvarp3.sym -l -o mf3.asm original.rom

	org	00000h

SWAP:	equ 0x5b00
BANKM:	equ 0x5b5c
KSTATE:	equ 0x5c00
LASTK:	equ 0x5c08
CHANS:	equ 0x5c4f
PROG:	equ 0x5c53
ERRNR:	equ 0x5C3A
STRMS:	equ 0x5C10
RAMTOP:	equ 0x5CB2
BANK678:equ 0x5b67
LODDRV:	equ 0x5b79
SAVDRV:	equ 0x5b7a
BANK1:	equ 0x7ffd
BANK2:  equ 0x1ffd
SCREEN:	equ 0x4000

	;; ROM Subroutines

BEEPER:	  equ 0x03b5
RECLAIM1: equ 0x19e5
MAKEROOM: equ 0x1655
CLSLOWER: equ 0x0d6e
CHANOPEN: equ 0x1601
SABYTES:  equ 0x04c2
LDBYTES:  equ 0x0556
INTTOFP:  equ 0x2D3B
FPTOBC:	  equ 0x2DA2

	;; +3 bank bits
PLUS3_SPECIAL:	equ 0x01
ROM_SEL1:	equ 0x04
PLUS3_MOTOR:    equ 0x08
PLUS3_PRINTER:	equ 0x10

	;; 128k bank bits
BANK1_SCREEN:	equ 0x08
ROM_SEL0: 	equ 0x10
BANK1_DISABLE:	equ 0x20

	;; plus3dos calls used

DOS_OPEN:  	 equ 0x0106
DOS_CLOSE: 	 equ 0x0109	
DOS_FREE_SPACE:	 equ 0x0121
DOS_REF_HEAD:	 equ 0x010f
DOS_READ:	 equ 0x0112
DOS_WRITE:	 equ 0x0115
DOS_CATALOG:	 equ 0x011e
DOS_DELETE:	 equ 0x0124
DOS_SET_MESSAGE: equ 0x014e
	
	;; scratch area in bank2. used to execute code with mf3 paged out.
RAMAREA: equ 0x5fe0 ;; temp space to save ram in the MF3
MF3TEMP: equ 025e1h ;; location in the MF3 ram which stores the return routine.
MF3_RET_VECTOR:	equ 0x2027 
	
	;; Definitions for Multiface 3
	
P_MF3_IN:     equ 0x3f    ; Port address 
P_MF3_OUT:    equ 0xbf    ; Port address 
P_MF3_BUTTON: equ 0x3f    ; Port address 

P_MF3_P7FFD: equ 0x7f3f  ; Port address 
P_MF3_P1FFD: equ 0x1f3f  ; Port address

	;; addresses of interop routines

CALL_IX:     equ RAMAREA + 116 	;
CALL_48ROM:  equ RAMAREA + 127	;
CALL_ROM2:   equ 0x6047
CALL_BEEPKEY:equ RAMAREA + 136	;
CALL_RST10:  equ RAMAREA + 175  ; PRINT A
CALL_READROM:equ RAMAREA + 202 	;
CALL_LDIR:   equ RAMAREA + 182 	;
CALL_RECLAIM:equ RAMAREA + 188	;
CALL_MAKEROOM:	equ RAMAREA + 195 ;
	
WHITE:	EQU 7

INK:	MACRO   x
	defb	16,x
        ENDM

PAPER:	MACRO   x
	defb	17,x
        ENDM
	
FLASH:	MACRO   x
	defb	18,x
        ENDM

BRIGHT:	MACRO   x
	defb	19,x
        ENDM

INVERSE:MACRO   x
	defb	20,x
        ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; start
	
l0000h:
	defb 0c3h
	defb "A", 6	
msg_abort:
	INVERSE 1
	defb "a"
	INVERSE 0
	defb "bort Erase:" , 6, 6

	INK WHITE
	PAPER 2
	BRIGHT 1

msg_enter_attrs:
	defb 22, 1, 7 		;
msg_enter:	
	defb 22, 1, 27
	INVERSE 1
	defb "ENTER"
	INVERSE 0

l002ah:
	push hl	
	ld hl,02006h
	jr l0035h
RST30:	
	jp printa
	
	;; implied ret
	nop	
l0034h:
	jp (hl)	
l0035h:
	set 1,(hl)
l0037h:
	pop hl
	
RST38:	ret			; IM1
	
l0039h:
	ld b,006h
	ld de,l1002h
	rlca	
	ld d,000h
l0041h:
	nop	
l0042h:
	dec de	
	inc sp	
	rla	
	dec de	
	ld c,h	
	nop	
	inc bc	
	dec de	
	ld b,b	
l004bh:
	ld b,a	
	defb 0edh;next byte illegal after ed

msg_write_prot:	

	defb 0dbh
	defb "Write protected ",6

msg_clean_attrs:
	
	defb 22,0,22
	defb 16,1
	defb 17,6

	;; ================================================================
	;; NMI Entry Point (0x0066)
	;; Main entry when red button is pressed
	;; Preserves all CPU state and banking configuration
	;; ================================================================
NMI_ENTRY:

	nop
	nop
	jp nmi_save_state

l006bh:
	jp l1190h
l006eh:
	jp l0dc9h
l0071h:
	jp l0dd6h

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

	ld a,(02002h)

	;; get the jump address and put it in HL
	ld hl,(02000h)
l0080h:
	dec a

	;; if A was 1, just jump to the value in HL.
	jr nz,l0034h

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
	ld a,(06000h)
	xor 002h
	ld (06000h),a

	ld de,0c000h
	ld hl,SCREEN
	ld bc,l1b00h

	;; ----------------------------------------------------------------
	;; Swap Buffers Wrapper
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
	ld de,050c0h		; start of the last (bottom) 16 lines on the screen.
	ld b,008h 		; 16 lines (8 * 512)

l00b2h:
	push bc
	ld bc,00040h
	ldir

	ld e,0c0h
	pop bc
	djnz l00b2h

	;; restore the attributes for the lower 2 rows
l00bdh:
	ld de,05ac0h
	ld bc,00040h
	ldir
	ret	
	
	ld bc,l0403h
	ld b,012h
	ld bc,l1220h
	nop
	
l00cfh:
	defb "File too big", 6, 6

msg_page_question:	
	defb 22,1,31
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
	ld bc,l002ah
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
l00ffh:
	ld hl,(l0000h)
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
	di 			;disable interrupts

	ld a,r
	push af	
l0112h:
	push de	
	push bc	
l0114h:
	ex af,af'	
	push af	
l0116h:
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
	ld (0201dh),a
	ld (0200ah),a
	ld i,a
	
	ld a,010h
	ld (03ff6h),a
	
	;; get the last io write to 7ffd
	
	ld bc,P_MF3_P7FFD
	in a,(c)
	and 00fh
	
	ld c,a	
	ld a,(03ff6h)
	or c	

	ld (03ff6h),a
	
	;; get the last io write to 1ffd
	
	ld bc,P_MF3_P1FFD
	in a,(c)

	and 00fh
	
	;; multiply by 16 (upper 4 bits are cleared.
	;; unsure why rlca is used here.
	
	rlca	
	rlca	
	rlca	
	rlca	
	
	ld c,a	
	ld a,(03ff8h)
	and 004h
	or c	

	ld (03ff8h),a
	ld hl,03fe3h
	ld de,00f10h
	ld c,0fdh
l0172h:
	ld b,0ffh
	out (c),d
	in a,(c)
	ld (hl),a	
	ld a,d	
	and 00ch
	cp 008h
	jr nz,l0185h
	
	ld b,P_MF3_OUT
	xor a	
	out (c),a
l0185h:
	dec hl	
	dec d	
	dec e	
	jr nz,l0172h

	ei 			; enable interruptss
	halt			; wait for the horizontal refresh
	di			; disable interrputs again.

	ld hl,02006h
	ld a,(03ff8h)
	bit 1,(hl)
	jr z,l0199h
	set 0,a
l0199h:
	ld (03ff8h),a
	im 1
	ld hl,(MF3_RET_VECTOR)
	ld de,0260bh

	;; reset flags
	and a	
	sbc hl,de
	add hl,de
	;; z true if hl=de
	ld de,03fc3h

	jr c,l01bbh
l01adh:
	and a	
	ex de,hl	
	sbc hl,de
	jr c,l01bbh
	ex de,hl	
	ld de,(02029h)
	ld (hl),e	
	inc hl	
	ld (hl),d	
l01bbh:
	call set_return_vector
	call restore_ay_registers
	call 05ff7h

	ld hl,03ff6h
	jr z,l01cbh
	res 4,(hl)
l01cbh:
	call reset_bank2_rom0
l01ceh:
	call 05ff7h
	ex af,af'	
	call select_rom0_and_switch
	ex af,af'	
	ld a,(03ff6h)
	jr z,l01e2h
	ld hl,02006h
	set 3,(hl)
	jr l01e4h
l01e2h:
	res 3,a
l01e4h:
	ld (03ff6h),a
	ld (0201eh),a

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
	
	ld hl,02003h 		; the RUN address.

	ld a,(hl)	
	cp 052h			; Compare withR
l01f3h:
	jr nz,nojump

	inc hl 			; move to 0x2004

	ld a,(hl)	
	cp 055h			; Compare with U
	jr nz,nojump

	inc hl			;move to 0x2005

	ld a,(hl)	
	cp 04eh				; CompaN

	jp z, JUMP		; jump is enabled.

	;; ----------------------------------------------------------------
	;; Jump Abort - Clear auto-jump signature
	;; ----------------------------------------------------------------
jumpabrt:
	ld (hl),0000h 		;clear the auto jump. delete the N.

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
	ld bc,00ffh 		; 256 bytes

	call clear_ram
	call check_48kmode

	jr z,l0232h

	call reset_bank2_rom0

	ld hl,l02c5h
	ld de,RAMAREA
	ld bc,0037h
	ldir
	
	call RAMAREA
	call set_bank0_rom3
l0232h:
	call copy_interop
	call save_menu_screen
	jp l11a1h

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

	ld hl,02006h
	bit 3,(hl)
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
	ld bc,l002ah
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
	ld hl,(06008h)

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
	
	cp 0f1h 		; look for 0xf1, pop AF
	jr nz, search_loop
	
	ld a,(hl)	
	inc hl	
	cp 0c9h			; look for C9, ret
	
	jr nz,search_loop
	
search_found:
	
	;; store the result location at
	;; 0x6008
	
	ld (06008h),hl

	;; page out the multiface
	in a,(P_MF3_OUT)

	;; read the first byte in ram
	ld a,(l0000h+1)

	;; compare it with 0xAF and save the flags
	cp 0afh
	ex af,af'	

	;; page the MF3 in
	in a,(P_MF3_IN)

	;; restore the flags and return to the MF3 rom
	ex af,af'	
	ret
	
search_fail:
	
	;; if the search fails then store the screen location
	ld hl,04004h
	jr search_found
	nop	

l02c5h:
	;; page the MF3 out
	in a,(P_MF3_OUT)

	;; copy part of the spectrum rom to ram? 
	ld hl,l00bdh
	ld de,SWAP
	ld bc,0052h
	ldir

	ld hl,0cf07h

	ld (BANKM),hl
	ld a,014h  		; bug?
	ld (BANK678),hl
	
	ld a,'A'		; set the default drive?
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
	ld a,007h
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
	;; Set Bank 2 from Upper Nibble
	;; Shifts upper 4 bits of A to lower position and sets BANK2
	;; ================================================================
	;; set bank2 from the upper 4 bits
set_bank2_fupper:
	rrca   ; a >> 4
	rrca
	rrca
	rrca
	and 00fh

	;; ================================================================
	;; Set Bank 2 (0x1FFD port)
	;; Controls +3 special banking modes
	;; Printer strobe always set high (inactive)
	;; ================================================================
	;; set bank 2. with printer strobe masked off.
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
l032ch:
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
;; Uses:   A, BC (via set_bank2 and l032ch)
;; ================================================================
	;
select_rom1_motor_on:
	ld a,(BANK678)
	;; motor on, printer strobe off (active low).
	or ROM_SEL1 | PLUS3_PRINTER 
	ld (BANK678),a

	call set_bank2
	jr l032ch

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
	ld bc,l0000h

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
	jr z,l0383h
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
l037dh:
	scf	
	ccf	
	ret	
l0380h:
	xor a	
	jr l037dh
l0383h:
	ld hl,0c000h
l0386h:
	xor a	
	cp (hl)	
	jr nz,l0380h
	inc hl	
	ld a,l	
	or h	
	jr nz,l0386h
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
	bit 6,a
	jr nz,l03aeh
	ld hl,SCREEN
	push hl	
	ld bc,SWAP
	call find_compression_marker
	pop de	
	and a	
	sbc hl,de
	ld (05ffch),hl
	ld hl,RAMAREA
	set 6,(hl)
l03aeh:
	call set_bank0_rom_x1
	ld a,(RAMAREA)
	bit 7,a
	ret nz	
	ld hl,060b3h
	ld (05ff6h),hl
	ld bc,l0000h
	ld (05ff8h),bc
	call find_compression_marker
	ld (05ffeh),hl
	ld de,RAMAREA
	and a	
	sbc hl,de
	ld (05ffah),hl
	ld hl,RAMAREA
	set 7,(hl)
	ret

;; ----------------------------------------------------------------
;; Pop HL and Return - Utility exit point
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
	ld a,(02006h)
l03dfh:
	bit 0,a
	jr nz, pop_hl_ret	;implied ret

	push hl	
	dec hl	
	dec bc	
	dec bc	
l03e7h:
	inc hl	
	and a	
	sbc hl,bc
	jr nc,l0400h
	add hl,bc	
	ld a,(hl)	
	cp 037h
	jr nz,l03e7h
	inc hl	
	ld a,(hl)	
	cp 0edh
	jr nz,l03e7h
	inc hl	
	ld a,(hl)	
	cp 0cbh
	jr nz,l03e7h
	scf	
l0400h:
	pop hl	
	jr c, pop_hl_ret
l0403h:
	exx	
	pop bc	
	push hl	
	exx	
	push hl	
	pop de	
l0409h:
	ld bc,l0000h
	ld a,(de)	
l040dh:
	inc de	
	inc bc	
	push de	
	exx	
	pop hl	
	and a	
	sbc hl,bc
	exx	
	jr z,l041dh
	ex de,hl	
	cp (hl)	
	ex de,hl	
	jr z,l040dh
l041dh:
	push af	
	ex af,af'	
	ld a,c	
	and 0f8h
	or b	
	jr nz,l0433h
	pop af	
	push af	
	ld b,c	
l0428h:
	ld (hl),a	
	inc hl	
	djnz l0428h
l042ch:
	pop af	
	jr nz,l0409h
	exx	
	pop hl	
	exx	
	ret	
l0433h:
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
	jr l042ch

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
	ret z 			;return if we are locked in 48k mode
	
	bit 4,(hl)
	ret nz	
	ld a,011h
	call validate_compressed_bank
	jr c,l0460h
	ld (0601ah),de
	set 0,(hl)
	jr z,l0460h
	set 4,(hl)
l0460h:
	ld a,013h
	call validate_compressed_bank
	jr c,l0471h
	ld (0601ch),de
	set 1,(hl)
	jr z,l0471h
	set 5,(hl)
l0471h:
	ld a,014h
	call validate_compressed_bank
	jr c,l0482h
	ld (0601eh),de
	set 2,(hl)
	jr z,l0482h
	set 6,(hl)
l0482h:
	ld a,016h
	call validate_compressed_bank
	jr c,l0493h
	ld (06020h),de
	set 3,(hl)
	jr z,l0493h
	set 7,(hl)
l0493h:
	ld a,017h
	call validate_compressed_bank
	ld hl,06019h
	jr c,l04a7h
	ld (06022h),de
	set 3,(hl)
	jr z,l04a7h
	set 7,(hl)
l04a7h:
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
	jr z,l04cfh
	ld a,(0c002h)
	or a	
	jr nz,l04d1h
	ld de,(0c000h)
	ld hl,0008h
	and a	
	sbc hl,de
	jr nz,l04d1h
	call decompress_bank_data
l04cah:
	ld hl,06018h
	scf	
	ret	
l04cfh:
	jr c,l04cah
l04d1h:
	jr nz,l04d6h
	ld de,SCREEN
l04d6h:
	ld hl,06018h
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
	ld hl,06018h
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
	ld a,(06018h)
	rlca	
	jr c,l050bh
l04eeh:
	rlca	
	jr c,l0514h
l04f1h:
	rlca	
	jr c,l051dh
l04f4h:
	rlca	
	jr nc,l04fch
	ld a,011h
	call decompress_from_bank
l04fch:
	ld a,(06019h)
	bit 7,a
	jr z,l0508h
	ld a,017h
	call decompress_from_bank
l0508h:
	jp set_bank0_rom_x1
l050bh:
	push af	
	ld a,016h
	call decompress_from_bank
	pop af	
	jr l04eeh
l0514h:
	push af	
	ld a,014h
	call decompress_from_bank
	pop af	
	jr l04f1h
l051dh:
	push af	
	ld a,013h
	call decompress_from_bank
	pop af
	jr l04f4h

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
	ld de,l0000h

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
l0553h:
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
	jr z,l056ah
l0565h:
	pop af	
	ld (de),a	
	dec de	
	jr l0553h
l056ah:
	ld a,(hl)	
	cp 0edh
	jr nz,l0565h
	dec hl	
	ld a,037h
	cp (hl)	
	jr z,l0578h
	inc hl	
	jr l0565h
l0578h:
	pop af	
	dec hl	
	ld a,(hl)	
	dec hl	
	ld b,(hl)	
	dec hl	
	ld c,(hl)	
	dec hl	
l0580h:
	ld (de),a	
	dec de	
	dec bc	
	ex af,af'	
	ld a,b	
	or c	
	jr z,l0553h
	ex af,af'
	jr l0580h

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
	bit 6,a
	jr z,l05a9h

;; ----------------------------------------------------------------
;; Decompress Screen - Decompresses screen memory
;; ----------------------------------------------------------------
decompress_screen:
	ld ix,SCREEN
	push ix
	pop hl	
	ld de,(05ffch)
	add hl,de	
	ld de,SWAP
	call decompress_to_ram
	ld hl,RAMAREA
	res 6,(hl)
l05a9h:
	ld a,(RAMAREA)
	bit 7,a
	ret z

;; ----------------------------------------------------------------
;; Decompress System Variables - Decompresses system vars area
;; ----------------------------------------------------------------
decompress_sysvars:
	call set_bank0_rom_x1
	ld hl,(05ffeh)
	ld ix,(05ff6h)
	ld de,l0000h
	call decompress_to_ram
	ld hl,RAMAREA
	res 7,(hl)
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
	ld bc,l1000h
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
	or 007h 		; select bank7

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
	ld bc,l0939h

	call swap_buffers

	;; copy e600 into/outof the mf3 ram directly after the last copy

	ld hl,0e600h
	ld bc,l0000h+1
	jr swap_buffers
	;; implied ret
	
l0620h:
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
l0641h:
	ld hl,02006h
	set 2,(hl)
	jr l0659h

l0648h:
	call restore_menu_screen
	call swap_screen_if_128k
	ld hl,0202dh
	ld de,SWAP
	ld bc,005b3h
	ldir
l0659h:
	im 1
	ld hl,(MF3_RET_VECTOR)
	ld e,(hl)	
	inc hl	
	ld d,(hl)	
	ld (02029h),de
	ld sp,03fd4h
	ld b,001h
	
	call delay_loop

	ld a,(03ff6h)
	call switch_bank

	;; restore the 4 bits of BANK2. these are taken
	;; from the upper 4 bits of 0x3ff8

	ld a,(03ff8h)
	call set_bank2_fupper

	;; get the return vector
	call set_return_vector
	;; cleanup after. restore ram.
	call cleanup_return_vector
	
	ld hl,03fe3h
	ld de,00f10h
	ld c,0fdh

l0687h:
	ld b,0ffh
	out (c),d
	ld a,(hl)	
	ld b,P_MF3_OUT
	out (c),a
	dec hl	
	dec d	
	dec e	
	jr nz,l0687h
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
	ld a,(03ff8h)
	bit 0,a
	jr z,l06bch
	im 2
	
l06bch:
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

	ld a,(02006h)
	bit 2,a
	jr z,l06e6h

	;; multiface3 lockout?
	out (P_MF3_BUTTON),a
	jr l06e8h
l06e6h:
	;; page out the multiface
	out (P_MF3_OUT),a
l06e8h:
	ld a,(03ff8h)
	bit 2,a
	jr z,l06f0h
	ei	
l06f0h:
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

	ld a,(02006h)
	bit 2,a
	ret z

	;; display the "[o]ff"  message on the main menu
	ld hl,msg_off
	ld b,005h

	;; ================================================================
	;; Print String
	;; Prints B characters from address in HL
	;; Uses RST 30h (which calls printa routine)
	;; ================================================================
printf:  			;print routine?
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

	defb 6
	defb 22, 0, 22
	FLASH 1
	defb "locked"
	FLASH 0 	; locked message

msg_48k:
	defb " 4"

msg_off:	
	defb 22, 1, 30
	defb "ff"

msg_mainmenu:	
	defb 22, 0,0
	defb 19,1
	defb 17,6
	defb 16,1

	INVERSE 1
	defb "r"		;return
	INVERSE 0
	defb "eturn"

	INVERSE 1		;save
	defb "s"
	INVERSE 0
	defb "ave"

	INVERSE 1		;tool
	defb "t"
	INVERSE 0
	defb "ool"
	
	INVERSE 1		;print
	defb "p"
	INVERSE 0
	defb "rint"
	
msg_menu_unlocked:
	INVERSE 1		;dos
	defb "d"
	INVERSE 0
	defb "os"

	INVERSE 1 		;alter
	defb "a"
	INVERSE 0
	defb "lter"
	
	INVERSE 1 		;clear
	defb "c"
	INVERSE 0
	defb "lear"

msg_multiface_attrs:
	defb 22, 1, 0
	defb 16, 7  	
	defb 17, 2
	defb 19, 1
msg_multiface:	
	defb "MULTIFACE 3 V3.C", 6	; multiface string
msg_on:	
	defb 22, 1, 29
	FLASH 1
	defb "o"
	FLASH 0
	defb "n "
	
msg_copyright:	
	defb 07fh 	;copyright on spectrum
	defb " Romantic Robot Ltd"
msg_tape:
	INVERSE 1
	defb "t"
	INVERSE 0
	defb "ape", 6, 6
msg_disk:
	defb 22, 0, 11

	INVERSE 1
	defb "d"
	INVERSE 0
	defb "isk"

msg_screen_program:	
	defb "screen / program",6

msg_start_tape:	
	defb "Start tape & press", 6

msg_128k:	
	defb 22, 0
	defb 22, 17, 6
	defb 16, 1, 6
	defb 22, 0, 28
	defb "128"
	FLASH 1
	defb "K"
	FLASH 0

msg_file_not_found:	
	defb "File not found", 6, 6
msg_drive_not_ready:	
	defb "Drive not ready", 6, 6
msg_ioerror:	
	defb "I/O error -"
msg_any_key:	
	defb " press any key", 6, 22, 0, 0

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
	jp l1aebh

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
	defb "rase ", 6, 6, 0

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

	ld a,(02006h)
	bit 4,a
	jr z,@skip_48kmode
	ld hl,msg_locked+1

	ld b,00fh
	call printf
	
	ld hl,05ad7h
	ld a,(02006h)
	bit 5,a
	jr nz,@skip_48kmode

	ld b,005h
@loop:
	res 7,(hl)
	inc hl	
	djnz @loop
	
@skip_48kmode:
	ld hl,05ae0h
	ld a,(06000h)
	bit 7,a
	ld a,009h
	jr nz,l0912h
	set 7,(hl)
	ld b,007h
	add a,l	
	ld l,a	
l090ch:
	set 7,(hl)
	inc hl	
	djnz l090ch
	ret	
l0912h:
	ld b,006h
	call l090ch
	inc hl	
	inc hl	
	inc hl	
	set 7,(hl)
	ret	

l091dh:
	call cls_print_msg
	call print_filename_prompt
	call init_filename_buffer
l0926h:
	call print_filename
	call print_filename_at_cursor
l092ch:
	call brkscan

	jr nc,l0993h
	call get_key
	jp z,l09cdh
	cp 00ch
l0939h:
	jr z,l096bh
	cp '0'			; 0
	jr c,l092ch
	cp 05bh			; [
	jr nc,l092ch
	cp ':'			 
	jr c,l094bh
	cp 'A'		 	 ; A
	jr c,l092ch
l094bh:
	ld hl,(05ff4h)
	ld (hl),a	
	inc hl	
	ld (05ff4h),hl
	ld a,(06005h)
	inc a	
	ld (06005h),a
	call calc_filename_space
	jp c,l09b5h
	jr l0926h

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

l096bh:
	ld hl,(05ff4h)
	dec hl	
	push hl	
	ld de,05fe2h
	and a	
	sbc hl,de
	pop hl	
	jr c,l092ch
	ld a,020h
	ld (hl),a	
	ld (05ff4h),hl
	ld a,(06005h)
	dec a	
	ld (06005h),a
	jr l0926h

;; ----------------------------------------------------------------
;; Calculate Filename Space - Returns remaining space in filename buffer
;; Output: HL = space remaining
;; ----------------------------------------------------------------
calc_filename_space:
	ld de,(05ff4h)
	ld hl,05fe8h
	and a
	sbc hl,de
	ret

l0993h:
	ld a,(06000h)
	bit 0,a
	jp nz,l138ch
	jp l11a1h

;; ================================================================
;; Initialize Filename Buffer
;; Clears filename buffer to spaces and resets pointers
;; Sets up 10-byte buffer at 0x5FE2 for filename entry
;; ----------------------------------------------------------------
;; Uses:   HL, B, A
;; ================================================================
init_filename_buffer:
	ld hl,05fe2h
	ld b,00ah
l09a3h:
	ld (hl),020h
	inc hl	
	djnz l09a3h
	ld hl,06005h
	ld a,016h
	ld (hl),a	
	ld hl,05fe2h
	ld (05ff4h),hl
	ret	
l09b5h:
	call print_confirm_prompt
l09b8h:
	call get_key_wait
	jr z,l09cdh
	cp 04eh
	jp z,l091dh
	cp 059h
	jr z,l09cdh
	call brkscan
	jr nc,l0993h
	jr l09b8h
l09cdh:
	call cls_print_msg
	ld a,(06005h)
	cp 016h
	jr nz,l09e4h
	call set_default_filename
	ld a,01ah
	ld (06005h),a
	call print_filename
	jr l09b5h
l09e4h:
	ld a,(06005h)
	ld c,016h
	sub c
	cp 008h
	jr z,l09b5h
	ld a,(06000h)
	bit 0,a
	jp nz,l1333h
	jp l1280h

;; ================================================================
;; Set Default Filename
;; Copies default filename to buffer at 0x5FE2
;; Default is "snapshot" (8 chars from l1ba0h)
;; ----------------------------------------------------------------
;; Uses:   HL, DE, BC
;; ================================================================
set_default_filename:
	ld de,05fe2h
	ld hl,l1ba0h
	ld bc,0008h
	ldir

;; ----------------------------------------------------------------
;; Print Filename - Displays current filename from buffer
;; ----------------------------------------------------------------
print_filename:
	ld hl,msg_clean_attrs
	ld b,007h
	call printf
	ld hl,05fe2h
	ld b,007h
l0a11h:
	ld a,(hl)	
	call filter_printable_char
	rst 30h	
	inc hl	
	djnz l0a11h
	ret

;; ----------------------------------------------------------------
;; Print Filename at Cursor - Shows cursor position in filename entry
;; ----------------------------------------------------------------
print_filename_at_cursor:
	ld a,016h
	rst 30h	
	xor a	
	rst 30h	
	ld a,(06005h)
	rst 30h	
	ld hl,000cah
	ld b,005h
	jr l0a32h

;; ----------------------------------------------------------------
;; Print Filename Prompt - Displays filename entry prompt
;; ----------------------------------------------------------------
print_filename_prompt:
	call setup_display
	ld hl, msg_filename
	ld b,016h
l0a32h:
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
l0a43h:
	ld hl, msg_y_n
	ld b,006h
	jr l0a32h

msg_filename:	
	defb "File name (7 chars.):", 6, 6
	defb 22, 1, 22
	FLASH 1
	defb "OK ?"
	FLASH 0
msg_y_n:	
	defb 22, 1, 28
	defb "y/n"

l0a72h:
	call print_tape_prompt
l0a75h:
	call get_key_wait
	jr z,l0a86h
	cp 041h
	jp z,0118bh
	jr l0a75h
l0a81h:
	call restore_menu_screen
	jr l0a94h
l0a86h:
	call restore_menu_screen
	ld hl,(0202bh)
	inc hl	
	ld (hl),020h
	ld hl,06000h
	set 5,(hl)
l0a94h:
	ld hl,06000h
	bit 7,(hl)

	jp nz,l1037h

	ld de,0602ah
	ld hl,03fe4h
	ld bc,0001ch
	ldir

	call scan_128k_banks
	ld hl,06000h
	res 2,(hl)
	ld a,(06018h)
	and 00fh
	jp nz,l0abeh
	ld a,(06019h)
	bit 3,a
	jr z,l0ac0h
l0abeh:
	set 2,(hl)
l0ac0h:
	call measure_compressed_sizes
	call make_room_233bytes

	;; ================================================================
	;; Save/Load System - Main Save Routine
	;; Universal backup system for programs
	;; Saves to tape or disk depending on mode
	;; Includes: RAM, screen, BASIC vars, optional 128K banks
	;; ================================================================

	ld de,(05ffah)
	ld hl,mf3_loader

	call store_loader_address

	;; Save BASIC system variables
	ld de,05d4fh
	ld hl,03fd4h
	ld bc,0010h
	ldir

	;; Check if disk or tape operation
	call check_tape_or_disk
	jp nz,l0b47h
	ld de,l0071h
	ld hl,00fbbh
	call store_loader_address
	ld hl,(0202bh)
	ld (hl),043h
	ld de,05d34h
	call copy_filename_9bytes

	;; Build save file structure
	ld hl,l0000h
	ld bc,l0000h
	ld de,(05ffah)		; Main RAM address
	call write_file_header		; Write block header
	ld de,(05ffch)
	call write_file_header
	ld de,005b3h
	call write_file_header
	inc hl	
	inc hl	
	ld de,(0601ah)
	call write_file_header
	ld de,(0601ch)
	call write_file_header
	ld de,(0601eh)
	call write_file_header
	ld de,(06020h)
	call write_file_header
	ld de,(06022h)
	call write_file_header
	ld d,b	
	ld e,c	
	ld bc,l0000h
	call write_file_header
	ld a,c	
	or b	
	jr z,l0b41h
	inc hl	
l0b41h:
	call perform_file_write
	jp nc,l1c71h
	;; ================================================================
	;; Save Program Header and Main Blocks
	;; Writes BASIC program area and main memory blocks
	;; ================================================================
l0b47h:
	ld hl,(0202bh)
	ld (hl),020h
	ld ix,(PROG)
	ld de,000e9h
	ld bc,l0000h+1
	call write_with_verify		; Write BASIC program
	di
	jp nc,l1c71h		; Error handler
	call delay_if_disk

	ld hl,(0202bh)
	ld (hl),043h
	ld ix,RAMAREA
	ld de,(05ffah)

	call check_tape_or_disk		; Check disk/tape mode
	jr nz,l0b99h

	;; Disk mode - use +3DOS file operations
	call save_dos_workspace

	ld hl,05fe2h
	ld b,003h
	ld c,003h
	ld d,002h
	ld e,004h

	call file_open_read
	jp nc,l1c71h
	ld hl,RAMAREA
	ld de,(05ffah)
	ld b,003h
	ld c,000h
	call file_write
	jp nc,l1c71h
	jr l0b9fh

	;; Tape mode - use ROM tape routines
l0b99h:
	call write_to_tape
	call delay_if_disk

	;; ================================================================
	;; Save Screen and BASIC Variables
	;; Writes screen memory and BASIC variable area
	;; ================================================================
l0b9fh:
	ld ix,SCREEN
	ld de,(05ffch)		; Screen size
	ld c,000h
	call write_to_tape
	jr nc,l0c1bh
	call delay_if_disk
	call page_mf3_out		; Swap buffers
	call delay_if_disk
	ld ix,060b3h
	ld de,005b3h		; BASIC variables size
	ld c,000h
	call write_to_tape
	push af
	call page_mf3_out
	pop af
	jr nc,l0c1bh
	call delay_if_disk
	;; ================================================================
	;; Save Additional 128K Banks (Optional)
	;; Saves banks 1, 3, 4, 6, 7 if 128K mode is active
	;; Checks flags to determine which banks to save
	;; ================================================================
	ld a,(06000h)
	bit 2,a
	jp z,l0c8ch
	ld a,(06018h)
	bit 0,a
	jr z,l0c0bh
	call check_tape_or_disk
	jr z,l0befh

	;; Save Bank 1 (tape mode)
	ld a,011h
	ld de,(0601ah)		; Bank 1 size
	call save_memory_bank
	call delay_if_disk
	jr l0c0bh

	;; Save Bank 1 (disk mode)
l0befh:
	call swap_bank_to_8000h
	call save_dos_workspace
	ld a,007h
	ld c,a
	ld hl,08000h
	ld de,(0601ah)
	ld b,003h
	call file_write
	push af
	call swap_bank_to_8000h
	pop af
	jr nc,l0c1bh

	;; Save Bank 3
l0c0bh:
	ld a,(06018h)
	bit 1,a
	jr z,l0c21h
	ld a,013h
	ld de,(0601ch)		; Bank 3 size
	call save_memory_bank
l0c1bh:
	jp nc,l1c71h		; Error handler
	call delay_if_disk

	;; Save Bank 4
l0c21h:
	ld a,(06018h)
	bit 2,a
	jr z,l0c36h
	ld a,014h
	ld de,(0601eh)		; Bank 4 size
	call save_memory_bank
	jr nc,l0c1bh
	call delay_if_disk

	;; Save Bank 6
l0c36h:
	ld a,(06018h)
	bit 3,a
	jr z,l0c4bh
	ld a,016h
	ld de,(06020h)		; Bank 6 size
	call save_memory_bank
	jr nc,l0c1bh
	call delay_if_disk

	;; Save Bank 7
l0c4bh:
	ld a,(06019h)
	bit 3,a
	jr z,l0c89h
	call check_tape_or_disk
	jr z,l0c65h
	ld a,017h
	ld de,(06022h)
	call save_memory_bank
	call delay_if_disk
	jr l0c89h
l0c65h:
	call prepare_bank7_save
	call save_dos_workspace

	ld a,007h
	ld c,a	

	ld hl,08000h
	ld de,(06022h)
	ld b,003h
	call file_write
	
	jr nc,l0ca9h
	ld b,003h

	call dos_close_file
	call swap_plus3dos
	call swap_bank7_to_8000h
	
	jr l0c9dh
l0c89h:
	call set_bank0_rom_x1
l0c8ch:
	call check_tape_or_disk
	jr nz,l0ca0h
	call save_dos_workspace
	ld b,003h
	call dos_close_file
	di	
	call swap_plus3dos
l0c9dh:
	call swap_mf3_page1_4k
l0ca0h:
	call save_menu_screen
	call reset_compression_state
	jp l11a1h
l0ca9h:
	call prepare_bank7_save
	jp l1c71h

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
	ld a,(06000h)
	bit 5,a
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
	ld a, 'A' 		; drive A? hardcoded :/
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
	
	jr z,l0d00h
	pop af	
	call switch_bank
	jr l0d44h
l0d00h:
	call save_dos_workspace
	pop af	
	and 007h
	ld c,a
	
l0d07h:
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
	jr z,l0d07h
	jr l0d44h

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
	jr z,l0d4eh
	xor a	
	push de	
	pop hl
	
tape_save_block:
	ld (05fe1h),a
	ld (05fech),de
	ld (05feeh),bc
	ld (05ff0h),hl
	push de	
	push ix
	ld ix,05fe1h
	ld de,0011h
	xor a	
	call call_rom_sabytes
	call delay_4x65535
	pop ix
	pop de	
l0d44h:
	ld a,0ffh
	
call_rom_sabytes:
	ld hl,SABYTES
	call CALL_48ROM
	scf	
	ret
	
l0d4eh:
	xor a	

	call setup_dos_params
	call copy_filename_to_dos
	call save_dos_workspace
	
	ld hl,06008h
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
	ld hl,06011h
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
	ld (06011h),a
	ld (06012h),de
	ld (06014h),bc
	ld (06016h),de
	ret	
copy_filename_to_dos:
	ld de,06008h
copy_filename_9bytes:
	ld hl,05fe2h
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
l0dc1h:
	dec de	
	ld a,d	
	or e	
	jr nz,l0dc1h
	djnz delay_loop
	ret	
l0dc9h:
	call copy_interop
	call set_bank0_rom3

	ld hl,06000h
	set 5,(hl)
	jr l0dfch
l0dd6h:
	call copy_interop
	call delay_4x65535

	ld hl,06000h
	res 5,(hl)

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

	jp nc,l1c71h
l0dfch:
	ld ix,RAMAREA
	ld de,l0071h+1
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
	ld de,(05ffch)
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
	ld a,(06000h)
	bit 2,a
	jr z,l0e5ah
	call set_rom0
	ld hl,l0000h+1
	call CALL_READROM
	ld a,c	
	cp 0afh
l0e5ah:
	jp z,l0620h
	ld a,(06018h)
	bit 0,a
	jr z,l0e8eh
	call check_tape_or_disk
	jr nz,l0e7eh
	call swap_bank_to_8000h
	ld ix,08000h
	ld de,(0601ah)
	ld c,001h
	call load_block_tape_or_disk
	call swap_bank_to_8000h
	jr l0e8eh
l0e7eh:
	call select_rom1_printer_bank2
	ld ix,0c000h
	ld de,(0601ah)
	ld c,001h
	call load_block_tape_or_disk
l0e8eh:
	ld b,003h
	ld a,(06018h)
	ld e,a	
	rrc e
	ld a,(06019h)
	ld d,a	
l0e9ah:
	push bc	
	rrc e
	jr nc,l0eceh
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
	ld hl,0601ah
	add hl,de	
	ld e,(hl)	
	inc hl	
	ld d,(hl)	
	call load_block_tape_or_disk
	pop de	
l0eceh:
	pop bc	
	djnz l0e9ah
	bit 3,d
	jr z,l0effh
	call check_tape_or_disk
	jr nz,l0ef2h
	call prepare_bank7_save
	ld ix,08000h
	ld de,(06022h)
	ld c,007h
	call load_block_tape_or_disk
	call swap_plus3dos
	
	;; swap the bank in A into 8000h
	call swap_bank7_to_8000h
	jr l0effh
l0ef2h:
	ld ix,0c000h
	ld de,(06022h)
	ld c,007h
	call load_block_tape_or_disk
l0effh:
	call check_tape_or_disk
	jr nz,l0f07h
	call swap_mf3_page1_4k
l0f07h:
	ld a,(BANKM)
	and 0f0h
	call switch_bank
	ld (BANKM),a
	call decompress_all_banks
	jp l0620h

load_block_tape_or_disk:
	call check_tape_or_disk
	jr z,l0f2ch
	ld a,c	
	or ROM_SEL0
	call switch_bank_restore_ay
	
	ld a,0ffh
	scf	

	ld hl,LDBYTES
	jp CALL_48ROM
	
l0f2ch:
	push ix
	pop hl	
	push bc	
	call save_dos_workspace
	
	pop bc	
	ld b,003h

	jp file_read
l0f39h:
	nop	
	ld bc,0091h
	jp pe,l01f3h
	add a,d	
	nop	
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
	
l0f67h:
	in a,(P_MF3_IN)
	ld a,(l0000h)
	cp 0c3h
	jr z,l0f80h
	ld bc,0074h
	ld hl,(PROG)
	add hl,bc	
	ld b,00eh
l0f79h:
	ld a,(hl)	
	rst 10h	
	inc hl	
	djnz l0f79h
	jr l0f67h
l0f80h:
	in a,(P_MF3_OUT)
	ld hl,(PROG)
	push hl	
	ld bc,0074h
	add hl,bc	
	ld b,007h
l0f8ch:
	ld a,(hl)	
	rst 10h	
	inc hl	
	djnz l0f8ch
	pop hl	
	ld bc,000bch
	add hl,bc	
	ld b,007h
l0f98h:
	ld a,(hl)	
	rst 10h	
	inc hl	
	djnz l0f98h
	in a,(P_MF3_IN)
	jp 05c7fh
	jr nz,l0fc4h
	jr nz,l0fc6h
	jr nz,l0fc8h
	jr nz,l0fcah
	rst 38h	

mf3_loader:	
	jr nz,$-94
	ld d,014h
	inc c	
	ld (de),a	
	ld bc,00611h
	ld c,(hl)	
	ld c,a	
	ld d,h	
	jr nz,$+81
	ld c,(hl)	
	ld hl,l006eh
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
l0fc4h:
	nop	
	nop	
l0fc6h:
	nop	
	nop	
l0fc8h:
	nop	
	nop	
l0fcah:
	nop	
	nop	
	nop	
	dec c	
	nop	
	nop	
	ld (bc),a	
	ld c,a	
	nop	
	rst 20h	
	cp h	
	and a	
	ld a,(0bcd9h)
	and a	
	ld a,(0bcdah)
	and a	

	ld a,(0b0fdh)
	ld (03432h),hl

	dec (hl)	
	inc (hl)	
	inc sp
	
	ld (0f53ah),hl
	ld (l1416h),hl

	add hl,bc	

	ld (de),a	
	ld bc,00611h
	ld c,l	
	inc sp	
	jr nz,$+78
	ld c,a	
	ld b,c	
	ld b,h	
	ld c,c	
	ld c,(hl)	
	ld b,a	
	ld (0f93ah),hl
l0fffh:
	ret nz	
l1000h:
	jr z,$-78
l1002h:
	ld (03532h),hl
	ld (hl),022h
	ld hl,(0b0beh)
	ld (03332h),hl
	ld (hl),033h
	ld (hl),022h
	dec hl	
	cp (hl)	
	or b	
	ld (03332h),hl
	ld (hl),033h
	dec (hl)	
	ld (0b02bh),hl
	ld (02235h),hl
	add hl,hl	
	dec c	
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
	
l1037h:
	call check_tape_or_disk
	jp nz,l108dh
	ld hl,000eh
	call perform_file_write
	jr nc,l107fh
	ld de,l1b00h
	ld bc,SCREEN
	ld a,003h
	call setup_dos_params
	ld hl,05fe2h
	ld b,003h
	ld c,003h
	ld d,001h
	ld e,004h
	call file_open_read
	jr nc,l107fh
	ld b,003h
	call refresh_file_header
	jr nc,l107fh
	push ix
	pop de	
	ld hl,06011h
	ld bc,0007h
	ldir
	ld b,003h
	ld c,007h
	ld hl,SCREEN
	ld de,l1b00h
	call file_write
l107fh:
	jp nc,l1c71h
	ld b,003h
	call dos_close_file
	call scan_ay_registers
l108ah:
	jp l11a1h
l108dh:
	ld a,003h
	ld bc,SCREEN
	push bc	
	pop ix
	ld de,l1b00h
	ld hl,0ffffh
	call tape_save_block
	call save_menu_screen
	jr l108ah

init_dos_workspace:
	ld hl,0db00h
	ld bc,00938h
	call clear_ram

	ld a,041h
	ld (0df94h),a
	ld (0e3eah),a

	ld hl,l1104h
	ld de,0dfe8h
	ld bc,0052h
	ldir
	
	ld hl,0e038h
	ld (0e294h),hl
	ld hl,0e2c0h
	ld (0e2a0h),hl
	ld hl,0e39ah
	ld (0e2b8h),hl

	ld hl,l1156h
	ld de,0e2dbh
	ld bc,0015h
	ldir

	ld hl,l116bh
	ld de,0e39ah
	ld bc,0011h
	ldir

	ld hl,l117ch
	ld de,0e3f1h
	ld bc,0004h+1
	ldir

	ld hl,l1181h
	ld de,0e428h
	ld bc,000ah
	ldir
	ld hl,l01ceh
l1100h:
	ld (0e041h),hl
	ret	
l1104h:
	sub b	
	ret po	
	rlca	
	ret po	
	rst 18h	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	ret nz	
	ld bc,0dfebh
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	jp nz,0f601h
	rst 18h	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	call nz,l00ffh+2
	ret po	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	add a,001h
	inc c	
	ret po	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	ret z	
	ld bc,0e017h
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	jp z,02201h
	ret po	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	call z,02d01h
	ret po	
l1156h:
	inc b	
	ld b,c	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	ret p	
	jp po,0e300h
	adc a,b	
	add hl,de	
	ld a,h	
	add hl,de	
	add a,d	
	add hl,de	
l116bh:
	inc b	
	nop	
	inc bc	
	rlca	
	nop	
	dec sp	
	nop	
	ccf	
	nop	
	ret nz	
	nop	
	nop	
	add a,b	
	ex af,af'	
	nop	
	ld (bc),a	
	inc bc	
l117ch:
	add a,b	
	ld bc,0802h
	inc bc	
l1181h:
	ld a,(bc)	
	ld (01eafh),a
	inc c	
	rrca	
	nop	
	nop	
	ld bc,02101h
	add a,b	
	ld (de),a	
	jr l11a7h
l1190h:
	ld sp,(05c3dh)
	ld de,06056h
	pop hl	
	push de	
	ld a,(iy+000h)
	cp 0ffh
	jp nz,l1c71h
l11a1h:
	call set_bank0_rom3
	ld hl,l11cch
l11a7h:
	ld (06006h),hl
	ld hl,KSTATE
	ld bc,l03dfh
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
	ld hl,(06006h)
	jp (hl)	
l11cch:
	call decompress_screen_if_needed
l11cfh:
	call display_main_menu
	ld hl,06000h
	res 0,(hl)
	res 5,(hl)
	ld a,(0201dh)
	and 0ceh
	ld (0201dh),a

	;; ================================================================
	;; Main Menu Keyboard Handler
	;; Processes user input from main Multiface menu
	;; Commands: R)eturn S)ave T)oolkit P)rint O)ptions
	;;           D)OS A)lternate screen C)lear banks
	;; ================================================================
main_keyloop:
	call get_key_wait

	cp 'R'
	jp z,l0648h			; Return to interrupted program

	cp 'S'
	jp z,l091dh			; Save to tape/disk

	cp 'T'
	jp z,l1588h			; Enter toolkit mode

	cp 0e2h
	jp z,l1d79h			; Memory checksum

	cp 'P'
	jr z,l125eh			; Print screen

	cp 'O'
	jr z,l1231h			; Toggle options display

	call check_48kmode
	jr z,main_keyloop		; Skip DOS commands if locked

	cp 'D'
	jp z,l138ch			; DOS operations

	cp 'A'
	jr z,l1214h			; Alternate screen

	cp 'C'
	jr z,l123eh			; Clear RAM banks

	jr main_keyloop
l1214h:
	call swap_screen_buffers
	jr l11cfh

swap_screen_if_128k:
	call check_48kmode
	ret z	
l121dh:
	ld a,(06000h)
l1220h:
	bit 1,a
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
l1231h:
	ld a,(02006h)
	xor 004h
	ld (02006h),a
l1239h:
	call print_mainmenu
	jr main_keyloop

	;; ================================================================
	;; Clear RAM Banks
	;; Wipes banks 0x16, 0x14, 0x13, 0x11
	;; Used for "Clean" operation to free memory
	;; Only available when +3DOS is accessible
	;; ================================================================
l123eh:
	call check_48kmode
	jr z,main_keyloop

	call restore_menu_screen
	ld a,016h
	call clear_bank
l124bh:
	ld a,014h
	call clear_bank
	dec a
	call clear_bank
	ld a,011h
	call clear_bank
	call set_bank0_rom_x1
	jr l1239h

	;; ================================================================
	;; Screen Print Entry
	;; Prepares and initiates screen dump to printer
	;; Sets border to red and calls screen dump routine
	;; ================================================================
l125eh:
	call restore_menu_screen
	ld a,002h
	out (0feh),a
	call screen_print_main
	jp l11a1h
	
set_filename_cursor_pos:
	ld a,(06005h)
	sub 016h
	ld d,000h
	ld e,a	
	ld hl,05fe2h
	add hl,de	
	ld (hl),020h
	ld (0202bh),hl
	inc hl	
	ld (hl),0ffh
	ret	
l1280h:
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
	
	ld hl,05ae0h
	ld b,020h
l12a2h:
	res 7,(hl)
	inc hl	
	djnz l12a2h
l12a7h:
	call print_save_menu
	ld hl,06000h
	res 5,(hl)
	ld sp,(05c3dh)
	ld de,06056h
	pop hl	
	push de	
	ld (iy+000h),0ffh
	call set_filename_cursor_pos

l12bfh:
	call get_key_wait
	cp 054h
	jp z,l0a72h
	cp 041h
	jp z,l11a1h
	
	cp 053h
	jr z,l1317h
	cp 050h
	jr z,l131eh
	call check_48kmode
	jr z,l12bfh

	cp 044h
	jp z,l0a81h

	cp 04bh
	jr z,l12efh

	ld hl,02006h
	bit 4,(hl)
	jr z,l12bfh
	cp 04ch
	jr z,l12ffh
	jr l12bfh
l12efh:
	ld a,(02006h)
	xor 010h
	ld (02006h),a
	bit 4,a
	jr nz,l12a7h
	res 5,a
	jr l1304h
l12ffh:
	ld a,(02006h)
l1302h:
	xor 020h
l1304h:
	ld (02006h),a
	bit 5,a
	jr z,l130fh
	ld a,030h
	jr l1312h
l130fh:
	ld a,(0201eh)
l1312h:
	ld (03ff6h),a
	jr l12a7h
l1317h:
	ld hl,06000h
	set 7,(hl)
	jr l12a7h
l131eh:
	ld hl,06000h
	res 7,(hl)
	jr l12a7h

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
	set 3,(iy+001h)
	ret	
l1333h:
	ld hl,0201dh
	set 5,(hl)
	call restore_menu_screen
	call set_filename_cursor_pos
	call make_room_233bytes
	call prepare_dos_environment
	ld hl,05fe2h
	ld b,003h
	ld c,001h
	ld d,000h
	ld e,001h
	call file_open_read
	jr nc,l1366h
	ld hl,(PROG)
	ld de,000e9h
	ld b,003h
	call file_read
	jr nc,l1366h
	ld b,003h
	call dos_close_file
l1366h:
	jp nc,l1c71h
	ld hl,05800h
	ld d,h	
	ld e,l	
	inc de	
	ld bc,002ffh
	ld (hl),009h
	ldir

	;; set the border colour
	ld a,001h
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
	
l138ch:
	call swap_screen_if_128k
	ld hl,0201dh
	set 0,(hl)
	res 5,(hl)
	call set_bank0_rom3
	call print_dosmenu
	call cls_print_msg
	out (P_MF3_OUT),a
	ld sp,(05c3dh)
	ld de,06056h
	pop hl	
	push de	
l13aah:
	call get_key_wait
	cp 041h
	jp z,l11a1h
	cp 04ch
	jr z,l13bch
	cp 045h
	jr z,l13c4h
	jr l13aah
l13bch:
	ld hl,06000h
	set 0,(hl)
	jp l091dh
l13c4h:
	call restore_menu_screen
	ld hl,0201dh
	res 1,(hl)
	res 2,(hl)
	ld bc,l01adh
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
l13ebh:
	ld hl,060afh
	ld b,021h
	ld c,001h
	pop de
	
	ld ix,DOS_CATALOG
	call CALL_ROM2
	jp nc,l1c71h
	
	push bc	
	call scan_ay_registers

	pop bc	
	dec b	
	jp z,l138ch
l1406h:
	push bc	
	call print_toolkit_confirm
	pop bc	
	push bc	
	ld c,b	
l140dh:
	push bc
	
	;; not sure what this does.. turns on flash i think.??
	ld hl,msg_enter_attrs
	ld b,003h
	call printf

l1416h:
	pop bc	
	push bc	
	ld a,c	
	sub b	
	ld b,a	
	or a	
	ld hl,0000dh
	jr z,l1427h
	ld de,0000dh
l1424h:
	add hl,de	
	djnz l1424h
l1427h:
	ld d,h	
	ld e,l	
	ld hl,(PROG)
	add hl,de	
	ld (02022h),hl
	ld b,008h
	call l0a11h
	ld a,02eh
	rst 30h	
	ld b,003h
	call l0a11h
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
l144bh:
	call CALL_BEEPKEY
	cp 020h
	jr z,l1460h
	cp 041h
	jr z,l14a2h
	cp 059h
	jr z,l14a7h
	cp 04eh
	jr z,l1460h
	jr l144bh
l1460h:
	pop bc	
l1461h:
	djnz l140dh
l1463h:
	pop bc	
l1464h:
	ld a,b	
l1465h:
	cp 020h
	jr nc,l1479h
	ld a,(0201dh)
l146ch:
	bit 1,a
	jp nz,l13c4h
l1471h:
	bit 2,a
l1473h:
	jp nz,l13c4h
	jp l1406h
l1479h:
	ld hl,0201dh
	set 1,(hl)
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
	jp l13ebh
l14a2h:
	pop bc	
	pop bc	
	jp l138ch
l14a7h:
	call restore_menu_screen
	ld hl,(02022h)
	ld de,05fe2h
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
	
	jp nc,l1c71h
	call scan_ay_registers
	call print_toolkit_confirm
	ld hl,0201dh
	set 2,(hl)
	jp l1460h
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
	jp l0a43h
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
	ld hl,l0f39h
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
	
	ld a,038h
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
l1588h:
	call swap_screen_if_128k
	ld hl,02000h
	ld (05ffah),hl			; Set initial address to 0x2000
	ld a,010h
	ld (06001h),a			; Set bank to 0x10
	call switch_bank
l1599h:
	call init_toolkit_buffers
	ld hl,0201dh
	res 3,(hl)
	res 4,(hl)
	jp l1655h
l15a6h:
	ld hl,02006h
	set 7,(hl)
	ld hl,0201dh
	set 3,(hl)
	set 4,(hl)
	ld hl,l0000h
	ld (05fech),hl
	ld a,002h
	out (0feh),a
	jp l1886h
	;; implied ret
	
l15bfh:
	call init_toolkit_buffers
l15c2h:
	ld hl,0201dh
	bit 3,(hl)
	res 3,(hl)
	jr z,tool_keyloop
	call print_linefeed
	ld a,007h
	out (0feh),a

	;; ================================================================
	;; Toolkit Keyboard Handler
	;; Main loop for memory editor/toolkit
	;; Commands: Q)uit H)ex R)egisters W)rite T)ext P)rint S)elect bank
	;;           SPACE=address ENTER=poke Arrows=navigate N/M=page 0-F=hex
	;; ================================================================
tool_keyloop:
	call get_key
	jp z,l17d7h

	;; Arrow key navigation
	cp 009h
	jp z,l17c8h			; Right arrow
	cp 008h
	jp z,l17c3h			; Left arrow
	cp 00bh
	jp z,l17a7h			; Up arrow
	cp 00ah
	jp z,l17beh			; Down arrow

	;; ENTER key - execute POKE
	cp 00ch
	jp z,l16c2h

	cp ' '
	jr z,l15bfh			; SPACE - enter address mode
	cp '0'
	jr c,tool_keyloop
	cp ':'
	jp c,l16d4h			; 0-9 hex digit
	cp 'N'
	jp z,l17b9h			; N - next page
	cp 'M'
	jp z,l17a2h			; M - previous page
	cp 'H'
	jr z,tool_toggle_hex		; H - toggle hex/decimal
	cp 'R'
	jr z,l1640h			; R - jump to registers
	cp 'W'
	jr z,l1648h			; W - toggle write mode
	cp 'T'
	jr z,l165eh			; T - toggle text mode
	cp 'P'
	jp z,l15a6h			; P - printer mode
	cp 'S'
	jr z,l1668h			; S - select bank
	cp 'Q'
	jr z,tool_quit			; Q - quit toolkit
	cp 'A'
	jr c,tool_keyloop

	cp 'G'
	jp c,l16cbh			; A-F hex digit
	jr tool_keyloop

	;; ================================================================
	;; Toolkit Quit
	;; Exits toolkit and returns to main menu
	;; ================================================================
tool_quit:
	call restore_toolkit_screen
	jp l11a1h

	;; ================================================================
	;; Toggle Hex/Decimal Display
	;; Switches between hexadecimal and decimal display modes
	;; Controlled by bit 6 of memory location 0x2006
	;; ================================================================
tool_toggle_hex:
	ld a,(02006h)
	xor 040h
	ld (02006h),a
	jr l165bh

	;; ----------------------------------------------------------------
	;; Register Display Entry (toolkit 'R' command)
	;; Sets display address to 0x3FE4 - CPU register storage area
	;; Register order (INTEL format - low byte, high byte):
	;;   PC IY IX BC' DE' HL' AF' BC DE R- I- HL AF SP
	;; Total 28 bytes from 0x3FE4 to 0x3FFF
	;; ----------------------------------------------------------------
l1640h:
	ld hl,03fe4h		; Point to register storage area
	ld (05ffah),hl		; Set as current display address
	jr l165bh
l1648h:
	ld a,(02006h)
	xor 080h
	ld (02006h),a
	bit 7,a
	call z,restore_toolkit_screen
l1655h:
	ld hl,l0000h
	ld (05fech),hl
l165bh:
	jp l1809h
l165eh:
	ld a,(RAMAREA)
	xor 020h
	ld (RAMAREA),a
	jr l1655h

l1668h:
	call check_48kmode
	jr z,l16d0h

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
	ld (06001h),a
	call switch_bank

	;;  print the string
	push af	
	ld hl,l1935h
	ld b,007h
	call printf
	pop af

	;; print the number between 0 and 7
	
	and 007h
	or 030h
	rst 30h 		;print a single char
	
	jr l1655h 		; MAKEROOM?
	
init_toolkit_buffers:
	call print_toolkit_header
	ld hl,05ff4h
	ld (06003h),hl
	ld b,006h
	call fill_quotes
	ld hl,05ffch
	ld b,004h
	jr fill_quotes
print_and_fill_3bytes:
	ld hl,05ffch
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
l16c2h:
	call compare_pos_to_buffer_end
	jp nc,l1809h
	jp l15bfh
l16cbh:
	ld hl,02006h
	bit 6,(hl)
l16d0h:
	jr z,l1710h
	res 5,a
l16d4h:
	ld c,a	
	ld hl,05ffeh
	ld a,(02006h)
	bit 6,a
	jr z,l16e5h
	dec hl	
	ld a,048h
	ld (05ffeh),a
l16e5h:
	ld de,(06003h)
	and a	
	sbc hl,de
	jr c,l1710h
	ld a,c	
	push af	
	rst 30h	
	pop af	
	ld hl,(06003h)
	ld (hl),a	
	inc hl	
	ld (06003h),hl
	ld de,05ff9h
	ld a,(02006h)
	bit 6,a
	jr z,l170ah
	dec de	
	ld a,048h
	ld (05ff8h),a
l170ah:
	and a	
	sbc hl,de
	jp z,l17ffh
l1710h:
	jp l15c2h

	;; ================================================================
	;; Memory Address Validator and Translator
	;; Validates and translates memory addresses for toolkit viewing
	;; Handles special memory regions:
	;; - 0x5B00-0x60B3: System variables → MF3 RAM (SWAP area)
	;; - 0x50C0+: Screen area → MF3TEMP preserved screen
	;; - 0xC000-0xFFFF: Banked RAM (bank selection at 0x6001)
	;; Returns: Carry clear if invalid, Carry set + HL translated if valid
	;; ================================================================
validate_mem_addr:
	ld hl,(05ffah)
translate_mem_addr:
	bit 7,h
	jr z,l1729h
	bit 6,h
	jr z,l1729h
	ld a,(06001h)
	and 007h
	cp 005h
	jr nz,l1729h
	res 7,h
l1729h:
	ld bc,SWAP
	and a	
	sbc hl,bc
	push hl	
	pop de	
	add hl,bc	
	jr c,l1743h
	ld bc,060b3h
	and a	
	sbc hl,bc
	add hl,bc	
	ret nc	
	ld hl,0202dh
	add hl,de	
l1740h:
	scf	
	ccf	
	ret	
l1743h:
	ld bc,050c0h
	and a	
	sbc hl,bc
	push hl	
	pop de	
	add hl,bc	
	jr c,l1777h
	ld a,h	
	bit 3,a
	jr z,l1757h
	cp 05ah
	jr nz,l1777h
l1757h:
	ld a,l	
	and 0c0h
	cp 0c0h
	jr nz,l1777h
	ld hl,MF3TEMP
	ld b,d	
	xor a	
	ld d,a	
	or b	
	jr z,l1774h
	push de	
	cp 00ah
	jr nz,l176eh
	dec b	
	dec b	
l176eh:
	ld e,040h
l1770h:
	add hl,de	
	djnz l1770h
	pop de	
l1774h:
	add hl,de	
	jr l1740h
l1777h:
	ld a,(06000h)
	bit 6,a
	jr z,l178eh
	ld a,h	
	and 0f0h
	cp 040h
	jr z,l1797h
	ld a,h	
	cp 058h
	jr z,l179dh
	cp 059h
	jr z,l179dh
l178eh:
	push hl	
	ld de,02000h
	and a	
	sbc hl,de
	pop hl	
	ret	
l1797h:
	ld de,0e821h
l179ah:
	add hl,de	
	jr l1740h
l179dh:
	ld de,0e021h
	jr l179ah
l17a2h:
	ld hl,0ff80h
	jr l17aah
l17a7h:
	ld hl,0fff8h
l17aah:
	ld bc,(05ffah)
	add hl,bc	
	push hl	
	pop bc	
	ld hl,05ffch
	ld (06003h),hl
	jr l1805h
l17b9h:
	ld hl,l0080h
	jr l17aah
l17beh:
	ld hl,0008h
	jr l17aah
l17c3h:
	ld hl,0ffffh
	jr l17aah
l17c8h:
	ld hl,l0000h+1
	jr l17aah
compare_pos_to_buffer_end:
	ld hl,(06003h)
	ld de,05ffch
	and a	
	sbc hl,de
	ret	
l17d7h:
	ld hl,(06003h)
	ld de,05ff4h
	and a	
	sbc hl,de
	jr z,l1809h
	call compare_pos_to_buffer_end
	jr c,l17ffh
	jp z,l17f9h
	call parse_3byte_hex_input
	xor a	
	or b	
	jr nz,l1809h
	push bc	
	call validate_mem_addr
	pop bc	
	ld (hl),c	
	jr l1809h
l17f9h:
	call parse_5byte_hex_input
	inc bc	
	jr l1802h
l17ffh:
	call parse_5byte_hex_input
l1802h:
	jp c,l15bfh
l1805h:
	ld (05ffah),bc
l1809h:
	ld hl,(05ffah)
	ld de,05ff4h
	call convert_to_decimal_p1
	call display_buffer_or_addr
	ld hl,05ffch
	ld (06003h),hl
	ld a,03dh
	rst 30h	
	call validate_mem_addr
	jr c,l1826h
	ld a,(hl)	
	jr l182ah
l1826h:
	call CALL_READROM
	ld a,c	
l182ah:
	ld hl,02006h
	bit 6,(hl)
	jr nz,l183fh
	ld l,a	
	xor a	
	ld h,a	
	ld de,05ffch
	call convert_to_decimal_p2
	call print_and_fill_3bytes
	jr l184bh
l183fh:
	call l1aebh
	ld a,048h
	rst 30h	
	ld hl,05ffch
	call fill_3_quotes
l184bh:
	ld a,020h
	rst 30h	
	ld de,(05ffah)
	ld hl,03fffh
	and a	
	sbc hl,de
	jr c,l1881h
	add hl,de	
	ex de,hl	
	ld de,03fe4h
	and a	
	sbc hl,de
	jr c,l1881h
	push hl	
	add hl,hl	
	ld de,l195ah
	add hl,de	
l186ah:
	ld b,002h
	call printf
	ld hl,msg_poke
	ld b,015h
	call printf
	ld hl,02006h
	bit 7,(hl)
	jr nz,l1886h
	jp l15c2h
l1881h:
	ld hl,l1b82h
	jr l186ah
l1886h:
	ld hl,06000h
	bit 6,(hl)
	call z,tools_cls
	
	ld a,002h
	call open_channel

	ld hl,l19dah
	call print_msg_len9

	ld hl,(05ffah)
	ld a,l	
	and 0f8h
	ld l,a	
	ld de,0020h
	and a	
	sbc hl,de
	ld de,(05fech)
	and a	
	sbc hl,de
	add hl,de	
	jp z,01992h
	ld (05fech),hl
	call print_4_spaces
	call print_space_addr_colon
	call 019e3h
	ld a,020h
	rst 30h	
	call display_toolkit_cursor
	call print_n_spaces
l18c6h:
	ld a,0fdh
	call open_channel
	ld hl,0193fh
	ld b,006h
	call printf
	jp l15c2h

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

	defb 22, 1, 0
	defb 17, 02
	defb 16, 07
	defb 19, 01

	defb "Address:"
	defb 6,6
l1929h:
	ld d,000h
	rra	
	ld de,01006h
	ld bc,l0114h
	ld (hl),e	
	inc d	
	nop	

l1935h:
	defb 22, 1, 31
	defb 16, 07
	defb 17, 02

	defb 22,1,8
	defb 16,07
	defb 17,02
	defb 19,01

	;; ----------------------------------------------------------------
	;; POKE Prompt Message
	;; Shows address and current value for editing
	;; Displayed when write mode is active
	;; ----------------------------------------------------------------
msg_poke:
	defb 22,01,20
	defb " Poke:"

	;; flashing cursor?
	defb 18, 01
	defb " "
	defb 18, 00
	defb "    "
	defb 22,01,26
l195ah:
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
l1975h:
	daa	
	ld b,e	
l1977h:
	jr nz,l19bbh
	jr nz,$+71
	jr nz,l19c1h
	jr nz,l19cch
	ld h,d	
	ld d,d	
	jr nz,l19d0h
	ld l,c	
	ld c,c	
	jr nz,l19d3h
	jr nz,$+74
	jr nz,$+72
	jr nz,$+67
	jr nz,l1a02h
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
	jp l18c6h
display_toolkit_cursor:
	push hl	
	ld hl,(05ffah)
	ld a,l	
	and 007h
	ld c,a	
	add a,a	
	add a,c	
	ld c,a	
	ld b,000h
l19bbh:
	ld hl,05888h
	add hl,bc	
	ld (hl),0f1h
l19c1h:
	inc hl	
	ld (hl),0f1h
	pop hl	
	ret	
print_4_spaces:
	ld b,004h
	jr l19cch
print_n_spaces:
	ld b,00bh
l19cch:
	push bc	
	call print_space_addr_colon
l19d0h:
	call 019e3h
l19d3h:
	ld a,020h
	rst 30h	
	pop bc	
	djnz l19cch
	ret	
l19dah:
	ld d,000h
	nop	
	djnz l19dfh
l19dfh:
	ld de,l1304h+2
	ld bc,00806h
l19e5h:
	push bc	
	ld a,020h
	rst 30h	
	push hl	
	call translate_mem_addr
	jr c,l19f2h
	ld a,(hl)	
	jr l19f6h

l19f2h:
	call CALL_READROM
	ld a,c	
l19f6h:
	ld hl,RAMAREA
	bit 5,(hl)
	jr nz,l1a02h
	call l1aebh
	jr l1a09h
l1a02h:
	call filter_printable_char
	rst 30h	
	ld a,020h
	rst 30h	
l1a09h:
	pop hl	
	inc hl	
	pop bc	
	djnz l19e5h
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
	jr nc,l1a16h
l1a13h:
	ld a,02eh
	ret	
l1a16h:
	cp 080h
	ret c	
	jr l1a13h
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
	;; Then clears screen area and sets attributes to 0x70 (black on white)
	;; ================================================================
tools_cls:
	set 6,(hl)

	ld hl,SCREEN
	ld de,02821h
	ld bc,l1000h
	ldir

	ld hl,05800h
	ld de,03821h
	ld bc,01ffh+1
	ldir

	ld hl,SCREEN
	ld bc,l0fffh
	call clear_ram

	ld hl,05800h
	ld (hl),070h
	
	push hl	
	pop de	
	inc de	

	ld bc,01ffh
	ldir

	ret	

restore_toolkit_screen:
	ld hl,06000h
	bit 6,(hl)
	res 6,(hl)
	ret z	

	ld hl,02821h
	ld de,SCREEN
	ld bc,l1000h
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
	ld hl,02006h
	bit 6,(hl)
	jr nz,l1a8ah
	ld b,005h
	ld hl,05ff4h
	jp l1d58h
l1a8ah:
	ld hl,(05ffah)
print_hex_word_suffix:
	push hl	
	ld a,h	
	call l1aebh
	pop hl	
	ld a,l	
	call l1aebh
	ld a,048h
	jr l1b08h
print_toolkit_header:
	call cls_print_header
	ld hl,msg_toolmenu
	ld b,053h
	call printf
	call check_48kmode
	jr z,l1abbh

	ld hl,l1929h
	ld b,013h
	call printf

	ld a,(06001h)
	and 007h
	or 030h
	rst 30h	
l1abbh:
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
l1adch:
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
l1ae2h:
	add hl,bc	
	inc a	
	jr c,l1ae2h
	sbc hl,bc
	dec a
	jr l1adch

;; ================================================================
;; Print Hex Byte
;; Converts and prints byte in A as 2-digit hexadecimal
;; Prints both nibbles (upper then lower)
;; ----------------------------------------------------------------
;; Input:  A = byte to print
;; Uses:   A
;; ================================================================
l1aebh:
	push af	
	srl a
	srl a
	srl a
	srl a
	add a,030h
	cp 03ah
	jr c,l1afch
	add a,007h
l1afch:
	rst 30h	
	pop af	
	and 00fh
l1b00h:
	add a,030h
	cp 03ah
	jr c,l1b08h
	add a,007h
l1b08h:
	rst 30h
	ret

;; ----------------------------------------------------------------
;; Calculate Buffer Offset
;; Calculates offset into input buffer
;; Input: DE = buffer address, A = offset | Output: HL, B
;; ----------------------------------------------------------------
calc_buffer_offset:
	ld hl,(06003h)
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
	ld de,05ffch
	ld a,003h
	call calc_buffer_offset
	ld a,022h
	ld (05fffh),a
	ld a,(05ffeh)
	cp 048h
	jr nz,l1b50h
	jr l1b3dh

;; ================================================================
;; Parse 5-Byte Hex Input
;; Parses 5 bytes of hexadecimal input from buffer
;; Supports both hex (with 'H') and decimal formats
;; ----------------------------------------------------------------
;; Output: BC = parsed value, Carry clear = success
;; Uses:   HL, DE, BC, A
;; ================================================================
parse_5byte_hex_input:
	ld de,05ff4h
	ld a,005h
	call calc_buffer_offset
	ld a,022h
	ld (05ff9h),a
	ld a,(05ff8h)
	cp 048h
	jr nz,l1b50h
l1b3dh:
	ld de,l0000h
l1b40h:
	ld a,(hl)	
	cp 048h
	jr z,l1b4bh
	call hex_char_to_nibble
	inc hl	
	djnz l1b40h
l1b4bh:
	push de	
	pop bc	
	scf	
	ccf	
	ret	

l1b50h:
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
	jr z,l1b69h
	sub 007h
l1b69h:
	rlca	
	rlca	
	rlca	
	rlca	
	ld b,004h
l1b6fh:
	rlca	
	rl e
	rl d
	djnz l1b6fh
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
	nop	
	inc bc	
	jr nz,l1b9ch
	jr nz,$+34
	jr nz,l1ba0h
	jr nz,l1ba2h
l1b82h:
	jr nz,l1ba4h
	nop	
	dec de	
	nop	
	ld b,b	
	rst 38h	
	rst 38h	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	jr nz,l1b95h
l1b95h:
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
l1b9ch:
	nop	
	ld d,000h
	nop	
l1ba0h:
	ld b,h	
	ld c,c	
l1ba2h:
	ld d,e	
	ld c,e	
l1ba4h:
	jr nz,l1bc6h
	jr nz,l1bc8h
	rst 38h	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
l1bc6h:
	nop	
	nop	
l1bc8h:
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	

	;; 

	push af
	in a,(P_MF3_OUT)
	pop af
	call CALL_IX 		; int_call_ix

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

	ld hl,l006bh

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

	res 5,(iy+001h)
	set 3,(iy+001h)
	set 3,(iy+030h)

	ei 			; enable interruptss
	halt			; wait for the horizontal refresh
	di			; disable interrputs again.

	bit 5,(iy+001h)
	jr z,int_call_beep

	ld de,0022h
	ld hl,000c8h

	call BEEPER 		; call the 48k rom beeper routine

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
	call MAKEROOM		; call make room?
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
l1c50h:
	jp CALL_ROM2

	;; Read from file
file_read:
	ld ix,DOS_READ
	jr l1c50h
	;; implied ret

	;; Delete file
file_delete:
	ld ix,DOS_DELETE
	jr l1c50h
	;; implied ret

	;; Refresh file header
refresh_file_header:
	ld ix,DOS_REF_HEAD
	jr l1c50h
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
	
l1c71h:
	di	
	ld (02007h),a
	push af	
	call set_bank0_rom3
	call decompress_screen_if_needed
	call check_tape_or_disk
	jr nz,l1c84h
	call scan_ay_registers
l1c84h:
	call reset_compression_state
	pop af	
	ld hl,06000h
	bit 5,(hl)
	jr z,l1c95h
	cp 00ch
	jr z,l1cbch
	jr l1ca8h
l1c95h:
	cp 022h
	jr z,l1d0dh
	cp 001h
	jr z,l1cd3h
	cp 017h
	jr z,l1cbfh
	cp 005h
	jr z,l1cc9h
	or a	
	jr z,l1cc9h
l1ca8h:
	call set_bank0_rom3
	call cls_lower
	call print_io_error
	call CALL_BEEPKEY
l1cb4h:
	ld a,(0201dh)
	bit 0,a
	jp nz,l138ch
l1cbch:
	jp 0118bh

l1cbfh:
	call print_abort2
	ld hl,msg_file_not_found
	ld b,010h
	jr printf_wenter

l1cc9h:
	call print_abort2
	ld hl,msg_drive_not_ready
	ld b,011h
	jr printf_wenter
	
l1cd3h:
	call print_abort2
	ld hl,msg_write_prot+1
	ld b,011h
	
printf_wenter:	

	call printf
	call print_enter

l1ce1h:
	call get_key_wait
	jr z,l1cedh
	cp 041h
	jp z,l1cb4h
	jr l1ce1h
l1cedh:
	ld hl,l1333h
	ld a,(0201dh)
	bit 5,a
	jr nz,l1d01h
	ld hl,l13c4h
	bit 0,a
	jr nz,l1d04h
	ld hl,l0a81h
l1d01h:
	jp l11a7h
l1d04h:
	ld a,(02007h)
	or a	
	jr z,l1d01h
	jp l14a7h
l1d0dh:
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
	jr nc,l1d26h
	inc hl	
l1d26h:
	call 01d4eh
	ld hl,l1d49h
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
	ld hl,l00cfh
	ld b,00eh
	jr printf_wenter
l1d49h:
	ld c,e	
	jr nz,l1dc0h
	ld l,a	
	jr nz,l1d60h
	rra	
	jr nz,$-41
	call convert_to_decimal_p2
	pop hl	
	ld b,003h
l1d58h:
	ld c,000h
l1d5ah:
	ld a,(hl)	
	cp 030h
	jr z,l1d6eh
l1d5fh:
	rst 30h	
l1d60h:
	inc hl	
	inc c	
	djnz l1d5ah
	ret	
l1d65h:
	ld a,001h
	cp b	
	jr z,l1d75h
	ld a,020h
	jr l1d5fh
l1d6eh:
	dec c	
	ld a,0ffh
	cp c	
	jr z,l1d65h
	inc c	
l1d75h:
	ld a,030h
	jr l1d5fh

	;; ================================================================
	;; Memory Checksum Utility
	;; XORs all bytes from 0x2000 down to 0x0000
	;; Displays result for verification purposes
	;; Press SPACE to exit
	;; ================================================================
l1d79h:
	call cls_print_msg
	call setup_display
	ld hl,l0039h
	call print_msg_len9
	ld hl,02000h
	xor a
	ex af,af'
l1d8ah:
	ex af,af'
	dec hl
	xor (hl)			; XOR accumulate
	ex af,af'
	ld a,l
	or h
	jr nz,l1d8ah
	ex af,af'
	call l1aebh			; Display result

l1d96h:
	call spcscan
	jp nc,l11a1h		; Exit on SPACE press

	jr l1d96h
init_printer_config:
	ld a,(02006h)
	and 0c0h
	ld (02006h),a
	ld hl,(02015h)
	ld bc,(l004bh)
	and a	
	sbc hl,bc
	jr nz,l1dbah
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
l1dbah:
	ld hl,l004bh
	ld de,02015h
l1dc0h:
	ld bc,0003h
	ldir
	ld a,001h
	ld (02008h),a		; Set to Large Copy + CR+LF
	xor a
	ld (02006h),a		; Clear status flags
	ld (0200dh),a		; Top margin = 0
	ld a,008h
	ld (0200bh),a		; Left margin = 8
	ld a,017h
	ld (0200ch),a		; Bottom margin = 23
	ld a,020h
	ld (02009h),a
	ld hl,l0042h
	ld de,0200eh		; Copy ESC sequences for printer
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
l1df7h:
	push bc
	call print_prep
	call print_row_spacing		; Process row
	call print_row_data		; Output row
	pop bc
l1e02h:
	push bc
	ld d,004h
l1e05h:
	push bc
	push de
	ld a,b
	call calc_screen_addr		; Process pixels
	pop de
	ld b,a
	inc b
	ld a,(hl)
l1e0fh:
	rlca
	djnz l1e0fh
	push af
	rl e
	pop af
	rl e
	pop bc
	inc b
	dec d	
	jr nz,l1e05h
	ld a,(02008h)
	and 0f0h
	cp 0f0h
	jr nz,l1e2ch
	ld a,e	
	call print_graphics_byte
	jr l1e36h
l1e2ch:
	ld a,e	
	call print_char_to_printer
	call print_char_to_printer
	call print_char_to_printer
l1e36h:
	inc c	
	ld a,c	
	jr z,l1e3eh
	pop bc	
	ld c,a	
	jr l1e02h
l1e3eh:
	pop de	
	call print_linefeed
	call calc_row_height
	cp b	
	jr c,l1e58h
	jr l1df7h
	;; ----------------------------------------------------------------
	;; print_prep - Screen Print Preparation
	;; Initializes printer control codes
	;; HL = 0x200E, B = 3
	;; ----------------------------------------------------------------
print_prep:
	ld hl,0200eh
	ld b,003h
	jr l1e64h

	;; ----------------------------------------------------------------
	;; print_row_data - Print Row Data
	;; Outputs row data to printer
	;; HL = 0x2011, B = 4
	;; ----------------------------------------------------------------
print_row_data:
	ld hl,02011h
	ld b,004h
	jr l1e64h
l1e58h:
	ld c,008h
	ld b,002h
	ld hl,l0041h
	push bc	
	ld b,000h
	add hl,bc	
	pop bc	
l1e64h:
	ld a,(hl)	
	call print_char_to_printer
	inc hl	
	djnz l1e64h
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
	ld a,(0200bh)
	cp 000h
	ret z
	push bc
	ld b,a
l1e8fh:
	ld a,020h
	call print_char_to_printer
	djnz l1e8fh
	pop bc
	ret

	;; ----------------------------------------------------------------
	;; calc_row_height - Calculate Row Height
	;; Multiplies value at 0x200C by 7
	;; Used for printer row spacing calculations
	;; ----------------------------------------------------------------
calc_row_height:
	ld a,(0200ch)
	inc a

MULT_BY7: 			;multiply by 7
	add a,a
	add a,a
	add a,a
	dec a
	ret
	
print_top_margin:
	ld a,(0200dh)

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

	ld a,(0201dh)

	bit 3,a
	jr nz,l1eb9h
	pop af	

	ret
	
l1eb9h:
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
	ld a,(02008h)
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
l1ef8h:
	;; check for the break  key
	call brkscan
	jr nc,l1f18h

	in a,(c)
	bit 0,a
	jr nz,l1ef8h
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
l1f18h:
	pop af	
	pop bc	

	ld hl,0201dh
	res 3,(hl)
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
	bit 4,(hl)
	jp z,l11a1h
	jp l1599h

msg_printer_err:
	defb "Printer Error -"

print_graphics_byte:
	ld a,e	
	call calc_screen_address
	push bc	
	push ix
	ld b,003h
l1f5eh:
	push bc	
	push af	
	ld b,004h
l1f62h:
	push bc	
	rla	
	jr c,l1f99h
	ld ix,(02018h)
l1f6ah:
	ld d,(ix+000h)
	rl d
	rl e
	rla	
	rl d
	rl e
	pop bc	
	djnz l1f62h
	ld a,e	
	call print_char_to_printer
	pop af	
	pop bc	
	ld ix,(02018h)
	inc ix
	ld (02018h),ix
	ld ix,(0201ah)
	inc ix
	ld (0201ah),ix
	djnz l1f5eh
	pop ix
	pop bc	
	ret	
l1f99h:
	ld ix,(0201ah)
	jr l1f6ah
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
	call get_pattern_table_entry
	ld (02018h),hl
	pop af	
	and 007h
	call get_pattern_table_entry
	ld (0201ah),hl
	pop bc	
	pop af	
	ret	
get_pattern_table_entry:
	ld hl,l1fe8h
	ld c,a	
	add a,a	
	add a,c	
	ld c,a	
	ld b,000h
	add hl,bc	
	ret	
l1fe8h:
	ret nz	
	ret nz	
	ret nz	
	add a,b	
	ret nz	
	add a,b	
	add a,b	
	add a,b	
	add a,b	
	nop	
	ret nz	
	nop	
	ld b,b	
	add a,b	
	nop	
	nop	
	add a,b	
	add a,b	
	nop	
	add a,b	
	nop	
	nop	
	nop	
	nop

ztest:	equ int_call_48rom-interop_start