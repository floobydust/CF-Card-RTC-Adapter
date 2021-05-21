;**************************************************************************************************
;*  C02BIOS 3.04 (c)2013-2021 by Kevin E. Maier   *     New Hardware design with the following:   *
;* - BIOS in pages $F8-$FF, less I/O in page $FE  *  - W65C02 with clock rate up to 6.0 MHz       *
;* - Full duplex interrupt-driven/buffered I/O    *  - AS6C66256 32KB Static RAM                  *
;* - Extendable BIOS structure with soft vectors  *  - AT28H256 32KB EEPROM ROM                   *
;* - Soft config parameters for I/O devices       *  - ATF22V10CQZ Single Glue Logic              *
;* - Monitor cold/warm start soft vectored        *  - NXP SCC2691 UART for console/timer         *
;* - Fully relocatable code (sans page $FF)       *  - Hardware map is flexible via Glue logic    *
;* - Precision timer services w/10ms accuracy     *  - 5 I/O selects @ 32-bytes wide              *
;* - RTC based Jiffy Clock, Sec, Min, Hour, Days  *  - 4 I/O selects available on expansion bus   *
;* - Accurate delays from 10ms to ~497 days       *  - 1 I/O select used by SCC2691 UART          *
;* - 10ms Benchmark Timing to 65535.99 seconds    *     Additional Hardware via Adapter Card      *
;*                                                *  - Compact Flash True IDE mode interface      *
;*                                                *    - 16-bit upper latch for data read/write   *
;*                                                *  - DS1511Y Realtime Clock/Calendar            *
;*                                                *                                               *
;*                                                *   Note: default HW system memory map as       *
;*  Uses <2KB EEPROM - JMP table page at $FF00    *         RAM - $0000 - $7FFF                   *
;*    Uses 160 bytes for I/O: starts at $FE00     *         ROM - $8000 - $FDFF                   *
;*        Default assembly starts at $F800:       *         I/O - $FE00 - $FE9F                   *
;*          14/05/2021 (Day/Month/Year)           *         ROM - $FEA0 - $FFFF                   *
;**************************************************************************************************
        PL      66      ;Page Length
        PW      132     ;Page Width (# of char/line)
        CHIP    W65C02S ;Enable WDC 65C02 instructions
        PASS1   OFF     ;Set ON when used for debug
        INCLIST ON      ;Set ON for listing Include files
;**************************************************************************************************
;
; C02BIOS Version 3.04 has some minor bug fixes for RTC and IDE routines, plus some cleanup.
; C02BIOS Version 3.03 makes a major change in LBA Read and Write routines!
; - To ensure compatibility with previously formatted IDE devices, the Byte swapping of high/low
; - per word transfer has been eliminated. It's important to note that the Identity Command
; - will provide data where the byte swapping needs to be done, otherwise it doesn't match up
; - per the SanDisk documentation.
;
; This BIOS and Monitor version also use a common source file for constants and variables used by
; both. This just simplifies keeping both code pieces in sync.
;
        INCLUDE         C02Constants3.asm
;
;**************************************************************************************************
;       - Monitor JUMP table: 32 JUMP calls are available. Calls 02-04 are currently Reserved
;
M_COLD_MON      .EQU    $E000           ;Call 00        Monitor Cold Start
M_WARM_MON      .EQU    $E003           ;Call 01        Monitor Warm Start
;
M_BSOUT         .EQU    $E00F           ;Call 05        Send Backspace
M_XMDM_SAVE     .EQU    $E012           ;Call 06        Xmodem Save Entry
M_XMDM_LOAD     .EQU    $E015           ;Call 07        Xmodem Load Entry
M_BENCH         .EQU    $E018           ;Call 08        Benchmark Start
M_QUITB         .EQU    $E01B           ;Call 09        Benchmark Stop/End
M_UPTIME        .EQU    $E01E           ;Call 10        Monitor Uptime
M_PRSTAT1       .EQU    $E021           ;Call 11        CPU Status Display
M_DIS_LINE      .EQU    $E024           ;Call 12        Disassemble Line of Code
M_INCINDEX      .EQU    $E027           ;Call 13        Increment Index by 1
M_DECINDEX      .EQU    $E02A           ;Call 14        Decrement Index by 1
M_RDLINE        .EQU    $E02D           ;Call 15        Read Line from Terminal
M_RDCHAR        .EQU    $E030           ;Call 16        Read Character from Terminal
M_HEXIN2        .EQU    $E033           ;Call 17        Hex input 2 characters
M_HEXIN4        .EQU    $E036           ;Call 18        Hex input 4 characters
M_HEX2ASC       .EQU    $E039           ;Call 19        Convert Hex to ASCII
M_BIN2ASC       .EQU    $E03C           ;Call 20        Convert Binary to ASCII
M_ASC2BIN       .EQU    $E03F           ;Call 21        Convert ASCII to Binary
M_BEEP          .EQU    $E042           ;Call 22        Send BEEP to Terminal
M_DOLLAR        .EQU    $E045           ;Call 23        Send $ to Terminal
M_CROUT         .EQU    $E048           ;Call 24        Send C/R to Terminal
M_SPC           .EQU    $E04B           ;Call 25        Send ASCII Space to Terminal
M_PRBYTE        .EQU    $E04E           ;Call 26        Print Byte to Terminal
M_PRWORD        .EQU    $E051           ;Call 27        Print Word to Terminal
M_PRASC         .EQU    $E054           ;Call 28        Print ASCII to Terminal
M_PROMPT        .EQU    $E057           ;Call 29        Send Message by number to Terminal
M_PROMPTR       .EQU    $E05A           ;Call 30        Send Message by address to Terminal
M_CONTINUE      .EQU    $E05D           ;Call 31        Y/N Prompt to Continue Command
;
;**************************************************************************************************
        .ORG    $F800   ;2KB reserved for BIOS, I/O device selects (160 bytes)                    *
;**************************************************************************************************
;                               START OF BIOS CODE                                                *
;**************************************************************************************************
;C02BIOS version used here is 3.04 (updated release)
; Contains the base BIOS routines in top 2KB of EEPROM
; - Input/Feedback from "BDD" - modified CHRIN/CHROUT I/O routines - saves 12 bytes
; - $F800 - $F9FF 512 bytes for BIOS SCC2691, NMI Panic routine
; - $FA00 - $FDFF reserved for BIOS expansion (1KB)
; - $FE00 - $FE7F reserved for HW (4-I/O selects, 32 bytes wide)
; - $FE80 - $FE9F SCC2691 UART (32 bytes wide, only 8 bytes used)
; - $FEA0 - $FEFF used for Vector and Hardware configuration data
; - $FF00 - $FFFF JMP table, CPU startup, NMI/BRK/IRQ pre-post routines, Page $03 init, BIOS msg
;
; UPDATEs:
; Fixed IRQ enable for CF Card. Also streamlined some startup routines. 11th Oct 2020
; Eliminates IDE byte swapping to ensure compatibility with standard formats 20th January 2021
; Reserved $0400 for second UART buffer space 5th February 2021
; Default IDE Block buffer at $0600 5th February 2021
; Default RTC NVRAM buffer space at $0500 5th February 2021
; Panic Routine changed, no longer saves multiple pages, just restores system 5th February 2021
; New routine to Start Benchmark Counter. Allows counter Start/Stop (pause) 5th February 2021
; Some minor cleanup and fixed a few bugs in the RTC and IDE routines 14th May 2021
;
; Note: Move Enhanced Basic buffer page to $0500
;
;**************************************************************************************************
; The following 32 functions are provided by BIOS via the JMP Table
; $FF21 - $FF2A are Reserved for future expansion (4 available)
;
; $FF00 IDE_RESET       ;Reset IDE Controller (run diagnostics)
; $FF03 IDE_GET_STAT    ;Get Status and extended error code
; $FF06 IDE_IDENTIFY    ;Load IDE Identity Data at $0600
; $FF09 IDE_READ_LBA    ;Read LBA into memory
; $FF0C IDE_WRITE_LBA   ;Write LBA from memory
; $FF0F IDE_VERFY_LBA   ;Verify LBA from last Read/Write
; $FF12 IDE_SET_LBA     ;Set LBA number (24-bit onlu)
; $FF15 IDE_SET_ADDR    ;Set LBA transfer address (16-bit plus block count)
;
; $FF18 RTC_NVRD        ;Read NVRAM (256 bytes) from RTC to memory (16-bit)
; $FF1B RTC_NVWR        ;Write NVRAM (256 bytes) from memory to RTC (16-bit)
; $FF1E RTC_INIT        ;Initialize software RTC from hardware RTC
;
; $FF2D CNT_INIT        ;Reset benchmark timing counters/Start 10ms benchmark timer
; $FF30 CNT_STRT        ;Start 10ms benchmark timing counter
; $FF33 CNT_STOP        ;Stop 10ms benchmark timing counter
;
; $FF36 CHRIN_NW        ;Data input from console, no waiting, clear carry if none
; $FF39 CHRIN           ;Data input from console
; $FF3C CHROUT          ;Data output to console
;
; $FF3F SET_DLY         ;Set delay value for milliseconds and 16-bit counter
; $FF42 EXE_MSDLY       ;Execute millisecond delay 1-256 * 10 milliseconds
; $FF45 EXE_LGDLY       ;Execute long delay; millisecond delay * 16-bit count
; $FF48 EXE_XLDLY       ;Execute extra long delay; 8-bit count * long delay
;
; $FF4B INIT_VEC        ;Initialize soft vectors at $0300 from ROM
; $FF4E INIT_CFG        ;Initialize soft config values at $0320 from ROM
;
; $FF51 INIT_2691       ;Initialize SCC2691 console 38.4K, 8-N-1 RTS/CTS
; $FF54 RESET_2691      ;Reset SCC2691 - called before INIT_2691
;
; $FF57 MONWARM         ;Monitor warm start - jumps to page $03
; $FF5A MONCOLD         ;Monitor cold start - jumps to page $03
; $FF5D COLDSTRT        ;System cold start - RESET vector for 65C02
;**************************************************************************************************
;               Data In and Out routines for Console I/O buffer                                   *
;**************************************************************************************************
;Data Input routines
;CHRIN_NW uses CHRIN, returns if data is not available from the buffer with carry flag clear
; else returns with data in A Reg and carry flag set. CHRIN waits for data to be in the
; buffer, then returns with carry flag set. Receive is IRQ driven/buffered with a size of 128 bytes
;
CHRIN_NW        CLC                     ;Clear Carry flag for no data (2)
                LDA     ICNT            ;Get buffer count (4)
                BNE     GET_CH          ;Branch if buffer is not empty (2/3)
                RTS                     ;or return to caller (6)
;
CHRIN           LDA     ICNT            ;Get data count (3)
                BEQ     CHRIN           ;If zero (no data, loop back) (2/3)
;
GET_CH          PHY                     ;Save Y Reg (3)
                LDY     IHEAD           ;Get the buffer head pointer (3)
                LDA     IBUF,Y          ;Get the data from the buffer (4)
                INC     IHEAD           ;Increment head pointer (5)
                RMB7    IHEAD           ;Strip off bit 7, 128 bytes only (5)
                DEC     ICNT            ;Decrement the buffer count (5)
;
                PLY                     ;Restore Y Reg (4)
                SEC                     ;Set Carry flag for data available (2)
                RTS                     ;Return to caller with data in A Reg (6)
;
;Data Output routine: puts the data in the A Reg into the xmit buffer, data in
; A Reg is preserved on exit. Transmit is IRQ driven/buffered with a size of 128 bytes
;
CHROUT          PHY                     ;save Y Reg (3)
OUTCH           LDY     OCNT            ;get data output count in buffer (3)
                BMI     OUTCH           ;check against limit, loop back if full (2/3)
;
                LDY     OTAIL           ;Get the buffer tail pointer (3)
                STA     OBUF,Y          ;Place data in the buffer (5)
                INC     OTAIL           ;Increment Tail pointer (5)
                RMB7    OTAIL           ;Strip off bit 7, 128 bytes only (5)
                INC     OCNT            ;Increment data count (5)
;
                LDY     #%00000100      ;Get mask for xmit on (2)
                STY     UART_COMMAND    ;Turn on xmit (4)
;
                PLY                     ;Restore Y Reg (4)
                RTS                     ;Return to caller (6)
;
;**************************************************************************************************
;Delay Routines: SET_DLY sets up the MSDELAY value and also sets the 16-bit Long Delay
; On entry, A Reg = 10-millisecond count, X Reg = High multiplier, Y Reg = Low multiplier
; these values are used by the EXE_MSDLY and EXE_LGDLY routines. Minimum delay is 10ms
; values for MSDELAY are $00-$FF ($00 = 256 times)
; values for Long Delay are $0000-$FFFF (0-65535 times MSDELAY)
; longest delay is 65,535*256*10ms = 16,776,960 * 0.01 = 167,769.60 seconds
;
;NOTE: All delay execution routines preserve registers (EXE_MSDLY, EXE_LGDLY, EXE_XLDLY)
;
SET_DLY         STA     SETMS           ;Save Millisecond count
                STY     DELLO           ;Save Low multiplier
                STX     DELHI           ;Save High multiplier
                RTS                     ;Return to caller
;
;EXE MSDELAY routine is the core delay routine. It sets the MSDELAY count value from the
; SETMS variable, enables the MATCH flag, then waits for the MATCH flag to clear
;
EXE_MSDLY       PHA                     ;Save A Reg
                SMB7    MATCH           ;Set MATCH flag bit
                LDA     SETMS           ;Get delay seed value
                STA     MSDELAY         ;Set MS delay value
;
MATCH_LP        BBS7    MATCH,MATCH_LP  ;Test MATCH flag, loop until cleared
                PLA                     ;Restore A Reg
                RTS                     ;Return to caller
;
;EXE LONG Delay routine is the 16-bit multiplier for the MSDELAY routine.
; It loads the 16-bit count from DELLO/DELHI, then loops the MSDELAY routine until the
; 16-bit count is decremented to zero.
;
EXE_LGDLY       PHX                     ;Save X Reg
                PHY                     ;Save Y Reg
                LDX     DELHI           ;Get high byte count
                INX                     ;Increment by one (checks for $00 vs $FF)
                LDY     DELLO           ;Get low byte count
                BEQ     SKP_DLL         ;If zero, skip to high count
DO_DLL          JSR     EXE_MSDLY       ;Call millisecond delay
                DEY                     ;Decrement low count
                BNE     DO_DLL          ;Branch back until done
;
SKP_DLL         DEX                     ;Decrement high byte index
                BNE     DO_DLL          ;Loop back to D0_DLL (will run 256 times)
                PLY                     ;Restore Y Reg
                PLX                     ;Restore X Reg
                RTS                     ;Return to caller
;
;EXE EXTRA LONG Delay routine uses XDL variable as an 8-bit count and calls the EXE LONG Delay
; routine XDL times. On entry, XDL contains the number of iterations This can increase the delay
; by 256 times the above value of 167,769.60 seconds (~497 Days). Note: $00 = 256 times
;
EXE_XLDLY       JSR     EXE_LGDLY       ;Call the Long Delay routine
                DEC     XDL             ;Decrement count
                BNE     EXE_XLDLY       ;Loop back until XDL times out
                RTS                     ;Return to caller
;
;**************************************************************************************************
;COUNTER BENCHMARK TIMING ROUTINES
; To enable a level of benchmarking, three new routines have been added to C02BIOS version 2.05
; Using the existing 10ms Jiffy Clock, three bytes of Page zero are used to hold the variables;
; MS10_CNT - a 10ms count variable for 0.01 resolution of timing - resets at 100 counts (1 second)
; SECL_CNT - a low byte seconds count
; SECH_CNT - a high byte seconds count
; This provides up to 65,535.99 seconds of timing with 0.01 seconds resolution
; - the count variables reset to zero after 65,535.99 seconds!
;
; New routines in C02Monitor version 2.05:
; - Start counter (calls CNT_STRT) after prompting user
; - Stop counter (calls CNT_STOP) and displays the timing count as: XXXXX.XX Seconds.
;
; CNT_INIT has been added as a new BIOS call, effectively replacing CNT_STRT, as it does
; the same thing. CNT_STRT has been changed to only set the MATCH flag to restart the timing.
; This allows the Benchmark Timer to effectively be paused and resumed without resetting the
; the count variables. CNT_INIT has also been added to the BIOS JUMP table.
;
;CNT_INIT is used to zero the timing pointers and start the benchmark timing
;CNT_STRT is used to start the timing by setting bit 6 of the MATCH flag (new in BIOS 3.03)
;CNT_STOP is used to stop the timing by clearing bit 6 of the MATCH flag
; the interrupt handler for the UART timer increments the timing variables when bit 6 of the
; MATCH flag is active.
;
CNT_INIT        RMB6    MATCH           ;Clear bit 6 of MATCH flag, ensure timing is disabled
                STZ     MS10_CNT        ;Zero 10ms timing count
                STZ     SECL_CNT        ;Zero low byte of seconds timing count
                STZ     SECH_CNT        ;Zero high byte of seconds timing count
;
CNT_STRT        SMB6    MATCH           ;Set bit 6 of MATCH flag to enable timing
                RTS                     ;Return to caller
;
CNT_STOP        RMB6    MATCH           ;Clear bit 6 of MATCH flag to disable timing
                RTS                     ;Return to caller
;
;**************************************************************************************************
;START of IDE Routines for Compact Flash adapter                                                  *
;**************************************************************************************************
;
IDE_READ_LBA                            ;Read a Block of data from IDE device
;
; This routine requires loading the requested LBA into the appropriate registers and
; issuing the READ command 20h. The LBA count supported for the BIOS are bits 0-23,
; so bits 24-27 are always set to 0. This provides access to IDE devices up to 8GB.
;
; Once the registers/parameters are setup, the Read Block command is issued.
; This results in an interrupt being generated. The ISR handles the transfer of LBA
; data from the CF Card to memory.
;
; The registers used are the same for read/write/verify. These are:
;
;       IDE_COMMAND = function requested (20h = READ LBA command)
;       IDE_DRV_HEAD = (Upper 4 bits) used as:
;               bit 7 = 1 per Sandisk documentation
;               bit 6 = 1 for LBA mode
;               bit 5 = 1 per Sandisk documentation
;               bit 4 = 0 for Drive 0
;       IDE_DRV_HEAD = LBA Address bits 27-24 (lower 4 bits) - not used, always 0000
;       IDE_CYL_HIGH = LBA Address bits 23-16
;       IDE_CYL_LOW = LBA Address bits 15-8
;       IDE_SCT_NUM = LBA Address bits 7-0
;       IDE_SCT_CNT = number of blocks to read (most CF-Cards are limited to 1)
;
                JSR     IDE_SET_PARMS   ;Setup required parameters (6)
                LDA     #$20            ;Get Read LBA command (2)
IDENT_READ                              ;Identity Command jumps to here to complete
                SMB3    MATCH           ;Set Read LBA bit (5)
                STA     IDE_COMMAND     ;Send command to IDE Controller (4)
;
LBA_RD_CMD
                LDA     IDE_ALT_STATUS  ;Get IDE Alternate Status register (4)
                BMI     LBA_RD_CMD      ;Loop until IDE controller not Busy (2/3)
;
LBA_RD_WAIT
                LDA     IDE_ALT_STATUS  ;Get IDE Alternate Status register (4)
                CMP     #$50            ;Compare for ready (2)
                BNE     LBA_RD_ERR      ;If not, check for error condition (2/3)
LBA_RD_OK
                BBS3    MATCH,LBA_RD_OK ;Wait for Read completed via ISR (5)
                RTS                     ;Return to caller (status in A Reg) (6)
LBA_RD_ERR
                LSR     A               ;Shift error bit to carry (2)
                BCC     LBA_RD_WAIT     ;If clear, loop back and continue waiting (2/3)
;
                RMB3    MATCH           ;Reset Read LBA bit (no ISR invoked) (5)
                LDA     IDE_ALT_STATUS  ;Get IDE Alternate Status register (4)
                STA     IDE_STATUS_RAM  ;Update RAM Status Register (3)
                RTS                     ;Return to caller (6)
;
IDE_WRITE_LBA                           ;Write a block of data to LBA
;
; This routine requires loading the requested LBA into the appropriate registers and
; issuing the WRITE command 30h. The LBA count supported for the BIOS are bits 0-23,
; so bits 24-27 are always set to 0. This provides access to IDE devices up to 8GB.
;
; The registers used are the same for read/write/verify. These are:
;
;       IDE_COMMAND = function requested (30h = WRITE LBA command)
;       IDE_DRV_HEAD = (Upper 4 bits) used as:
;               bit 7 = 1 per Sandisk documentation
;               bit 6 = 1 for LBA mode
;               bit 5 = 1 per Sandisk documentation
;               bit 4 = 0 for Drive 0
;       IDE_DRV_HEAD = LBA Address bits 27-24 (lower 4 bits) - not used, always 0000
;       IDE_CYL_HIGH = LBA Address bits 23-16
;       IDE_CYL_LOW = LBA Address bits 15-8
;       IDE_SCT_NUM = LBA Address bits 7-0
;       IDE_SCT_CNT = number of blocks to read (most CF-Cards are limited to 1)
;
                JSR     IDE_SET_PARMS   ;Setup required parameters (6)
;
                SMB2    MATCH           ;Set Write LBA bit (5)
                LDA     #$30            ;Get Write LBA command (2)
                STA     IDE_COMMAND     ;Send command to IDE Controller (4)
LBA_WR_CMD
                LDA     IDE_ALT_STATUS  ;Get IDE Alternate Status register (4)
                BMI     LBA_WR_CMD      ;Loop until IDE controller not Busy (2/3)
                LSR     A               ;Shift Error bit into Carry flag (2)
                BCS     IDE_WRITE_ERR   ;If Carry set, IDE error (2/3)
;
; Write Block routine integrated into IDE_WRITE_LBA
;
; - High byte needs to be loaded into the latch before the
;   low byte is loaded into the Data Register!
;
IDE_WRITE_BLK                           ;Write a block of data
                PHY                     ;Save Y reg
                LDY     #$01            ;Set offset for high byte latch
;
IDE_WRITE_LOOP
                LDA     IDE_ALT_STATUS  ;Get IDE Alternate Status register
                AND     #%00001000      ;Check for DRQ active
                BEQ     IDE_WR_FIN      ;If not active, exit (below)
IDE_WR_WBLK
                LDA     (BIOS_XFERL),Y  ;Get first byte of buffer + 1
                STA     IDE_16_WRITE    ;Place into high byte latch
                LDA     (BIOS_XFERL)    ;Get first byte of buffer
                STA     IDE_DATA        ;Write buffer to IDE (writes a word)
;
; - Buffer index needs to be incremented twice
;
                INC     BIOS_XFERL      ;Increment pointers once
                BNE     IDE_WR_BLK1
                INC     BIOS_XFERH
IDE_WR_BLK1
                INC     BIOS_XFERL      ;Increment pointers again
                BNE     IDE_WRITE_LOOP
                INC     BIOS_XFERH
IDE_WR_BLK2
                BRA     IDE_WRITE_LOOP  ;Loop back for 256 words
;
IDE_WR_FIN      JSR     TST_IDE_RDY     ;Wait for IDE Controller ready
;
WR_WAIT
                BBS2    MATCH,WR_WAIT   ;Wait for Write completed via ISR
                PLY                     ;Restore Y reg
                RTS                     ;Return to caller
;
IDE_WRITE_ERR
                LDA     IDE_ALT_STATUS  ;Get IDE Alternate Status register (4)
                STA     IDE_STATUS_RAM  ;Save Status Register (3)
                RMB2    MATCH           ;Reset Write LBA bit (no ISR) (5)
                RTS                     ;Return to caller (6)
;
IDE_VERIFY_LBA                          ;Verify LBA after write
;
; This routine requires loading the requested LBA into the appropriate registers and
; issuing the VERIFY command 40h. The LBA count supported for the BIOS are bits 0-23,
; so bits 24-27 are always set to 0. This provides access to IDE devices up to 8GB.
;
; Note: The Verify function here is a feature of the CF Card controller.
; It is identical to a Read Block command except no Data is transferred, only verified.
; Also, no DRQ is set, but an interrupt is generated after the Verify is completed.
;
; The registers used are the same for read/write/verify. These are:
;
;       IDE_COMMAND = function requested (40h = Verify LBA command)
;       IDE_DRV_HEAD = (Upper 4 bits) used as:
;               bit 7 = 1 per Sandisk documentation
;               bit 6 = 1 for LBA mode
;               bit 5 = 1 per Sandisk documentation
;               bit 4 = 0 for Drive 0
;       IDE_DRV_HEAD = LBA Address bits 27-24 (lower 4 bits) - not used, always 0000
;       IDE_CYL_HIGH = LBA Address bits 23-16
;       IDE_CYL_LOW = LBA Address bits 15-8
;       IDE_SCT_NUM = LBA Address bits 7-0
;       IDE_SCT_CNT = number of blocks to read (most CF-Cards are limited to 1)
;
                JSR     IDE_SET_PARMS   ;Setup required parameters
;
                SMB1    MATCH           ;Set Verify LBA bit
                LDA     #$40            ;Get Verify LBA command
                STA     IDE_COMMAND     ;Send command to IDE Controller
LBA_VF_CMD
                LDA     IDE_ALT_STATUS  ;Get IDE Alternate Status register
                BMI     LBA_VF_CMD      ;Loop until IDE controller not Busy
                LSR     A               ;Shift Error bit into Carry flag
                BCC     VF_FINISH       ;If Carry clear, no error
;
                LDA     IDE_ALT_STATUS  ;Get IDE Alternate Status register
                STA     IDE_STATUS_RAM  ;Save Status Register
                RMB1    MATCH           ;Reset Verify LBA bit (no ISR)
                RTS                     ;Return to caller
;
VF_FINISH
                JSR     TST_IDE_RDY     ;Wait for IDE Controller ready
VF_WAIT
                BBS1    MATCH,VF_WAIT   ;Wait for Verify to complete via ISR
                RTS                     ;Return to caller
;
IDE_SET_ADDRESS                         ;Set Address for LBA read or write
;
; This routine uses the A,Y,X registers to setup the address in memory that a block
; will be read to or written from (16-bit address), along with the block count.
; The Register usage is as follows:
;       A Register = Memory address low byte
;       Y Register = Memory address high byte
;       X Register = Block count to transfer (device dependent, most CF Cards = 1)
                STA     LBA_ADDR_LOW    ;Set LBA low byte address (3)
                STY     LBA_ADDR_HIGH   ;Set LBA high byte address (3)
                STX     LBA_XFER_CNT    ;Set LBA Block count for xfer (3)
                RTS                     ;Return to caller (6)
;
IDE_SET_LBA                             ;Set the LBA block for transfer (read/write)
;
; This routine sets the variables used to select the starting LBA for transfer.
; The Register usage is as follows:
;       A Register = LBA Address bits 7-0
;       Y Register = LBA Address bits 15-8
;       X Register = LBA Address bits 23-16
                STA     LBA_LOW_BYTE    ;Store Address bits 0-7 (3)
                STY     LBA_HIGH_BYTE   ;Store Address bits 8-15 (3)
                STX     LBA_EXT_BYTE    ;Store Address bits 16-23 (3)
                RTS                     ;Return to caller (6)
;
; This routine sets the LBA number used for all transfers.
; - The IDE Controller is checked first to ensure it's ready to receive parameters
; - then the requested LBA (stored in Page Zero variables) are loaded into the
; - IDE Controller registers, followed by the required Mode parameters.
; - Last, the transfer address is setup which points to the location in memory that
; - will be used to transfer Data to or from.
;
IDE_SET_PARMS                           ;Set All parameters for LBA transfers
;
                LDA     IDE_ALT_STATUS  ;Get IDE Alternate Status register (4)
                BMI     IDE_SET_PARMS   ;Loop until BUSY bit is clear (2/3)
;
                LDA     LBA_EXT_BYTE    ;Set LBA bits 23-16 (3)
                STA     IDE_CYL_HIGH    ;Send to IDE (4)
                LDA     LBA_HIGH_BYTE   ;Set LBA bits 15-8 (3)
                STA     IDE_CYL_LOW     ;Send to IDE (4)
                LDA     LBA_LOW_BYTE    ;Get LBA bits 7-0 (3)
                STA     IDE_SCT_NUM     ;Send to IDE (4)
                LDA     LBA_XFER_CNT    ;Get Block count to read (CF always 1) (3)
                STA     IDE_SCT_CNT     ;Send to IDE (4)
;
IDE_SET_PARMS2                          ;Set partial parameters (non LBA xfer commands)
;
                LDA     #%11100000      ;Set Drive 0, LBA mode, LBA bits 27-24 as 0 (2)
                STA     IDE_DRV_HEAD    ;Send to IDE (4)
;
                LDA     LBA_ADDR_LOW    ;Setup buffer address (3)
                STA     BIOS_XFERL      ;Store low byte (3)
                LDA     LBA_ADDR_HIGH   ;Block Buffer Address (3)
                STA     BIOS_XFERH      ;Store high byte (3)
                STZ     IDE_STATUS_RAM  ;Clear RAM Status Register, ISR updates it (3)
                RTS                     ;Return to caller (6)
;
;Test for IDE Controller Ready
; This routine tests that the IDE Controller (CF Card) is ready and can accept a command
; for execution. It turns out that two bits in the status register must be tested to ensure
; the card is ready... testing only one flag is unreliable and results in random failures
; in data transfers.
;
TST_IDE_RDY
                LDA     IDE_ALT_STATUS  ;Get IDE Alternate Status register (4)
                CMP     #$50            ;Mask off Ready bits (2)
                BNE     TST_IDE_RDY     ;Loop back until set (ready) (2/3)
                RTS                     ;Return to caller (6)
;
;Test for IDE Controller Busy
; This routine loops on the Busy flag. If the IDE Controller is busy, no other
; status register flags are valid and no commands can be sent to the IDE Controller.
; Hence, this routine is key to determine if the IDE Controller is available to
; accept a command. Bit 7 is the Busy Bit. The 65C02 will set the "n" flag
; if Bit 7 is active, else clear it.
;
TST_IDE_BUSY
                LDA     IDE_ALT_STATUS  ;Get IDE Alternate Status register (4)
                BMI     TST_IDE_BUSY    ;Loop until BUSY bit is clear (2/3)
                RTS                     ;Return to Caller (6)
;
;**************************************************************************************************
;START of Realtime Clock Routines for Compact Flash adapter                                       *
;**************************************************************************************************
;
;RTC Routines to Read and Write Non-Volatile RAM - 256 Bytes
; To use either routine, the starting memory address needs to be
; loaded into the A/Y registers. A reg = low byte, Y reg = high byte
;
RTC_NVRD        JSR     RTC_NVSET       ;Setup for transfer
RTC_RD_LOOP     LDA     RTC_RAM_DATA    ;Get NVRAM Data
                STA     (BIOS_XFERL),Y  ;Save into RAM
                INY                     ;Increment Y Index
                BNE     RTC_RD_LOOP     ;Loop back reading NVRAM
                BRA     CLEAR_BMI       ;Clear Burst Mode, exit
;
RTC_NVWR        JSR     RTC_NVSET       ;Setup for transfer
RTC_WR_LOOP     LDA     (BIOS_XFERL),Y  ;Get RAM
                STA     RTC_RAM_DATA    ;Write to NVRAM Data
                INY                     ;Increment Y Index
                BNE     RTC_WR_LOOP     ;Loop back writing NVRAM
CLEAR_BMI       LDA     #%00100000      ;Get Burst Mode Increment bit
                TRB     RTC_CONTROL_B   ;Clear Burst Mode for NVRAM
                RTS                     ;Return to caller
;
RTC_NVSET       STA     BIOS_XFERL      ;Save Low Address location
                STY     BIOS_XFERH      ;Save High Address location
                LDA     #%00100000      ;Get Burst Mode Increment bit
                TSB     RTC_CONTROL_B   ;Set Burst Mode for NVRAM
                STZ     RTC_RAM_ADDR    ;Set NVRAM Address to Zero
                LDY     #$00            ;Zero Y reg - Index
                RTS                     ;Return to caller
;
;**************************************************************************************************
;START OF PANIC ROUTINE
;The Panic routine is for debug of system problems, i.e., a crash. The hardware design requires a
; debounced NMI trigger button which is manually operated when the system crashes or malfunctions.
;
;Starting with C02BIOS 3.03, pages are no longer saved! This is the result of adding additional
; hardware devices which require additional buffer space as:
; - Second UART Port Receive/Transmit buffer (future reserve) at Page $04
; - DS1511 Realtime Clock NVRAM buffer at Page $05
; - IDE Controller device LBA buffer at Pages $06 and $07
;
; User presses the NMI (panic) button. The NMI vectored routine will perform the following tasks:
; 1- Save registers in page zero locations
; 2- Clear all Console I/O buffer pointers
; 3- Call the ROM routines to init the vectors and config data (page $03)
; 4- Call the ROM routines to reset/init the Console UART (SCC2691)
; 5- Enter the Monitor via the warm start vector
;
; Note: The additional hardware detection (RTC-IDE) are NOT executed with the Panic routine!
; The interrupt vectors are restored without including the additional ISR for the IDE controller.
;
; Note: no memory is cleared except the required pointers/vectors to restore the system.
;
;Note: it's possible to lockup the SCC2691 so that the Reset/Init routines can not recover it.
; The problem is one of two possibilities:
;  1- The BRG Test mode has been toggled via a read of the BRG Test Register. * - see below.
;  2- The X1/X16 Test mode has been toggled via a read of the X1/X16 Test Register.
;
;The first scenario can be avoided if the baud rate is setup for 19.2K or 38.4K, as the test mode
; uses the same baud rate as normal mode. Note that this does not lock up the UART in any way, but
; simply changes the baud rate per the BRG Test mode table. A second read to the BRG Test mode
; register will change the baud rate back to normal. As a default, the baud rate is set for 38.4K.
;
; NOTE: This is no longer a problem due to extended baud rates now supported starting in BIOS 2.05!
; New default baud rate is now set to 115.2K.
;
;The second scenario is more difficult to workaround. There's no telling if a read was done to the
; X1/X16 Test Mode Register. There are only two options to correct this:
;  1- a second read of the X1/X16 Test Mode Register
;  2- a hardware Reset applied to the UART.
; In the interest of being able to use the NMI Panic routine, the code does a read of the X1/X16
; Test Mode Register. If pressing the Panic button does not restore the UART, pressing it a second
; time might, unless the NMI vector has been changed or corrupted!
; NOTE: See below for a change in accessing this register in the NMI routine!
;
;NOTE: The X1/X16 Test mode is toggled via the INIT_2691 routine below! This is the result of an
; anomaly in the W65C02 as explained in the Init section below. This is noted here for awareness
; only. Please read the text below preceding the initialization routines for more detail!
;
NMI_VECTOR      STA     AREG            ;Save A Reg
                STX     XREG            ;Save X Reg
                STY     YREG            ;Save Y Reg
                PLA                     ;Get Processor Status
                STA     PREG            ;Save in PROCESSOR STATUS preset/result
                TSX                     ;Get Stack pointer
                STX     SREG            ;Save STACK POINTER
                PLA                     ;Pull RETURN address from STACK
                STA     PCL             ;Store Low byte
                PLA                     ;Pull high byte
                STA     PCH             ;Store High byte
;
                STZ     UART_IMR        ;Disable ALL interrupts from UART
                LDA     UART_STATUS     ;Get UART Status Register
                STA     UART_SRT        ;Save it in Page 0
                LDA     UART_ISR        ;Get UART Interrupt Status Register
                STA     UART_IRT        ;Save it in Page 0
;
;The following line of code can be considered optional. It's a work-around for an anomaly of
; the W65C02 which might toggle the X1/X16 Test Mode Register. In over a year of running
; 3- C02 Pocket SBCs, this problem has not yet been encountered, so it is commented out.
;
;               LDA     UART_CLKTEST    ;Toggle the X1/X16 Test mode
;
                LDX     #$08            ;Set count for 8
PAN_LP1         STZ     ICNT-1,X        ;Clear console I/O pointers/Status
                DEX                     ;Decrement index
                BNE     PAN_LP1         ;Branch back till done
;
                JSR     INIT_PG03       ;Xfer default Vectors/HW Config to $0300
                JSR     INIT_IO         ;Reset and Init the UART for Console
DO_NMI0         JMP     (NMIRTVEC0)     ;Jump to NMI Return Vector (Monitor Warm Start)
;
;**************************************************************************************************
;Initializing the SCC2691 UART as a Console.
;An anomaly in the W65C02 processor requires a different approach in programming the SCC2691
; for proper setup/operation. The SCC2691 uses two Mode Registers which are accessed at the same
; register in sequence. There is a command that Resets the Mode Register pointer (to MR1) that is
; issued first. Then MR1 is loaded followed by MR2. The problem with the W65C02 is a false read of
; the register when using indexed addressing (i.e., STA UART_REGISTER,X). This results in the Mode
; Register pointer being moved to the second register, so the write to MR1 never happens. While
; the indexed list works fine for all other register functions/commands, the loading of the
; Mode Registers need to be handled separately.
;
;NOTE: the W65C02 will function normally "if" a page boundary is crossed as part of the STA
; (i.e., STA $FDFF,X) where the value of the X Register is high enough to cross the page boundary.
; Programming in this manner would be confusing and require modification if the base I/O address
; is changed for a different hardware I/O map.
;
;The same anomaly in the W65C02 also creates a false read when sending any command to the Command
; Register (assuming indexed addressing), as the read function of that hardware register is the
; BRG Test Register. This can result in a different baud rate being selected, depending on the
; baud rate tables listed in the Datasheet. When using either 19.2K or 38.4K baud rate, the tables
; are the same for both normal and BRG Test mode, so the UART will operate normally. Changing to
; a different baud rate via the BRG Test Register requires additional coding to use any of the
; extended baud rates.
;
;There are two routines called to setup the 2691 UART:
;
;The first routine is a RESET of the UART.
; It issues the following sequence of commands:
;  1- Send a Power On command to the ACR
;  2- Reset Break Change Interrupt
;  3- Reset Receiver
;  4- Reset Transmitter
;  5- Reset All errors
;
;The second routine initializes the 2691 UART for operation. It uses two tables of data; one for
; the register offset and the other for the register data. The table for register offsets is
; maintained in ROM. The table for register data is copied to page $03, making it soft data. If
; needed, operating parameters can be altered and the UART re-initialized via the ROM routine.
;
; Updated CO2BIOS version to Ver. 2.01 in April 2018. Shorten INIT_IO routine by moving up the
; INIT_2691 to remove the "JMP INIT_2691", saves a few bytes and some clock cycles.
;
; Updated C02BIOS version to 2.05 provides for selecting extended baud rates. This is more of a
; band-aid approach for the SCC2691 as the BRG Test register needs to be accessed (READ) to
; toggle to the extended rates. To ensure this can be accomplished by soft config data only,
; a modification to the MR2 data is implemented. Bit 7 is generally a zero (0) as the SCC2691
; is configured for console use only. By setting Bit 7 to one (1), the BIOS config routine
; will determine if extended baud rates are requested. It also requires that Bit 7 is masked
; off when setting up the SCC2691 during power up, reset or a software config change.
;
; The BIOS init routine also uses MATCH bits 4,5 to track the status of the extended bit rates.
; Bit 5 is used to show that the current config requires the BRG Test register to be toggled to
; the extended bit rates. Bit 4 shows the current state of the BRG Test register.
; Bit 5 is set to 1 if extended baud are required.
; Bit 4 is set to 1 is extended baud rates are active.
; NOTE: changing bits 4,5 should be avoided. The Init routine should only be used when extended
; baud rates are required or any change is required.
;
; NOTE: The C02Monitor will also sense the active EBR status via Bit 4 when a Reset or Zero RAM
; is selected. This is required as a Reset or Zero RAM will clear out the MATCH flags bits and
; the status of the BRG Test register will be lost.
;
; Also realize that a hardware reset will rest the SCC2691 and the default ROM config will be
; initialized. Also note that the Panic routine invoked by a NMI trigger will also reset the
; UART to the default ROM config, but does not inquire or change any of the config data. It only
; calls the init routines, which will track the UART operating mode and handle as required.
;
INIT_IO         JSR     RESET_2691      ;Power-Up Reset of SCC2691 UART
                LDA     #DF_TICKS       ;Get divider for jiffy clock for 1-second
                STA     TICKS           ;Preload TICK count
;
;This routine sets the initial operating mode of the UART
;
INIT_2691       SEI                     ;Disable interrupts
                LDX     #INIT_DATAE-INIT_DATA ;Get the Init byte count
2691_INT        LDA     LOAD_2691-1,X   ;Get Data for 2691 Register
                LDY     INIT_OFFSET-1,X ;Get Offset for 2691 Register
                STA     SCC2691_BASE,Y  ;Store Data to selected register
                DEX                     ;Decrement count
                BNE     2691_INT        ;Loop back until all registers are loaded
;
; Mode Register is reset to MR1 by above INIT_2691
; MR2 data Bit 7 is used for extended baud rates... should always be "0" for normal config!
;
                LDA     LOAD_2691+13    ;Get Mode Register 1 Data
                STA     UART_MODEREG    ;Send to 2691
                LDA     LOAD_2691+14    ;Get Mode Register 2 Data
                BMI     SET_EBR         ;If Bit7 set, branch
                STA     UART_MODEREG    ;Else, Send MR2 data to 2691
                RMB5    MATCH           ;Turn off EBR Bit 5 in Match flag
                BBR4    MATCH,NO_EBR    ;If EBR off, no need to toggle off
                BIT     UART_BRGTST     ;Toggle BRG Test register (read)
                RMB4    MATCH           ;Turn off EBR active bit
NO_EBR          CLI                     ;Enable interrupts
                RTS                     ;Return to caller
;
SET_EBR         SMB5    MATCH           ;Set EBR bit in Match flag
                AND     #$7F            ;Mask off Bit 7 for EBR config
                STA     UART_MODEREG    ;Else, Send MR2 to 2691
;
                BBS4    MATCH,SKIP_BRG  ;If BRG Test toggled, skip
                BIT     UART_BRGTST     ;Else, toggle BRG Test register (read)
                SMB4    MATCH           ;Set Bit 4 fpr BRG Test toggled
SKIP_BRG        CLI                     ;Enables interrupts
                RTS                     ;Return to caller
;
;This routine does a Reset of the SCC2691
;
RESET_2691      LDA     #%00001000      ;Get Power On mask
                STA     UART_AUXCR      ;Send to 2691 (ensure it's on)
;
                LDX     #UART_RDATAE-UART_RDATA1 ;Get the Reset commands byte count
UART_RES1       LDA     UART_RDATA1-1,X ;Get Reset commands
                STA     UART_COMMAND    ;Send to UART CR
                DEX                     ;Decrement the command list index
                BNE     UART_RES1       ;Loop back until all are sent
                RTS                     ;Return to caller
;
;Core routines that are used to detect and configure additional I/O devices.
; The I/O adapter supported by the 3.0x Release of C02BIOS contains two I/O devices:
; - A Maxim DS1511 Realtime Clock
; - A Compact Flash Card configured in True IDE Mode (per SanDisk documentation)
;
; The first routine is to detect the DS1511 RTC. This is done by reading the NVRAM section
; and testing for the Signature at the end of NVRAM. Obviously, this is something that needs
; to be setup before it can be detected. There is a separate utility that is used to configure
; the RTC for Time/Date, NVRAM usage and the two-byte signature. This utility must be run first
; to properly configure the RTC. Once completed, the BIOS will detect the RTC and read the
; Date and Time to set the BIOS variables accordingly.
;
; The second routine is to detect the Compact Flash device. This can be a bit tricky...
; as reading the Status register may or may not be valid depending on the individual CF Card
; and it's internal timing to become ready after a hardware reset.
; Assuming the CF Card is present and attempting to execute a Reset or Self Diagnostic command
; could likely get stuck in a loop waiting for a bit change in the Status register.
; From testing, the most reliable method is trying to load the Status register and comparing it
; against the Phantom Address read when there is no device (or memory) at the decoded location.
; As the configured I/O Page is $FE, this value will be returned if a CF Card is not present.
; Also note that there is no valid condition of the Status register where all but the lowest
; order bits would be active. If a read returns other than $FE, additional tests are done to
; detect the CF Card and initialize it. Once completed, the ISR for the IDE controller is added
; to the interrupt handler via a reserved interrupt vector.
;
;This routine detects the DS1511 RTC.
; While there's really no standard method to detect the RTC, the NVRAM is initialized
; during the configuration for Date, Time, etc. The last two bytes of the NVRAM will
; contain a 2-byte signature of "KM", in honor of the author ;-)
; If the signature is found, the INIT_RTC routine is called, which will setup the
; RTC variables in Page Zero, where the correct Date and Time will be maintained.
;
DETECT_RTC
                LDX     #$FE            ;Load NVRAM Offset
                STX     RTC_RAM_ADDR    ;Set index to start of signature
                LDA     RTC_RAM_DATA    ;Get NVRAM Data
                CMP     #"K"            ;Check for "M"
                BNE     SET_EPOCH       ;If not found, set EPOCH as default
                INX                     ;Increment Offset
                STX     RTC_RAM_ADDR    ;Set index to start of signature
                LDA     RTC_RAM_DATA    ;Get NVRAM Data
                CMP     #"M"            ;Check for "K"
                BEQ     FOUND_RTC       ;If good, go Init RTC and exit
;
;If RTC not found, preload Date variables as EPOCH time (Thursday, 1st January, 1970)
; Note: Time variables default to zero from a cold start.
;
SET_EPOCH
                LDX     #$03            ;Get Index of 3
EPOCH_LP        LDA     EPOCH-1,X       ;Get Preload data
                STA     DAY_DATE-1,X    ;Store in RTC variables
                DEX                     ;Decrement Index
                BNE     EPOCH_LP        ;Loop back until done
                RTS                     ;Return, no RTC found
;
FOUND_RTC
                INX                     ;Increment Index ($FF -> $00)
FOUND_RTC_LP    LDA     RTC_MSG,X       ;Get BIOS init msg
                BEQ     INIT_RTC        ;If zero, msg done, go Init RTC
                JSR     CHROUT          ;Send to console
                INX                     ;Increment Index
                BRA     FOUND_RTC_LP    ;Loop back until done
;
;This routine reads the BCD registers from the DS1511 RTC, converts BCD to Binary and loads the
; BIOS RTC variables. Note that some of the variables are shared to conserve Page Zero space.
; Note: Buffer area used for init is also used by C02 Monitor for hex input and code conversion,
; but is initialized as needed before usage, so we can clobber it here when configuring the BIOS.
;
;As there is a period of time required to get the data from the RTC, convert from BCD, then format
; into the Page Zero locations used, we need to manage the existing Timer/Counter status to ensure
; the best accuracy in time once the routine finishes. In short, the best we can manage accuracy
; is within one second, as the RTC internal update of the registers we read are halted.
;
;To help minimize the timing transfer, we first set the tick count to it's maximum value of $FF
; to ensure that the software RTC values are not updated during loading from the hardware RTC.
; The time to detect and load the current time/date is less than 5000 clock cycles. However, the
; overall accuracy can only be within one second. After the RTC values are transferred, the
; software RTC tick count will be loaded with a minimal count to ensure the time is closer to
; what the hardware RTC is set for.
;
INIT_RTC
                LDX     #$FF            ;Get $FF for tick count (2)
                STX     TICKS           ;Set the Tick count (3)
                INX                     ;Increment Index ($FF -> $00) (2)
                LDA     #%10000000      ;Get TE Bit mask (2)
                TRB     RTC_CONTROL_B   ;Turn off TE Bit to disable update (6)
;
RTC_LOAD_LOOP
                LDA     RTC_SECONDS,X   ;Get Data from RTC (4)
                STA     BUFF_PG0,X      ;Store into RAM (4)
                INX                     ;Increment register count (2)
                CPX     #$08            ;Check for all 8 moved (2)
                BNE     RTC_LOAD_LOOP   :Loop back until all registers are read (2/3)
;
                LDA     #%10000000      ;Get TE Bit mask (2)
                TSB     RTC_CONTROL_B   ;Turn on TE Bit to enable update (6)
;
;Now convert BCD to Binary (X reg = $08)
;
BCD_LOOP        LDA     BUFF_PG0-1,X    ;Get BCD Data from Buffer (4)
;
                TAY                     ;Save BCD value (2)
                AND     #$F0            ;Mask for high nibble (2)
                LSR     A               ;Shift right (/2) (2)
                STA     RTC_TEMP        ;Save in temp (3)
                LSR     A               ;Shift over (/4) (2)
                LSR     A               ;Shift over (/8) -Carry is clear (2)
                ADC     RTC_TEMP        ;Add to temp (3)
                STA     RTC_TEMP        ;Store it back (3)
                TYA                     ;Get BCD back (2)
                AND     #$0F            ;Mask for low nibble (2)
                ADC     RTC_TEMP        ;Add temp (upper converted nibble) (3)
;
                STA     BUFF_PG0-1,X    ;Save Binary Data back to Buffer (4)
                DEX                     ;Decrement Index (2)
                BNE     BCD_LOOP        ;Loop back until done (2/3)
;
;Now take the binary data for the RTC, and format it for the BIOS RTC variables:
; Seconds, Minutes and Hours are fine as they are, so they simply move over.
; The Day (of the week) and the Date (of the Month) are combined into a single variable.
; Note: Day (of the week) is variable and decided by the user. In honor of my past, I'm
; declaring Saturday as the 1st day of the week, an IBM tradition.
; The Month is also kept in the upper 4 bits of the Century variable.
; Note: The Century and Year are two BCD digits each. As 4 BCD digits, these need to be
; converted to a 16-bit binary integer. Only 12-bits are needed, which frees the upper
; 4-bits to be used for the Month.
;
                LDA     BUFF_PG0+3      ;Get the Day count (3)
                ASL     A               ;Shift to upper 3 bits (2)
                ASL     A               ; (2)
                ASL     A               ; (2)
                ASL     A               ; (2)
                ASL     A               ; (2)
                ORA     BUFF_PG0+4      ;OR in the Date variable (3)
                STA     BUFF_PG0+3      ;Save to the Day variable (3)
;
                LDA     BUFF_PG0+5      ;Get the Month variable (3)
                ASL     A               ;Shift to upper 4 bits (2)
                ASL     A               ; (2)
                ASL     A               ; (2)
                ASL     A               ; (2)
                STA     BUFF_PG0+4      ;Save it (lower nibble = 0) (3)
;
; The easist way to create the 16-bit Year is to take the high binary digit
; and multiply it by 100, then add the low binary digit. We do the multiply
; by adding it to the 16-bit variable 100 times ;-)
;
                LDX     #100            ;Get the index count for 100 (2)
                STZ     BUFF_PG0+8      ;Zero Temp low byte (3)
                STZ     BUFF_PG0+9      ;Zero Temp high byte (3)
;
RTC_ADD         LDA     BUFF_PG0+7      ;Get Year value (3)
                JSR     ADD_16          ;Go add it... (6)
                DEX                     ;Decrement count (2)
                BNE     RTC_ADD         ;Loop back until done (2/3)
;
                LDA     BUFF_PG0+6      ;Get Year lower digit (3)
                JSR     ADD_16          ;Add it into the Temp (6)
;
                LDA     BUFF_PG0+8      ;Get lower byte for year (3)
                STA     BUFF_PG0+5      ;Save it (3)
                LDA     BUFF_PG0+9      ;Get upper byte for year (3)
                ORA     BUFF_PG0+4      ;OR in Month (upper 4 bits) (3)
                STA     BUFF_PG0+4      ;Save it (3)
;
; Now transfer the buffer contents to the Page Zero RTC variables
;
                LDX     #$06            ;Set count for 6 (2)
RTC_XFER        LDA     BUFF_PG0-1,X    ;Get the buffer data (4)
                STA     SECS-1,X        ;Save to RTC variables (4)
                DEX                     ;Decrement count (2)
                BNE     RTC_XFER        ;Loop back till done (2/3)
;
                LDA     #$10            ;Get a minimal tick count (2)
                STA     TICKS           ;Update RTC tick count (3)
                RTS                     ;Return to caller (6)
;
ADD_16          CLC                     ;Clear carry flag (2)
                ADC     BUFF_PG0+8      ;Add to Temp low byte (3)
                STA     BUFF_PG0+8      ;And save itm (3)
                LDA     BUFF_PG0+9      ;Get high byte (3)
                ADC     #$00            ;Add carry to Temp high byte (2)
                STA     BUFF_PG0+9      ;And save it (3)
                RTS                     ;Return to caller (6)
;
;This routine detects the IDE Port
; To detect the IDE controller (Compact Flash Card), it can be a bit tricky. It takes
; a fairly long time from a physical Reset of the CF-Card before the Busy flag goes off.
; During this time, any commands sent to the IDE controller will fail. As I like to say,
; "timing is everything". After quite a bit of testing, the easy way to detect the IDE
; controller present is to load the IDE_STATUS register. If a CF-Card is not plugged in,
; The A Reg will show a phantom address of $FE (high order IDE hardware address) and
; any initialization can be bypassed.
;
; If the IDE controller is present, the IDE_STATUS read may be invalid, so it's necassary
; to test the BUSY flag of the status register. Once the IDE Controller is no longer busy,
; the controller can be initialized. This does create an obvious pause in the startup, but
; ensures that the IDE controller can be reliably detected and initialized at boot time.
;
; A change has been made in the IDE_SETUP routine. As the interrupts were disabled on the
; CF Card initially, it needs to be re-enabled before the setup is completed. The short
; bit of code that does this is also called by the Reset-Diag function (JMP $FF00).
;
DETECT_IDE
                LDA     IDE_STATUS      ;Get the IDE Status
                CMP     #$FE            ;Check for an empty (phantom) address
                BNE     IDE_INIT        ;If not #$FE, try to Init the IDE controller
NO_IDE          RTS                     ;Return to caller, no IDE controller found
;
;Init the IDE controller
;
IDE_INIT        JSR     TST_IDE_BUSY    ;Check/wait if IDE is still busy after HW Reset
                JSR     IDE_RESET       ;Reset the IDE Controller
                CMP     #$50            ;Check for $50 on A reg (drive ready)
                BNE     NO_IDE          ;If not, no IDE present
                CPX     #$00            ;X Reg will show #$00 if successful
                BNE     NO_IDE          ;If failed, exit
;
                JSR     IDE_DIAG        ;Run the self diagnostic
                CMP     #$50            ;Check for $50 on A reg (drive ready)
                BNE     NO_IDE          ;If not, no IDE present
                CPX     #$01            ;X Reg will show #$01 if diags successful
                BNE     NO_IDE          ;If failed, exit
;
; IDE Controller found and passed initial self diagnostics test.
; Send IDE Found message to terminal
;
                LDX     #$00            ;Zero X Reg for index to msg
IDE_MSG_LP      LDA     IDE_MSG,X       ;Get BIOS init msg
                BEQ     IDE_SETUP       ;If zero, msg done, go setup IDE
                JSR     CHROUT          ;Send to console
                INX                     ;Increment Index
                BRA     IDE_MSG_LP      ;Loop back until done
;
;IDE Setup
; This will insert the IDE Controller ISR into the Interrupt Handler chain.
;
; First, disable interrupts, capture the current IRQ exit vector address
; and save it to the first Insert Vector. Second, load the IDE ISR routine
; address and store it to the main IRQ exit vector, then re-enable interrupts.
;
; Second, this routine will execute an Identify IDE command to load the Soft
; Config Data for the maximum LBA Count accessible by the current IDE device
;
; Note: Two routines are below, use only one! They allow a choice to insert
; the IDE ISR before or after the main Interrupt Handler. By default, loading
; is after the main ISR. Despite the high speed of the data transfers, the
; interrupt rate is not that high.
;
IDE_SETUP       SEI                     ;Disable interrupts
;
; To load the IDE ISR Handler AFTER the existing UART ISR Handler:
;
;                LDA     IRQRTVEC0       ;Get low byte of IRQ Exit
;                LDY     IRQRTVEC0+1     ;Get high byte of IRQ Exit
;                STA     VECINSRT0       ;Save low byte of IRQ Exit
;                STY     VECINSRT0+1     ;Save high byte of IRQ Exit
;
;                LDA     #<INTERUPT1     ;Get low byte of IDE ISR
;                LDY     #>INTERUPT1     ;Get high byte of IDE ISR
;                STA     IRQRTVEC0       ;Save low byte of IRQ Exit
;                STY     IRQRTVEC0+1     ;Save high byte of IRQ Exit
;
; To load the IDE ISR Handler BEFORE the existing UART ISR Handler:
;
                LDA     IRQVEC0         ;Get low byte of IRQ Exit
                LDY     IRQVEC0+1       ;Get high byte of IRQ Exit
                STA     VECINSRT0       ;Save low byte of IRQ Exit
                STY     VECINSRT0+1     ;Save high byte of IRQ Exit
;
                LDA     #<INTERUPT1     ;Get low byte of IDE ISR
                LDY     #>INTERUPT1     ;Get high byte of IDE ISR
                STA     IRQVEC0         ;Save low byte of IRQ Exit
                STY     IRQVEC0+1       ;Save high byte of IRQ Exit
;
                CLI                     ;Enable interrupts
;
                JSR     IDE_EN_IRQ      ;Enable CF-Card interrupt
;
; Drop into Identify Drive routine
;
IDE_IDENTIFY                            ;Identify Device
;
; This requests a 512-byte block of data that shows capabilities, CHS (not used), LBA Count, etc.
; The format is similar to Read LBA, except no LBA parameter is required. It effectively works as
; a Read Block operation and the data transferred is handled by the ISR for Read Block.
; NOTE: The Identify Command is coded to load into LBA_BUFFER (address $0600).
;
                LDA     #<LBA_BUFFER    ;Set Address low byte
                LDY     #>LBA_BUFFER    ;Set Address high byte
                LDX     #$01            ;Set Block count to 1
                JSR     IDE_SET_ADDRESS ;Set Xfer address and block count
;
                JSR     TST_IDE_BUSY    ;Wait for IDE Controller not busy
                JSR     IDE_SET_PARMS2  ;Setup required parameters (no LBA parameter)
;
                LDA     #$EC            ;Get Identify Command
                JSR     IDENT_READ      ;Use READ_LBA routine to finish read
;
; Check for LBA mode supported by looking at Word 49 offset bit 9 active
;
                LDA     LBA_BUFFER+$63  ;Get Capabilities data (bit 9 of Word 49)
                LSR     A               ;Shift DMA bit to Carry
                LSR     A               ;Shift LBA bit to Carry
                BCS     LBA_VALID       ;If active, finish setup
;
; Else, send an error messsage to console for LBA mode unsupported
;
                LDX     #$00            ;Set index to zero
NO_LBA_LP       LDA     NO_LBA,X        ;Get LBA unsupported message
                BEQ     NO_IDE          ;If done, exit
                JSR     CHROUT          ;Send to console
                INX                     ;Increment index
                BRA     NO_LBA_LP       ;Loop back until done
;
; Identify data loaded in buffer. Now extract LBA count and store to Soft Config Data for usage
; by access routines (Read/Write/Verify). Four bytes are used and the format from the Identify
; Command are Low-order Word / High-order Word, where each word is in Big Endian. We will store
; the LBA count as Little Endian, Low-order Word / High-order Word.
; The offset from the buffer are Words 60-61 (decimal).
;
; A table is used to index the offset of bytes to move into consecutive soft data.
;
LBA_VALID       LDX     #$04            ;Set count for 4 bytes
LBA_SIZE        LDY     LBA_OFFSET-1,X  ;Get Offset to LBA count
                LDA     (LBA_ADDR_LOW),Y        ;Load LBA Data
                STA     LOAD_IDE-1,X    ;Store to Soft Config Data
                DEX                     ;Decrement count
                BNE     LBA_SIZE        ;Loop back until done
                RTS                     ;Return to caller
;
IDE_RES_DIAG                            ;Do a Reset and run Diagnostics
                JSR     IDE_RESET       ;Call IDE_RESET (set LBA mode)
                JSR     IDE_DIAG        ;Call IDE Self Diagnostics
                JSR     IDE_EN_IRQ      ;Re-enable CF-Card interrupt
;
; Drop into Get Status routine
;
IDE_GET_STATUS                          ;Get Status / Extended error code from the IDE controller
;
; This routine gets the current status of the IDE Controller and can be issued at any time.
; It does not rely on any interrupt capability as it's a simple command issue and reads the
; status from the IDE Controller.
;
; Note: This routine should be invoked whenever an Error has occurred, as it returns the
; extended error code caused by the previous failed command. Error code details are listed
; in the SanDisk Documentation.
;
                JSR     TST_IDE_BUSY    ;Wait for IDE Controller not busy
                LDA     #%11100000      ;Set Drive 0, LBA mode and LBA 27-24 as 0 (lower 4 bits)
                STA     IDE_DRV_HEAD    ;Send to IDE Controller
;
                LDA     #$03            ;Get Request Sense command
                STA     IDE_COMMAND     ;Send command to IDE Controller
;
                JSR     TST_IDE_RDY     ;Wait for IDE Controller ready
                LDA     IDE_STATUS      ;Get IDE Status
                LDX     IDE_ERROR       ;Get any extended error code
                RTS                     ;Return to Caller
;
IDE_RESET                               ;A Recalibrate Command
;
;According to Sandisk, the Recalibrate command is essentially a NOP,
; as there are no moving parts and nothing to calibrate. However, there is
; the option of setting LBA mode by setting bit 6 in the mask for DRV-HEAD
; To send this command, a seperate routine is used to save ROM space.
;
                LDA     #$10            ;Get Reset Command
                BRA     IDE_SEND_CMD    ;Send Command and return
;
IDE_DIAG                                ;Run internal Diagnostics on the CF-Card controller
;
; This is a basic self test within the CF-Card controller... per SanDisk documentation.
; This runs some internal tests for the IDE controller and returns with drive ready bits
; active ($50) and the error register as $01 if successful.
; To send this command, a seperate routine is used to save ROM space.
;
                LDA     #$90            ;Get Diagnostic Command
;
; Drop into Send Command routine
;
IDE_SEND_CMD                            ;Send a Command to the IDE controller
;
;Accepts a Command code via the A reg and sets up the necassary CF Card
; registers to accept it. It also tests to ensure the controller is ready
; to accept the command and get the Status and Error registers on return.
;
; NOTE: this routine turns off the interrupt capability as it is called
; during initial setup, where the interrupt handler has not been setup yet.
; A seperate routine is called to enable the interrupt capability.
;
                SEI                     ;Disable Interrupts
                TAX                     ;Save Command to X Reg
                JSR     TST_IDE_RDY     ;Wait for IDE to be ready
                LDA     #%00001010      ;Get Mask to disable IRQ
                STA     IDE_DEV_CTRL    ;Send to control register
                LDA     #%11100000      ;Get Select Mask (LBA Mode, Drive 0)
                STA     IDE_DRV_HEAD    ;Select Drive 0
;
                TXA                     ;Get Command from X Reg
                STA     IDE_COMMAND     ;Send command to IDE
                JSR     TST_IDE_BUSY    ;Wait for drive Busy
;
                LDA     IDE_STATUS      ;Get IDE Status Register
                LDX     IDE_ERROR       ;Get IDE Error Register
                CLI                     ;Enable interrupts
                RTS                     ;Return to caller
;
; Enable Interrupts on the CF Card IDE Controller
; This needs to executed during initial setup and anytime the Reset/Diag BIOS function
; is called.
;
IDE_EN_IRQ                              ;Enable CF-Card interrupt
                JSR     TST_IDE_RDY     ;Wait for IDE to be ready
                LDA     #%00001000      ;Get Mask to enable IRQ
                STA     IDE_DEV_CTRL    ;Send to control register
                RTS                     ;Return to caller
;
;Interrupt 1 - This is the ISR which is responsible for servicing the IDE controller on the
; RTC-CF-Card adapter. The RTC does not actually require any ISR capabilities as no Alarm functions
; are being used in the BIOS. There are extra inserts which can be used if needed at a later date.
; The only functions that might make sense would be to add the Alarm function at a future date.
; Once the IDE controller BIOS has matured, if there's any room left in the allocated ROM area,
; I'll revisit it.
;
;The ISR for the IDE controller will handle the data transfer for LBA read/write/verify functions
; and handle any error functions. By design, the 16-bit Data Transfer feature is used for:
; Reading / Writing / Verifying of all LBA block data and the IDE Identification data.
;
;The 16-bit IOCS16 line was routed to the Set Overflow (SO) pin on the CPU, so it could be used in
; the data transfer routine as a handshake signal. Testing showed that the IOSC16 line goes active
; during a block transfer, but you still need to read the Status Register and test the DRQ bit.
; As a result, there's really no advantage in using the IOCS16 line.
;
;Update: I've opted to remove the jumper and connection from the IOCS16 line to the SO line to the
; CPU in the latest hardware adapter version. It's clear that there's no actual value in trying to
; use it, as the Status register must be read for each 16-bit data transfer, so any access of the
; IOCS16 line would just be overhead.
;
;The BIOS is using the Alternate Status register to determine if DRQ (Data Request) is set.
; This works as a handshake for 16-bit data transfers without issue. Note that the normal Status
; register resets the interrupt when read, so this is only done once in the ISR.
;
;Arrival here takes place after the first ISR for the SCC2691 UART has been serviced. If the UART
; did not generate the interrupt, it will be quickly be directed to this routine. Note that during
; the Init and Setup of the IDE controller, the Init routine will insert a new vector into the
; chain by taking the existing exit vector, replacing it to point here and the exit from this ISR
; points to the original exit vector. In the source code here, you can also REM out the interrupt
; insert and choose to load this ISR first. This will improve data transfer rate slightly.
;
;Update: This ISR has been moved to the front of the ISR chain, i.e., this ISR routine gets
; serviced first, then jumps to the next ISR, which services the UART. This makes a noticeable
; improvement in data transfer from the IDE controller. Note that overhead for this routine will
; add 33 clock cycles if it just exits (IDE controller did not generate an interrupt).
;
;To check if an interrupt has been generated by the IDE controller, the Alternate Status register
; can be read. This contains the same information as the standard Status register but will NOT
; reset the interrupt on the IDE controller. By reading the Alternate Status register first, we
; can first determine what the status of the IDE controller is and take action if required.
; Note that not all bit settings imply an interrupt was generated. Specifically, looking at the
; bit definitions below, Bits 6 and 4 are set when the IDE is ready, hence a normal condition
; where nothing requires any attention. Also, a Busy condition can imply the IDE controller is
; working on a command but may not have generated an interrupt yet. If The Busy bit (7) is set,
; then all other bits are invalid per SanDisk documentation, so we trigger on that first.
;
;One annoying feature for IDE is "when" interrupts are generated. For any Read operation, once
; the command has been accepted, data is placed into the CF-Card buffer, followed by generating
; an interrupt to the system. Once this is done, the system will read the data. By accessing the
; Status register, the interrupt will be reset. This is normal operation. For a write operation,
; The command is sent, then DRQ goes active, which requires the data be sent to the CF-Card.
; Once the data is written, an interrupt is generated after it's completed. As a result, there's
; really no useful function of having an ISR for servicing the write functions, unless the command
; is to write multiple blocks.... which most CF-Cards do not support a block transfer of more than
; one block. Still, as interrupts are active for the CF Card, we handle it here to ensure there
; are no errors and the calling routine doesn't attempt to write another block until the last one
; has been completed.
;
; Status Register bits as defined as follows:
;       - Bit 7 = Busy (a Command has been accepted)
;       - Bit 6 = Ready (IDE controller is ready to accept a command)
;       - Bit 5 = Write Fault (A write command failed against the media)
;       - Bit 4 = DSC (is set when the CF Card is ready)
;       - Bit 3 = Data Request (set when there is data to transfer, read or write)
;       - Bit 2 = Correction (set when a recoverable data error was corrected)
;       - Bit 1 = 0 (not used)
;       - Bit 0 = Error (set when the previous command had an unrecoverable error)
;
;       NOTE: 25 clock cycles to here if UART ISR is second!
;       NOTE: 40 clock cycles to here if NO UART interrupt occurs!
;
INTERUPT1                               ;Interrupt 1 (IDE)
                LDA     IDE_ALT_STATUS  ;Get Alternate Status Register (4)
                BMI     REGEXT01        ;If Busy bit active, just exit (2/3)
;
; - Check for Data Request (DRQ), as it's the only function that will require servicing
; against the IDE controller.
;
                LDA     IDE_STATUS      ;Get Status (resets IRQ) (4)
                AND     #%00001000      ;Check for DRQ (2)
                BNE     IDE_READ_BLK    ;Branch if active (2/3)
;
; - If no DRQ, possible Write or Verify Block command, check for these next.
;
                BBS2    MATCH,IDE_WRIT_BLK      ;If Bit 2 set, Write operation (5)
                BBS1    MATCH,IDE_VRFY_BLK      ;If Bit 1 set, Verify operation (5)
                JMP     (VECINSRT0)     ;Exit ISR handler, saves 3 clock cycles (6)
;
IDE_READ_BLK                            ;IDE Read a Block of data
;
;Note: This next instruction is required! Arrival here states that the DRQ bit in the
; status register is active, so a data transfer "is" in place. If it's not the result
; of a LBA Read command, then it's from a LBA Write command. Also realize that this ISR
; will be executed every time the UART generates an interrupt. This will happen every
; 10ms for the Jiffy-Clock timer and for character transmit and receive.
;
                BBR3    MATCH,REGEXT01  ;If Bit 3 not set, exit ISR, (Write Cmd) (5)
;
LBA_XFER        LDA     IDE_ALT_STATUS  ;Get Status (4)
                AND     #%00001000      ;Check for DRQ (2)
                BEQ     IDE_RD_DONE     ;If not active, done, exit (2/3)
;
IDE_RD_RBLK
                LDA     IDE_DATA        ;Read low byte (high byte in latch) (4)
                STA     (BIOS_XFERL)    ;Store low byte (5)
                INC     BIOS_XFERL      ;Increment pointers (5)
                BNE     IDE_RD_BLK1     ; (2/3)
                INC     BIOS_XFERH      ; (5)
IDE_RD_BLK1
                LDA     IDE_16_READ     ;Read high byte from latch (4)
                STA     (BIOS_XFERL)    ;Store high byte (5)
                INC     BIOS_XFERL      ;Increment pointers (5)
                BNE     LBA_XFER        ;Loop back to Xfer, saves 3 clock cycles (2/3)
                INC     BIOS_XFERH      ; (5)
IDE_RD_BLK2
                BRA     LBA_XFER        ;Loop back till no more DRQs (3)
;
IDE_RD_DONE     RMB3    MATCH           ;Clear Read Block flag (5)
;
IDE_ALL_DONE    LDA     IDE_ALT_STATUS  ;Get Alternate Status Register (4)
                STA     IDE_STATUS_RAM  ;Save it to RAM location (3)
REGEXT01        JMP     (VECINSRT0)     ;Exit ISR handler (6)
;
IDE_WRIT_BLK                            ;IDE Write a Block of data
                RMB2    MATCH           ;Clear Write Block flag (5)
                BRA     IDE_ALL_DONE    ;Branch and finish ISR (3)
;
IDE_VRFY_BLK                            ;IDE Verify a Block of data
                RMB1    MATCH           ;Clear Verify Block flag (5)
                BRA     IDE_ALL_DONE    ;Branch and finish ISR (3)
;
;**************************************************************************************************
;BRK/IRQ Interrupt service routines
;The pre-process routine located in page $FF soft-vectors to INTERUPT0/BRKINSTR0 below
;       These are the routines that handle BRK and IRQ functions
;       The BRK handler saves CPU details for register display
;       - A Monitor can provide a disassembly of the last executed instruction
;       - A Received Break is also handled here (ExtraPutty/Windows or Serial/OSX)
;
; SCC2691 handler
;       The 2691 IRQ routine handles Transmit, Receive, Timer and Received-Break interrupts
;       - Transmit and Receive each have a 128 byte circular FIFO buffer in memory
;       - Xmit IRQ is controlled (On/Off) by the handler and the CHROUT routine
; The 2691 Timer resolution is 10ms and used as a Jiffy Clock for RTC, delays and benchmarking
;
;**************************************************************************************************
;BIOS routines to handle interrupt-driven I/O for the SCC2691
;NOTE: MPI Pin is used for RTS, which is automatically handled in the chip. As a result,
; the upper 2 bits of the ISR are not used in the handler. The Lower 5 bits are used, but
; the lower two are used to determine when to disable transmit after the buffer is empty.
;
;The UART_ISR bits are defined as follows:
; 7- MPI Pin change             0=No, 1=Yes
; 6- MPI Pin current state      0=Low, 1=High
; 5- Unused                     Always Active=1
; 4- Counter Ready              0=No, 1=Yes
; 3- Delta Break                0=No, 1=Yes
; 2- RxRDY/Full                 0=No, 1=Yes
; 1- TxEMT                      0=No, 1=Yes
; 0- TXRDY                      0=No, 1=Yes
;
;**************************************************************************************************
;
UART_RCV        LDY     ICNT            ;Get input buffer count (3)
                BMI     BUFFUL          ;Check against limit ($80), branch if full (2/3)
                LDA     UART_RECEIVE    ;Else, get data from 2691 (4)
;
                LDY     ITAIL           ;Get the tail pointer to buffer (3)
                STA     IBUF,Y          ;Store into buffer (5)
                INC     ITAIL           ;Increment tail pointer (5)
                RMB7    ITAIL           ;Strip off bit 7, 128 bytes only (5)
                INC     ICNT            ;increment data count (5)
;
                LDA     UART_STATUS     ;Get 2691 Status Reg (4)
                BIT     #%00000010      ;Check for xmit active (2)
                BEQ     REGEXT0         ;Exit if inactive (2/3)
;
UART_XMT        LDA     OCNT            ;Get output buffer count, any data to xmit? (3)
                BEQ     NODATA          ;If zero, no data left, turn off xmit (2/3)
;
                LDY     OHEAD           ;Get the head pointer to buffer (3)
                LDA     OBUF,Y          ;Get the next data (5)
                STA     UART_TRANSMIT   ;Send the data to 2691 (4)
                INC     OHEAD           ;Increment head pointer (5)
                RMB7    OHEAD           ;Strip off bit 7, 128 bytes only (5)
                DEC     OCNT            ;Decrement counter (5)
                BNE     REGEXT0         ;If not zero, exit and continue normal processing (2/3)
;
;No more buffer data to send, check 2691 status and disable transmit if it's finished.
;
NODATA          LDA     UART_STATUS     ;Get Status Register (4)
                BIT     #%00001000      ;Check for THR empty (2)
                BNE     REGEXT0         ;Exit if data still loaded (2/3)
                BIT     #%00000100      ;Check for TxRDY active (2)
                BEQ     REGEXT0         ;Exit if not active, another data byte in THR (2/3)
                LDY     #%00001000      ;Else, get mask for xmit off (2)
                STY     UART_COMMAND    ;Turn off xmit (4)
                BRA     REGEXT0         ;Exit IRQ handler (3)
;
; SCC2691 uses all bits in the Status Register!
; - for Receive Buffer full, we mimic the receiver FIFO being full. The SCC2691 init routine
;   enables an interrupt whenever a data is received, so the FIFO will not fill up unless
;   the buffer is full for 3 received data bytes.
;
BUFFUL          LDA     #%00000010      ;Get buffer overflow flag (2)
                BRA     BUFF_ERR        ;Branch to exit (3)
;
;IRQ Vector defaults to here, which is the Start of Interrupt handler.
; NOTE: 25 clock cycles to get to this routine
; NOTE: If this ISR is after the IDE ISR, it will take 33 additional clock cycles
;
INTERUPT0                               ;Interrupt 0 to handle the SCC2691 UART
                LDA     UART_ISR        ;Get the UART Interrupt Status Register (4)
                CMP     #%00100000      ;Check for no active IRQ source (2)
                BEQ     REGEXT0         ;If no bits are set, exit handler (2/3)
;
                BIT     #%00001000      ;Test for Delta Break (2)
                BNE     UART_BRK        ;If yes, Reset the UART receiver (2/3)
                BIT     #%00000100      ;Test for RHR having data (2)
                BNE     UART_RCV        ;If yes, put the data in the buffer (2/3)
                BIT     #%00000001      ;Test for THR ready to receive data (2)
                BNE     UART_XMT        ;If yes, get data from buffer (2/3)
                BIT     #%00010000      ;Test for Counter ready (RTC) (2)
                BNE     UART_RTC        ;If yes, go increment RTC variables (2/3)
;
IRQEXT0         STA     UART_IRT        ;Else, save the 2691 IRT for later use (4)
                LDA     UART_STATUS     ;Get 2691 Status Register (4)
BUFF_ERR        STA     UART_SRT        ;Save 2691 Status Register for later use (4)
REGEXT0         JMP     (IRQRTVEC0)     ;Return to Exit/ROM IRQ handler (6)
UART_BRK        JMP     UART_BRK0       ;Gotta JUMP to the routine
;
;NOTE: Stop timer cmd resets the interrupt flag, counter continues to generate interrupts.
; NOTE: 25 clock cycles to here from INTERUPT0 - 50 in total
;
UART_RTC        LDA     #%10010000      ;Get Command mask for stop timer (2)
                STA     UART_COMMAND    ;Send command to 2691 (4)
;
; Check the MATCH flag bit7 to see if a Delay is active. If yes, decrement the MSDELAY
; variable once each pass until it is zero, then clear the MATCH flag bit7
;
                BBR7    MATCH,SKIP_DLY  ;Skip Delay if bit7 is clear (5)
                DEC     MSDELAY         ;Decrement Millisecond delay variable (5)
                BNE     SKIP_DLY        ;If not zero, skip (2/3)
                RMB7    MATCH           ;Else clear MATCH flag (5)
;
; Check the MATCH flag bit6 to see if Benchmarking is active. If yes, increment the
; variables once each pass until the MATCH flag bit6 is inactive.
;
SKIP_DLY        BBR6    MATCH,SKIP_CNT  ;Skip Count if bit6 bit is clear (5)
                INC     MS10_CNT        ;Else, increment 10ms count (5)
                LDA     MS10_CNT        ;Load current value (3)
                CMP     #100            ;Compare for 1 second elapsed time (2)
                BCC     SKIP_CNT        ;If not, skip to RTC update (2/3)
                STZ     MS10_CNT        ;Else, zero 10ms count (3)
                INC     SECL_CNT        ;Increment low byte elapsed seconds (5)
                BNE     SKIP_CNT        ;If no overflow, skip to RTC update (2/3)
                INC     SECH_CNT        ;Else, increment high byte elapsed seconds (5)
;
SKIP_CNT        DEC     TICKS           ;Decrement RTC tick count (5)
                BNE     REGEXT0         ;Exit if not zero (2/3)
                LDA     #DF_TICKS       ;Get default tick count (2)
                STA     TICKS           ;Reset tick count (3)
;
                INC     SECS            ;Increment seconds (5)
                LDA     SECS            ;Load it to A reg (3)
                CMP     #60             ;Check for 60 seconds (2)
                BCC     REGEXT0         ;If not, exit (2/3)
                STZ     SECS            ;Else, reset seconds, inc Minutes (3)
;
                INC     MINS            ;Increment Minutes (5)
                LDA     MINS            ;Load it to A reg (3)
                CMP     #60             ;Check for 60 minutes (2)
                BCC     REGEXT0         ;If not, exit (2/3)
                STZ     MINS            ;Else, reset Minutes, inc Hours (3)
;
                INC     HOURS           ;Increment Hours (5)
                LDA     HOURS           ;Load it to A reg (3)
                CMP     #24             ;Check for 24 hours (2)
                BCC     REGEXT0         ;If not, exit (2/3)
                STZ     HOURS           ;Else, reset hours, inc Days (3)
;
;This is the tricky part ;-)
; One variable holds the Day of the week and the Date of the Month!
; First, update the Day of the Week, which is the upper 3 bits of the variable
; Valid days are 1 to 7. Mask off the upper 3 bits, and add #%00100000 to it,
; if the result is zero, it was #%11100000, so start over by making the Day
; #%00100000, then save it to RTC_TEMP variable.
;
;Once that's done, load the variable again, mask off the Date and increase by
; one, then check against days of the month, update as required and check for
; Leap year and February 29th stuff. When all updated, OR in the Day from the
; RTC_TEMP variable and finish updating the DAY_DATE variable.
;
                LDA     DAY_DATE        ;Get Day and Date variable (3)
                AND     #%11100000      ;Mask off for Day of Week (1-7) (2)
                CLC                     ;Clear carry for add (2)
                ADC     #%00100000      ;Add effective "1" to Day of week (2)
                CLC                     ;Clear carry to avoid extra day add (2)
                BNE     NO_DAY_ADD      ;If non-zero, don't reset to "1" (2/3)
                LDA     #%00100000      ;Else, reset Day to "1" (2)
NO_DAY_ADD
                STA     RTC_TEMP        ;Store the updated Day into temp (3)
;
;Get the Month and Year variable, move the upper 4-bits down to get the
; current Month. Xfer it the X reg, then get the Date, increment by one
; and check against the number of days in that month.
;
                LDA     MONTH_CENTURY   ;Get Month and Year variable (3)
                LSR     A               ;Shift Month to lower 4 bits (2)
                LSR     A               ; (2)
                LSR     A               ; (2)
                LSR     A               ; (2)
                TAX                     ;Move to X reg (2)
                LDA     DAY_DATE        ;Get Day and Date variable (3)
                AND     #%00011111      ;Mask off for Date of Month (1-31) (2)
                INC     A               ;Increment by one (2)
                CMP     MAX_DATE-1,X    ;Check for Max Day per Month+1 (4)
                BCS     MONTH_ADD       ;Branch if we need to update the Month (2/3)
DO_29           ORA     RTC_TEMP        ;Else OR in updated Date to updated Day (3)
                STA     DAY_DATE        ;Update Day and Date variable (3)
                BRA     REGEXT00        ;Then exit IRQ Handler (3)
;
MONTH_ADD       CPX     #$02            ;Check for Month = February (2)
                BNE     MONTH_INC       ;If not, increment Month (2/3)
                LDA     YEAR            ;Else, Get current year low byte (3)
                AND     #%00000011      ;Mask off lower 2 bits (2)
                BNE     MONTH_INC       ;If not a Leap Year, continue on (2/3)
                LDA     DAY_DATE        ;Get Day and Date variable (3)
                AND     #%00011111      :Mask off Date (2)
                INC     A               ;Increment by one (2)
                CMP     #30             ;Check for 29th+1 Day for Leap Year (2)
                BCC     DO_29           ;Save Date as 29th and exit IRQ handler (2/3)
;
MONTH_INC       LDA     RTC_TEMP        ;Get updated Day (Date is effective zero)
                ORA     #%00000001      ;OR in the 1st Day of the Month
                STA     DAY_DATE        ;Save updated Day and Date of the Month
;
                LDA     MONTH_CENTURY   ;Get Month and Year variable
                CLC                     ;Clear Carry for add
                ADC     #$10            ;Add "1" to Month (upper 4 bits)
                STA     RTC_TEMP        ;Save it to work temp
                AND     #%11110000      ;Mask off Century (lower 4 bits)
                CMP     #$D0            ;Check for "13" (December + 1)
                BCS     YEAR_ADD        ;If 13 go add to YEAR
                LDA     RTC_TEMP        ;Else, Get updated Month and Century
                STA     MONTH_CENTURY   ;Save it
                BRA     REGEXT00        ;Exit IRQ Handler
;
YEAR_ADD        LDA     MONTH_CENTURY   ;Get Month and Century
                AND     #%00001111      ;Mask off old month
                ORA     #%00010000      ;OR in $10 for January
                STA     MONTH_CENTURY   ;Save updated Month and existing upper 4 bits
                INC     YEAR            ;Increment Year low byte (0-255)
                BNE     REGEXT00        ;If no rollver, exit ISR
                LDA     MONTH_CENTURY   ;Get Month and Year variable
                TAX                     ;Save to X reg
                AND     #%11110000      ;Mask off upper 4-bits for year
                STA     RTC_TEMP        ;Save it in the temp area
                TXA                     ;Get the Month and Year back
                AND     #%00001111      ;Mask off the month
                CLC                     ;Clear carry for add
                ADC     #$01            ;Add 1 to upper 4 bits
                ORA     RTC_TEMP        ;OR in the Month
                STA     MONTH_CENTURY   ;Update Month and Century variable
REGEXT00        JMP     (IRQRTVEC0)     ;If no rollover, then exit IRQ handler (6)
;
UART_BRK0       LDA     UART_STATUS     ;Get UART Status Register (4)
                BMI     BREAKEY         ;If bit 7 set, received Break was detected (2/3)
;
; If a received Break was not the cause, we should reset the UART as the cause
; could have been a receiver error, i.e., parity or framing
;
                LDX     #UART_RDATAE-UART_RDATA ;Get index count (2)
UART_RST1       LDA     UART_RDATA-1,X  ;Get Reset commands (4)
                STA     UART_COMMAND    ;Send to UART CR (3)
                DEX                     ;Decrement the command list (2)
                BNE     UART_RST1       ;Loop back until all are sent (2/3)
                BRA     REGEXT00         ;Exit (3)
;
BREAKEY         LDA     #%01000000      ;Get Reset Received Break command (2)
                STA     UART_COMMAND    ;Send to UART to reset (4)
                LDA     #%01010000      ;Get Reset Break Interrupt command (2)
                STA     UART_COMMAND    ;Send to UART to reset (4)
                CLI                     ;Enable IRQ (2)
;
; BRK Vector defaults to here
;
BRKINSTR0       PLY                     ;Restore Y Reg (4)
                PLX                     ;Restore X Reg (4)
                PLA                     ;Restore A Reg (4)
                STA     AREG            ;Save A Reg (3)
                STX     XREG            ;Save X Reg (3)
                STY     YREG            ;Save Y Reg (3)
                PLA                     ;Get Processor Status (4)
                STA     PREG            ;Save in PROCESSOR STATUS preset/result (3)
                TSX                     ;Xfer STACK pointer to X Reg (2)
                STX     SREG            ;Save STACK pointer (3)
;
                PLX                     ;Pull Low RETURN address from STACK then save it (4)
                STX     PCL             ;Store program counter Low byte (3)
                STX     INDEXL          ;Seed Indexl for DIS_LINE (3)
                PLY                     ;Pull High RETURN address from STACK then save it (4)
                STY     PCH             ;Store program counter High byte (3)
                STY     INDEXH          ;Seed Indexh for DIS_LINE (3)
                BBR4    PREG,DO_NULL    ;Check for BRK bit set (5)
;
; The following three subroutines are contained in the base C02 Monitor code. These calls
; do a register display and disassembles the line of code that caused the BRK to occur
;
                JSR     M_PRSTAT1       ;Display CPU status
                JSR     M_DECINDEX      ;Decrement Index to BRK ID Byte
                JSR     M_DECINDEX      ;Decrement Index to BRK instruction
                JSR     M_DIS_LINE      ;Disassemble BRK instruction
;
DO_NULL         LDA     #$00            ;Clear all Processor Status Register bits (2)
                PHA                     ;Push it to Stack (3)
                PLP                     ;Pull it to Processor Status (4)
                STZ     ITAIL           ;Clear input buffer pointers (3)
                STZ     IHEAD           ; (3)
                STZ     ICNT            ; (3)
                JMP     (BRKRTVEC0)     ;Done BRK service process, re-enter monitor (6)
;
; EPOCH time (Unix) starts on Thursday, 1st January, 1970.
; If no RTC is detected, preload the EPOCH Date as a start.
; Note: No time of day is preloaded, time is 00:00:00 after a cold start.
;
EPOCH           .DB     %11000001       ;Day 6 / Date 1
                .DB     %00010111       ;Month 1  High byte (nibble) of 1970
                .DB     %10110010       ;Low byte of 1970 ($B2)
;
NO_LBA          .DB     "No LBA Support"
                .DB     $0D,$0A,$00
;
;END OF BIOS CODE for Pages $F8 through $FD
;**************************************************************************************************
        .ORG    $FE00   ;Reserved for I/O space - do NOT put code here
;There are 5- I/O Selects, each is 32-bytes wide.
; I/O-0 = $FE00-$FE1F  Available on BUS expansion connector
; I/O-1 = $FE20-$FE3F  Available on BUS expansion connector
; I/O-2 = $FE40-$FE5F  Available on BUS expansion connector
; I/O-3 = $FE60-$FE7F  Available on BUS expansion connector - Used for RTC-IDE Adapter!
; I/O-4 = $FE80-$FE9F  SCC2691 UART resides here (only 8 bytes used)
;**************************************************************************************************
        .ORG    $FEA0   ;Reserved space for Soft Vector and I/O initialization data
;START OF BIOS DEFAULT VECTOR DATA AND HARDWARE CONFIGURATION DATA
;
;There are 96 bytes of ROM space remaining on page $FE from $FEA0 - $FEFF
; 64 bytes of this are copied to page $03 and used for soft vectors/hardware soft configuration.
; 32 bytes are for vectors and 32 bytes are for hardware config. The last 32 bytes are only held
; in ROM and are used for hardware configuration that should not be changed.
;
;The default location for the NMI/BRK/IRQ Vector data is at $0300. They are defined at the top of
; the source file. There are 8 defined vectors and 8 vector inserts, all are free for base config.
;
;The default location for the hardware configuration data is at $0320. It is a freeform table which
; is copied from ROM to page $03. The allocated size for the hardware config table is 32 bytes.
;
VEC_TABLE      ;Vector table data for default ROM handlers
;
                .DW     NMI_VECTOR      ;NMI Location in ROM
                .DW     BRKINSTR0       ;BRK Location in ROM
                .DW     INTERUPT0       ;IRQ Location in ROM
;
                .DW     M_WARM_MON      ;NMI return handler in ROM
                .DW     M_WARM_MON      ;BRK return handler in ROM
                .DW     IRQ_EXIT0       ;IRQ return handler in ROM
;
                .DW     M_COLD_MON      ;Monitor Cold start
                .DW     M_WARM_MON      ;Monitor Warm start
;
;Vector Inserts (total of 8)
; these can be used as required, all are free for now, as NMI/BRK/IRQ and the Monitor are
; vectored, all can be extended by using these reserved vector locations.
;
                .DW     $FFFF           ;Insert 0 Location (used if IDE is found)
                .DW     $FFFF           ;Insert 1 Location
                .DW     $FFFF           ;Insert 2 Location
                .DW     $FFFF           ;Insert 3 Location
                .DW     $FFFF           ;Insert 4 Location
                .DW     $FFFF           ;Insert 5 Location
                .DW     $FFFF           ;Insert 6 Location
                .DW     $FFFF           ;Insert 7 Location
;
;Configuration Data - The following tables contains the default data used for:
; - Reset of the SCC2691 (RESET_2691 routine)
; - Init of the SCC2691 (INIT_2691 routine)
; - Basic details for register definitions are below, consult SCC2691 DataSheet
;   and Application Note AN405 for details and specific operating conditions.
;
;Mode Register 1 definition ($93)
; Bit7          ;RxRTS Control - 1 = Yes
; Bit6          ;RX-Int Select - 0 = RxRDY
; Bit5          ;Error Mode - 0 = Character
; Bit4/3        ;Parity Mode - 10 = No Parity
; Bit2          ;Parity Type - 0 = Even (doesn't matter)
; Bit1/0        ;Bits Per Character - 11 = 8
;
;Mode Register 2 Definition ($17)
; Bit7/6        ;Channel Mode   - 00 = Normal
; Bit5          ;TxRTS Control - 0 = Yes
; Bit4          ;CTS Enable - 1 = Yes
; Bit3-0        ;Stop Bits - 0111 = 1 Stop Bit
;
;Baud Rate Clock Definition ($CC)
; Upper 4 bits = Receive Baud Rate
; Lower 4 bits = Transmit Baud Rate
; for 38.4K setting is %11001100
; Also set ACR Bit7 = 0 for standard rates
;
;Command Register Definition
; Bit7-4        ;Special commands
; Bit3          ;Disable Transmit
; Bit2          ;Enable Transmit
; Bit1          ;Disable Receive
; Bit0          ;Enable Receive
;
;Aux Control Register Definition ($68)
; Bit7          ;BRG Set Select - 0 = Default
; Bit6-5-4      ;Counter/Timer operating mode 110 = Counter mode from XTAL
; Bit3          ;Power Down mode 1 = Off (normal)
; Bit2-1-0      ;MPO Pin Function 000 = RTSN (active low state)
;
;Interrupt Mask Register Definition ($1D)
; Bit7          ;MPI Pin Change Interrupt 1 = On
; Bit6          ;MPI Level Interrupt 1 = On
; Bit5          ;Not used (shows as active on read)
; Bit4          ;Counter Ready Interrupt 1 = On
; Bit3          ;Delta Break Interrupt 1 = On
; Bit2          ;RxRDY Interrupt 1 = On
; Bit1          ;TxEMT Interrupt 1 = On
; Bit0          ;TxRDY Interrupt 1 = On
;
CFG_TABLE       ;Configuration table for hardware devices
;
;Data commands are sent in reverse order from list. This list is the default initialization for
; the UART as configured for use as a Console connected to either ExtraPutty(WIN) or Serial(OSX)
; The data here is copied to page $03 and is used to configure the UART during boot up. The soft
; data can be changed and the core INIT_2691 can be called to reconfigure the UART.
; NOTE: the register offset data is not kept in soft config memory as the initialization
; sequence should not be changed!
;
INIT_DATA       ;Start of UART Initialization Data
                .DB     %00010000       ;Reset Mode Register pointer
                .DB     %10100000       ;Enable RTS (Receiver)
                .DB     %00001001       ;Enable Receiver/Disable Transmitter
                .DB     %00011101       ;Interrupt Mask Register setup
                .DB     %01101000       ;Aux Register setup for Counter/Timer
                .DB     %01001000       ;Counter/Timer Upper Preset
                .DB     %00000000       ;Counter/Timer Lower Preset
                .DB     %01100110       ;Baud Rate clock for Rcv/Xmt - 115.2K
                .DB     %10010000       ;Disable Counter/Timer
                .DB     %00001010       ;Disable Receiver/Transmitter
                .DB     %10110000       ;Disable RTS (Receiver)
                .DB     %00000000       ;Interrupt Mask Register setup
                .DB     %00001000       ;Aux Register setup for Power On
INIT_DATAE      ;End of UART Initialization Data
;
;Mode Register Data is defined separately. Using a loop routine to send this data to the
; UART does not work properly. See the description of the problem using Indexed addressing
; to load the UART registers above. This data is also kept in soft config memory in page $03.
;
; NOTE: MR2_DAT Bit 7 is now used to configure Extended Baud rates!
;
MR1_DAT         .DB     %10010011       ;Mode Register 1 Data
;
; Bit 7 set for extended baud rates - 115.2k
;
MR2_DAT         .DB     %10010111       ;Mode Register 2 data
;
;Reserved for additional I/O devices (17 bytes free)
;
                .DB     $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
;
;Reset UART Data is listed here. The sequence and commands do not require changes for any reason.
; These are maintained in ROM only. A total of 32 bytes are available for hard configuration data.
; These are the register offsets and Reset data for the UART
;
UART_RDATA      ;UART Reset Data for Received Break (ExtraPutty/Serial Break)
                .DB     %00000001       ;Enable Receiver
;
UART_RDATA1     ;Smaller list for entry level Reset (RESET_2691)
                .DB     %01000000       ;Reset All Errors
                .DB     %00110000       ;Reset Transmitter
                .DB     %00100000       ;Reset Receiver
                .DB     %01010000       ;Reset Break Change Interrupt
UART_RDATAE     ;End of UART Reset Data
;
INIT_OFFSET     ;Start of UART Initialization Register Offsets
                .DB     $02             ;Command Register
                .DB     $02             ;Command Register
                .DB     $02             ;Command Register
                .DB     $05             ;Interrupt Mask Register
                .DB     $04             ;Aux Command Register
                .DB     $06             ;Counter Preset Upper
                .DB     $07             ;Counter Preset Lower
                .DB     $01             ;Baud Clock Register
                .DB     $02             ;Command Register
                .DB     $02             ;Command Register
                .DB     $02             ;Command Register
                .DB     $05             ;Interrupt Mask Register
                .DB     $04             ;Aux Command Register
INIT_OFFSETE    ;End of UART Initialization Register Offsets
;
;Reserved for additional I/O devices (14 bytes)
;
; BIOS 3.00 Data space for software RTC.
;
MAX_DATE                                ;Maximum days per Month+1
                .DB     32,29,32,31,32,31,32,32,31,32,31,32
;
                .DB     $FF,$FF         ;Spare bytes
;
;END OF BIOS VECTOR DATA AND HARDWARE DEFAULT CONFIGURATION DATA
;**************************************************************************************************
;START OF TOP PAGE - DO NOT MOVE FROM THIS ADDRESS!! JUMP Table starts here.
; - BIOS calls are listed below - total of 32, Reserved calls are for future hardware support
; - "B_" JUMP Tables entries are for BIOS routines, provides isolation between Monitor and BIOS
; - Two new calls used for Benchmark Timer, calls 16/17 starting with CO2BIOS 2.02
; - New calls added for IDE Controller and RTC starting with C02BIOS 3.00
;
; NOTE: All Jump table calls add 3 clock cycles to execution time for each BIOS function.
;
        .ORG    $FF00   ;BIOS JMP Table, Cold Init and Vector handlers
;
B_IDE_RESET     JMP     IDE_RES_DIAG    ;Call 00 $FF00
B_IDE_GET_STAT  JMP     IDE_GET_STATUS  ;Call 01 $FF03
B_IDE_IDENTIFY  JMP     IDE_IDENTIFY    ;Call 02 $FF06
B_IDE_READ_LBA  JMP     IDE_READ_LBA    ;Call 03 $FF09
B_IDE_WRITE_LBA JMP     IDE_WRITE_LBA   ;Call 04 $FF0C
B_IDE_VERFY_LBA JMP     IDE_VERIFY_LBA  ;Call 05 $FF0F
B_IDE_SET_LBA   JMP     IDE_SET_LBA     ;Call 06 $FF12
B_IDE_SET_ADDR  JMP     IDE_SET_ADDRESS ;Call 07 $FF15
;
B_RTC_NVRD      JMP     RTC_NVRD        ;Call 08 $FF18
B_RTC_NVWR      JMP     RTC_NVWR        ;Call 09 $FF1B
B_RTC_INIT      JMP     INIT_RTC        ;Call 10 $FF1E
;
B_Reserve11     JMP     RESERVE         ;Call 11 $FF21
B_Reserve12     JMP     RESERVE         ;Call 12 $FF24
B_Reserve13     JMP     RESERVE         ;Call 13 $FF27
B_Reserve14     JMP     RESERVE         ;Call 14 $FF2A
;
B_CNT_INIT      JMP     CNT_INIT        ;Call 15 $FF2D
B_CNT_STRT      JMP     CNT_STRT        ;Call 16 $FF30
B_CNT_STOP      JMP     CNT_STOP        ;Call 17 $FF33
;
B_CHRIN_NW      JMP     CHRIN_NW        ;Call 18 $FF36
B_CHRIN         JMP     CHRIN           ;Call 19 $FF39
B_CHROUT        JMP     CHROUT          ;Call 20 $FF3C
;
B_SET_DLY       JMP     SET_DLY         ;Call 21 $FF3F
B_EXE_MSDLY     JMP     EXE_MSDLY       ;Call 22 $FF42
B_EXE_LGDLY     JMP     EXE_LGDLY       ;Call 23 $FF45
B_EXE_XLDLY     JMP     EXE_XLDLY       ;Call 24 $FF48
;
B_INIT_VEC      JMP     INIT_VEC        ;Call 25 $FF4B
B_INIT_CFG      JMP     INIT_CFG        ;Call 26 $FF4E
B_INIT_2691     JMP     INIT_2691       ;Call 27 $FF51
B_RESET_2691    JMP     RESET_2691      ;Call 28 $FF54
;
B_WRMMNVEC0     JMP     (WRMMNVEC0)     ;Call 29 $FF57
B_CLDMNVEC0     JMP     (CLDMNVEC0)     ;Call 30 $FF5A
;
B_COLDSTRT      SEI                     ;Call 31 $FF5D - Disable Interrupts (safety)
                CLD                     ;Clear decimal mode (safety)
                LDX     #$00            ;Index for length of page
PAGE0_LP        STZ     $00,X           ;Clear Page Zero
                DEX                     ;Decrement index
                BNE     PAGE0_LP        ;Loop back till done
                DEX                     ;LDX #$FF ;-)
                TXS                     ;Set Stack Pointer
;
                JSR     INIT_PG03       ;Xfer default Vectors/HW Config to $0300
                JSR     INIT_IO         ;Init I/O - UART (Console/Timer)
;
; Send BIOS init msg to console - note: X Reg is zero on return from INIT_IO
;
BMSG_LP         LDA     BIOS_MSG,X      ;Get BIOS init msg
                BEQ     CHECK_IO        ;If zero, msg done, Test for extra I/O
                JSR     CHROUT          ;Send to console
                INX                     ;Increment Index
                BRA     BMSG_LP         ;Loop back until done
CHECK_IO
                JSR     DETECT_RTC      ;Detect and Init RTC
                JSR     DETECT_IDE      ;Detect and Init IDE
                JSR     RESERVE         ;Reserve one more Init routine for future use
                BRA     B_CLDMNVEC0     ;Branch to Coldstart Monitor
;
IRQ_VECTOR                              ;This is the ROM start for the BRK/IRQ handler
                PHA                     ;Save A Reg (3)
                PHX                     ;Save X Reg (3)
                PHY                     ;Save Y Reg (3)
                TSX                     ;Get Stack pointer (2)
                LDA     $0100+4,X       ;Get Status Register (4)
                AND     #$10            ;Mask for BRK bit set (2)
                BNE     DO_BRK          ;If set, handle BRK (2/3)
                JMP     (IRQVEC0)       ;Jump to Soft vectored IRQ Handler (6)
DO_BRK          JMP     (BRKVEC0)       ;Jump to Soft vectored BRK Handler (6)
NMI_ROM         JMP     (NMIVEC0)       ;Jump to Soft vectored NMI handler (6)
;
;This is the standard return for the IRQ/BRK handler routines
;
IRQ_EXIT0       PLY                     ;Restore Y Reg (4)
                PLX                     ;Restore X Reg (4)
                PLA                     ;Restore A Reg (4)
                RTI                     ;Return from IRQ/BRK routine (6)
;
INIT_PG03       JSR     INIT_VEC        ;Init the Soft Vectors first
INIT_CFG        LDY     #$40            ;Get offset to Config data
                BRA     DATA_XFER       ;Go move the Config data to page $03
INIT_VEC        LDY     #$20            ;Get offset to Vector data
;
DATA_XFER       SEI                     ;Disable Interrupts, can be called via JMP table
                LDX     #$20            ;Set count for 32 bytes
DATA_XFLP       LDA     VEC_TABLE-1,Y   ;Get ROM table data
                STA     SOFTVEC-1,Y     ;Store in Soft table location
                DEY                     ;Decrement index
                DEX                     ;Decrement count
                BNE     DATA_XFLP       ;Loop back till done
                CLI                     ;Re-enable interrupts
RESERVE         RTS                     ;Return to caller
;
RTC_MSG
;
;This is a short BIOS message that is displayed when the DS1511 RTC is found
                .DB     "RTC found"
                .DB     $0D,$0A,$00
;
IDE_MSG
;
;This is a short BIOS message that is displayed when the IDE controller is found
                .DB     "IDE found"
                .DB     $0D,$0A,$00
;
;The offset data here is used as an index to the Identity Block of Data from the IDE controller
LBA_OFFSET      .DB     120,121,122,123 ;Offset Data for LBA Size
;
; This BIOS version does not rely on CPU clock frequency for RTC timing. Timing is based on the
; SCC2691 UART Timer/Counter which has a fixed frequency of 3.6864MHz. Jiffy clock set at 10ms.
; NOTE: The SCC2691 UART can run with a CPU clock frequency up to 6MHz! (datasheet limitation).
; Edit Displayed clock rate at CPU_CLK below as needed if running "other" than 6MHz.
;
        .ORG    $FFD0   ;Hard coded BIOS message to the top of memory (Monitor uses this)
;BIOS init message - sent before jumping to the monitor coldstart vector.
; CO2BIOS 2.01 provides a longer BIOS message with more detail, fixed length/location!
;
BIOS_MSG        .DB     $0D,$0A         ;CR/LF
                .DB     "C02BIOS 3.04"  ;Updated Release Version
                .DB     $0D,$0A         ;CR/LF
                .DB     "14/05/2021"    ;DD/MM/YYYY
                .DB     $0D,$0A         ;CR/LF
                .DB     "W65C02@"       ;Display CPU type
CPU_CLK         .DB     "6MHz"          ;Displayed CPU Clock frequency
                .DB     $0D,$0A,$00     ;CR/LF and terminate string
;
        .ORG    $FFFA   ;65C02 Vectors:
                .DW     NMI_ROM         ;NMI
                .DW     B_COLDSTRT      ;RESET
                .DW     IRQ_VECTOR      ;IRQ
        .END