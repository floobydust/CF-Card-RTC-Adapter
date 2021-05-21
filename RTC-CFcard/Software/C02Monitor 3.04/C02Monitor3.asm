;**************************************************************************************************
;*   C02Monitor 3.04 - Release version for Pocket SBC  (c)2013-2021 by Kevin E. Maier 14/05/2021  *
;*                                                                                                *
;*  Monitor Functions are divided into groups as follows:                                         *
;*   1. Memory Operations:                                                                        *
;*      - Fill Memory: Source, Length, Value (prompts for commit)                                 *
;*      - Move Memory: Source, Target, Length (prompts for commit)                                *
;*      - Compare Memory: Source, Target, Length                                                  *
;*      - Examine/Edit: Address, Data (edit) sequential memory                                    *
;*      - Input ASCII Text into memory: Address, Data (ESC quits)                                 *
;*      - Hex Data Search: Hex data up to 16 bytes                                                *
;*      - Text String Search: ASCII data up to 16 characters                                      *
;*      - Display Memory as Hex/ASCII: Address start - 256 consecutive bytes displayed            *
;*      - Execute from Memory: Start address                                                      *
;*                                                                                                *
;*   2. Register Operations:                                                                      *
;*      - Display All Registers                                                                   *
;*      - Display/Edit A, X, Y, (S)tack Pointer, (P)rocessor Status                               *
;*                                                                                                *
;*   3. Timer/Counter Functions:                                                                  *
;*      - Set delay time: 8-bit 10-millisecond count and 16-bit multiplier count                  *
;*      - Execute Millisecond Delay 1-256 times 10ms (Jiffy Clock)                                *
;*      - Execute Millisecond Delay times 16-bit multiplier count (0-65535)                       *
;*      - Extended Delay: up to 256 times above count ($00 = 256)                                 *
;*      - RTC function based on 10ms Jiffy Clock: Ticks, Seconds, Minutes, Hours, Days            *
;*      - Benchmark timing up to 65535.99 seconds with 10ms resolution                            *
;*                                                                                                *
;*   4. Keyboard Macro Facility:                                                                  *
;*      - Provides up to 127 byte keyboard buffer loop capability                                 *
;*      - Optional 16-bit Loop Counter (1-65,535)                                                 *
;*      - Send Break command (from Terminal program) exits Macro function                         *
;*                                                                                                *
;*   5. Control-Key Functions:                                                                    *
;*      - CTRL-A: Starts Assembler for simple code entry based on Mnemonics (not completed yet)   *
;*      - CTRL-B: Starts Rich Leary's DOS/65 ROM Version 3 (NEW)                                  *
;*      - CTRL-D: Table-Driven Disassembler - Supports Full WDC Opcodes/Addressing modes          *
;*      - CTRL-E: Edit single byte in the EEPROM - uses BurnByte routine to ensure proper write   *
;*      - CTRL-L: Xmodem Loader w/CRC-16 Support, auto detect S19 Records from WDC Linker         *
;*      - CTRL-P: Program EEPROM - Source, Target, Length (Source must be RAM based)              *
;*      - CTRL-Q: Query Commands - Shows all available Monitor functions                          *
;*      - CTRL-R: Reset System - Initiates Cold Start of BIOS and Monitor                         *
;*      - CTRL-S: Xmodem Save w/CRC-16 Support                                                    *
;*      - CTRL-T: Shows Elapsed time since System Cold Start                                      *
;*      - CTRL-V: Shows Version for BIOS and Monitor                                              *
;*      - CTRL-Z: Zeros out ALL RAM and initiates Cold Start of BIOS and Monitor                  *
;*                                                                                                *
;*   6. Panic Button (NMI Support Routine in C02BIOS)                                             *
;*      - Saves Page Zero, CPU Stack, Console Buffer and Vector/Config Data pages                 *
;*      - Re-initializes Vector and Configuration Data in Page $03                                *
;*      - Clears Console Buffer pointers in Page $00 and restarts Console only                    *
;*      - Toggles Test Flag in SCC2691 UART - might need to invoke twice (removed in 2.02)        *
;**************************************************************************************************
        PL      66      ;Page Length
        PW      132     ;Page Width (# of char/line)
        CHIP    W65C02S ;Enable WDC 65C02 instructions
        PASS1   OFF     ;Set ON for debugging
        INCLIST ON      ;Set ON for listing Include files
;**************************************************************************************************
; Page Zero definitions $00 to $9F reserved for user routines
; NOTES:- Locations $00 and $01 are used to zero RAM (calls CPU reset)
;       - EEPROM Byte Write routine loaded into Page Zero at $88-$9C
;       - Enhanced Basic uses locations $00 - $85
;**************************************************************************************************
;
; This BIOS and Monitor version also use a common source file for constants and variables used by
; both. This just simplifies keeping both code pieces in sync.
;
        INCLUDE         C02Constants3.asm
;
;**************************************************************************************************
;
;**************************************************************************************************
;The following 32 functions are provided by BIOS via the JMP Table below
; $FF21 - $FF2A are Reserved for future expansion
;
; $FF00 IDE_RES_DIAG    ;Reset IDE and Run Diagnostics
; $FF03 IDE_GET_STATUS  ;Get IDE Status and Extended Error codes
; $FF06 IDE_IDENTIFY    ;Get IDE Identification Block
; $FF09 IDE_READ_LBA    ;Read a Block from IDE device
; $FF0C IDE_WRITE_LBA   ;Write a Block to IDE device
; $FF0F IDE_VERIFY_LBA  ;Verify the last Block from IDE device
; $FF12 IDE_SET_LBA     ;Set the LBA Block ID for Read/Write/Verify
; $FF15 IDE_SET_ADDRESS ;Set the Memory Address to transfer Block data to/from 
;
; $FF18 RTC_NVRD        ;Read the NVRAM from the RTC
; $FF1B RTC_NVWR        ;Write the NVRAM to the RTC
; $FF1E RTC_INIT        ;Initialize the Software RTC from the hardware RTC
;
; $FF2D CNT_INIT        ;Reset counters and Start Benchmark Counter
; $FF30 CNT_STRT        ;Start Benchmark Counter
; $FF33 CNT_STOP        ;Stop Benchmark Counter
; $FF36 CHRIN_NW        ;Data input from console, no waiting, clear carry if none
; $FF39 CHRIN           ;Data input from console, carry set if data
; $FF3C CHROUT          ;Data output to console, sent data preserved
; $FF3F SET_DLY         ;Set delay value for milliseconds and 16-bit counter
; $FF42 EXE_MSDLY       ;Execute millisecond delay 1-256 * 10 milliseconds
; $FF45 EXE_LGDLY       ;Execute long delay; millisecond delay * 16-bit count
; $FF48 EXE_XLDLY       ;Execute extra long delay; 8-bit count * long delay
; $FF4B INIT_VEC        ;Initialize soft vectors at $0300 from ROM
; $FF4E INIT_CFG        ;Initialize soft config values at $0320 from ROM
; $FF51 INIT_2691       ;Initialize SCC2691 console 38.4K, 8-N-1 RTS/CTS
; $FF54 RESET_2691      ;Reset SCC2691 - called before INIT_2691
; $FF57 MON_WARM        ;Monitor warm start - jumps to page $03
; $FF5A MON_COLD        ;Monitor cold start - jumps to page $03
; $FF5D COLDSTRT        ;System cold start - RESET vector for 65C02
;
;**************************************************************************************************
; Additional BIOS equates for using extended baud rates
MR2_DAT         .EQU    $FECE                   ;ROM config data - UART
;**************************************************************************************************
;BIOS JUMP Table starts here:
;       - BIOS calls are listed below - total of 32
;       - Reserved calls are for future hardware support
;
B_IDE_RESET             .EQU    $FF00           ;Call 00
B_IDE_GET_STAT          .EQU    $FF03           ;Call 01
B_IDE_IDENTIFY          .EQU    $FF06           ;Call 02
B_IDE_READ_LBA          .EQU    $FF09           ;Call 03
B_IDE_WRITE_LBA         .EQU    $FF0C           ;Call 04
B_IDE_VERFY_LBA         .EQU    $FF0F           ;Call 05
B_IDE_SET_LBA           .EQU    $FF12           ;Call 06
B_IDE_SET_ADDR          .EQU    $FF15           ;Call 07
;
B_RTC_NVRD              .EQU    $FF18           ;Call 08
B_RTC_NVWR              .EQU    $FF1B           ;Call 09
B_RTC_INIT              .EQU    $FF1E           ;Call 10
;
B_Reserve11             .EQU    $FF21           ;Call 11
B_Reserve12             .EQU    $FF24           ;Call 12
B_Reserve13             .EQU    $FF27           ;Call 13
B_Reserve14             .EQU    $FF2A           ;Call 14
;
B_CNT_INIT              .EQU    $FF2D           ;Call 15
B_CNT_STRT              .EQU    $FF30           ;Call 16
B_CNT_STOP              .EQU    $FF33           ;Call 17
;
B_CHRIN_NW              .EQU    $FF36           ;Call 18
B_CHRIN                 .EQU    $FF39           ;Call 19
B_CHROUT                .EQU    $FF3C           ;Call 20
;
B_SET_DLY               .EQU    $FF3F           ;Call 21
B_EXE_MSDLY             .EQU    $FF42           ;Call 22
B_EXE_LGDLY             .EQU    $FF45           ;Call 23
B_EXE_XLDLY             .EQU    $FF48           ;Call 24
;
B_INIT_VEC              .EQU    $FF4B           ;Call 25
B_INIT_CFG              .EQU    $FF4E           ;Call 26
B_INIT_2691             .EQU    $FF51           ;Call 27
B_RESET_2691            .EQU    $FF54           ;Call 28
;
B_WRMMNVEC0             .EQU    $FF57           ;Call 29
B_CLDMNVEC0             .EQU    $FF5A           ;Call 30
B_COLDSTRT              .EQU    $FF5D           ;Call 31
;
BIOS_MSG                .EQU    $FFD0           ;BIOS Startup Message is hard-coded here
;**************************************************************************************************
        .ORG $E000                              ;6KB reserved for Monitor ($E000 through $F7FF)
;**************************************************************************************************
;Monitor JUMP Table starts here:
;       - Monitor calls are listed below - total of 32
;       - Reserved calls are for future Monitor functions
;
M_COLD_MON      JMP     COLD_MON                ;Call 00 $E000
M_WARM_MON      JMP     WARM_MON                ;Call 01 $E003
;
M_RESERVE2      JMP     RESERVED                ;Call 02 $E006
M_MOVE_RAM      JMP     MOVER+3                 ;Call 03 $E009
M_FILL_LP       JMP     FILL_LP                 ;Call 04 $E00C
;
M_BSOUT         JMP     BSOUT                   ;Call 05 $E00F
M_XMDM_SAVE     JMP     XMDM_SAVE               ;Call 06 $E012
M_XMDM_LOAD     JMP     XMDM_LOAD               ;Call 07 $E015
M_BENCH         JMP     BENCH                   ;Call 08 $E018
M_QUITB         JMP     QUITB                   ;Call 09 $E01B
M_TIME          JMP     TIME                    ;Call 10 $E01E
M_PRSTAT1       JMP     PRSTAT1                 ;Call 11 $E021
M_DIS_LINE      JMP     DIS_LINE                ;Call 12 $E024
M_INCINDEX      JMP     INCINDEX                ;Call 13 $E027
M_DECINDEX      JMP     DECINDEX                ;Call 14 $E02A
M_RDLINE        JMP     RDLINE                  ;Call 15 $E02D
M_RDCHAR        JMP     RDCHAR                  ;Call 16 $E030
M_HEXIN2        JMP     HEXIN2                  ;Call 17 $E033
M_HEXIN4        JMP     HEXIN4                  ;Call 18 $E036
M_HEX2ASC       JMP     HEX2ASC                 ;Call 19 $E039
M_BIN2ASC       JMP     BIN2ASC                 ;Call 20 $E03C
M_ASC2BIN       JMP     ASC2BIN                 ;Call 21 $E03F
M_BEEP          JMP     BEEP                    ;Call 22 $E042
M_DOLLAR        JMP     DOLLAR                  ;Call 23 $E045
M_CROUT         JMP     CROUT                   ;Call 24 $E048
M_SPC           JMP     SPC                     ;Call 25 $E04B
M_PRBYTE        JMP     PRBYTE                  ;Call 26 $E04E
M_PRWORD        JMP     PRWORD                  ;Call 27 $E051
M_PRASC         JMP     PRASC                   ;Call 28 $E054
M_PROMPT        JMP     PROMPT                  ;Call 29 $E057
M_PROMPTR       JMP     PROMPTR                 ;Call 30 $E05A
M_CONTINUE      JMP     CONTINUE                ;Call 31 $E05D
;
;START OF MONITOR CODE
;**************************************************************************************************
;*                      This is the Monitor Cold start vector                                     *
;**************************************************************************************************
COLD_MON        LDA     #$14                    ;Get intro msg / BEEP
                JSR     PROMPT                  ;Send to Console
;
;**************************************************************************************************
;*                              Command input loop                                                *
;**************************************************************************************************
;*                      This is the Monitor Warm start vector                                     *
;**************************************************************************************************
WARM_MON        LDX     #$FF                    ;Initialize Stack pointer
                TXS                             ;Xfer to stack
                RMB7    CMDFLAG                 ;Clear bit7 of command flag
                LDA     #$16                    ;Get prompt msg
                JSR     PROMPT                  ;Send to terminal
;
CMON            JSR     RDCHAR                  ;Wait for keystroke (converts to upper-case)
                LDX     #MONTAB-MONCMD-1        ;Get command list count
CMD_LP          CMP     MONCMD,X                ;Compare to command list
                BNE     CMD_DEC                 ;Check for next command and loop
                PHA                             ;Save keystroke
                TXA                             ;Xfer Command index to A reg
                ASL     A                       ;Multiply keystroke value by 2 (command offset)
                TAX                             ;Xfer Command offsett address to table MONTAB
                PLA                             ;Restore keystroke (some commands send to terminal)
                JSR     DOCMD                   ;Call Monitor command processor as a subroutine
                BRA     WARM_MON                ;Command processed, branch / wait for next command
DOCMD           JMP     (MONTAB,X)              ;Execute command from Table
;
CMD_DEC         DEX                             ;Decrement index count
                BPL     CMD_LP                  ;If more to check, loop back
                JSR     BEEP                    ;Beep for error, not valid command character
                BRA     CMON                    ;Branch back and re-enter Monitor
;
;**************************************************************************************************
;*                      Basic Subroutines used by multiple routines                               *
;**************************************************************************************************
;
;ASC2BIN subroutine: Convert 2 ASCII HEX digits to a binary (byte) value
; Enter: A register = high digit, Y register = low digit
; Return: A register = binary value
; Updated routine via Mike Barry... saves 3 bytes, 10 clock cycles
ASC2BIN         STZ     TEMP1           ;Clear TEMP1
                JSR     BINARY          ;Convert high digit to 4-bit nibble
                ASL     A               ;Shift to high nibble
                ASL     A
                ASL     A
                ASL     A
                STA     TEMP1           ;Store it in temp area
                TYA                     ;Get Low digit
;
BINARY          EOR     #$30            ;ASCII -> HEX nibble
                CMP     #$0A            ;Check for result < 10
                BCC     BNOK            ;Branch if 0-9
                SBC     #$67            ;Else subtract for A-F
BNOK            ORA     TEMP1           ;OR into temp value
RESERVED        RTS                     ;Return to caller
;
;BIN2ASC subroutine: Convert single byte to two ASCII HEX digits
; Enter: A register contains byte value to convert
; Return: A register = high digit, Y register = low digit
BIN2ASC         PHA                     ;Save A Reg on stack
                AND     #$0F            ;Mask off high nibble
                JSR     ASCII           ;Convert nibble to ASCII HEX digit
                TAY                     ;Move to Y Reg
                PLA                     ;Get character back from stack
                LSR     A               ;Shift high nibble to lower 4 bits
                LSR     A
                LSR     A
                LSR     A
;
ASCII           CMP     #$0A            ;Check for 10 or less
                BCC     ASCOK           ;Branch if less than 10
                ADC     #$06            ;Add $06+CF ($07) for A-F
ASCOK           ADC     #$30            ;Add $30 for ASCII
                RTS                     ;Return to caller
;
;HEX8ASC - Accepts 8-bit Hexadecimal value (00-99 decimal) and converts to ASCII numeric values.
; A register contains the single byte value on entry and outputs the two ASCII numeric values.
; leading zero is output as it is used for showing hundredths of a second after a decimal point.
HEX8ASC         LDY     #$FF            ;Load Y reg with "-1"
                SEC                     ;Set carry for subtraction
HEX8LP1         INY                     ;Increment 10's count (starts at zero)
                SBC     #10             ;Subtract 10 decimal
                BCS     HEX8LP1         ;Branch back if >10
                ADC     #$3A            ;Add the last 10 back plus $30 (ASCII "0")
                PHA                     ;Save 1's count to the Stack
                TYA                     ;Get the 10's count
                CLC                     ;Clear carry for add
                ADC     #$30            ;Add $30 for ASCII digit
                JSR     B_CHROUT        ;Print the first digit (10's)
                PLA                     ;Get 1's count from the Stack
                JMP     B_CHROUT        ;Print the second digit, return
;
;HEX2ASC - Accepts 16-bit Hexadecimal value and converts to an ASCII decimal string. Input is
; via the A and Y registers and output is up to 5 ASCII digits in DATABUFF. The High Byte is in
; the Y Register and Low Byte is in the A register. Output data is placed in variable DATABUFF
; and terminated with a null character.
; Note: leading zeros are supressed. PROMPTR routine is used to print the ASCII decimal value.
; Core routine based on Michael Barry's code. Saves many bytes with two updates/changes ;-)
HEX2ASC         STA     BINVALL         ;Save Low byte
                STY     BINVALH         ;Save High byte
                LDX     #5              ;Get ASCII buffer offset
                STZ     DATABUFF,X      ;Zero last buffer byte for null end
;
CNVERT          LDA     #$00            ;Clear remainder
                LDY     #16             ;Set loop count for 16-bits
;
DVLOOP          CMP     #$05            ;Partial remainder >= 10/2
                BCC     DVLOOP2         ;Branch if less
                SBC     #$05            ;Update partial (carry set)
;
DVLOOP2         ROL     BINVALL         ;Shift carry into dividend
                ROL     BINVALH         ;Which will be quotient
                ROL     A               ;Rotate A reg
                DEY                     ;Decrement count
                BNE     DVLOOP          ;Branch back until done
                ORA     #$30            ;Or in $30 for ASCII
;
                DEX                     ;Decrement buffer offset
                STA     DATABUFF,X      ;Store digit into buffer
;
                LDA     BINVALL         ;Get the Low byte
                ORA     BINVALH         ;OR in the High byte (check for zero)
                BNE     CNVERT          ;Branch back until done
;
;Conversion is complete, get the string address, add offset, then call prompt routine and return
; note DATABUFF is fixed location in Page 0, carry flag need not be cleared as result can never
; set flag after ADC instruction.
                TXA                     ;Get buffer offset
                ADC     #<DATABUFF      ;Add Low byte address
                LDY     #>DATABUFF      ;Get High byte address
                BRA     PROMPTR         ;Branch to PROMPTR to Print numeric string
;
;PROMPT routine: Send indexed text string to terminal. Index is contained in A reg.
; String buffer address is stored in variable PROMPTL/PROMPTH.
PROMPT          ASL     A               ;Multiply by two for msg table index
                TAX                     ;Xfer to X reg - index
                LDA     MSG_TABLE,X     ;Get low byte address
                LDY     MSG_TABLE+1,X   ;Get high byte address
;
;PROMPTR routine: takes message address in Y/A and prints via PROMPT2 routine
PROMPTR         STA     PROMPTL         ;Store low byte
                STY     PROMPTH         ;Store high byte
;
;PROMPT2 routine: prints message at address (PROMPTL) till null character found
PROMPT2         LDA     (PROMPTL)       ;Get string data
                BEQ     HINEXIT         ;If null character, exit (borrowed RTS)
                JSR     B_CHROUT        ;Send character to terminal
                INC     PROMPTL         ;Increment low byte index
                BNE     PROMPT2         ;Loop back for next character
                INC     PROMPTH         ;Increment high byte index
                BRA     PROMPT2         ;Loop back and continue printing
;
;SETUP subroutine: Request HEX address input from terminal
SETUP           JSR     B_CHROUT        ;Send command keystroke to terminal
                JSR     SPC             ;Send [SPACE] to terminal
                BRA     HEXIN4          ;Request a 0-4 digit HEX address input from terminal
;
;HEX input subroutines: Request 1 to 4 ASCII HEX digits from terminal, then convert digits into
; a binary value. For 1 to 4 digits entered, HEXDATAH and HEXDATAL contain the output.
; Variable BUFIDX will contain the number of digits entered
; HEXIN2 - returns value in A reg and Y reg only (Y reg always $00)
; HEXIN4 - returns values in A reg, Y reg and INDEXL/INDEXH
; HEX2 - Prints MSG# in A reg then calls HEXIN2, HEX4 - Prints MSG# in A reg then calls HEXIN4
HEX4            JSR     PROMPT          ;Print MSG # from A reg
HEXIN4          LDX     #$04            ;Set for number of characters allowed
                JSR     HEXINPUT        ;Convert digits
                STY     INDEXH          ;Store to INDEXH
                STA     INDEXL          ;Store to INDEXL
                RTS                     ;Return to caller
;
HEX2            JSR     PROMPT          ;Print MSG # from A reg
HEXIN2          LDX     #$02            ;Set for number of characters allowed
;
;HEXINPUT subroutine: request 1 to 4 HEX digits from terminal, then convert ASCII HEX to HEX
; minor update from Mike Barry, saves a byte.
; Setup RDLINE subroutine parameters:
HEXINPUT        JSR     DOLLAR          ;Send "$" to console
                JSR     RDLINE          ;Request ASCII HEX input from terminal
                BEQ     HINEXIT         ;Exit if none (Z flag already set)
                STZ     HEXDATAH        ;Clear Upper HEX byte, Lower HEX byte will be updated
                LDY     #$02            ;Set index for 2 bytes
ASCLOOP         PHY                     ;Save it to stack
                LDA     INBUFF-1,X      ;Read ASCII digit from buffer
                TAY                     ;Xfer to Y Reg (LSD)
                DEX                     ;Decrement input count
                BEQ     NO_UPNB         ;Branch if no upper nibble
                LDA     INBUFF-1,X      ;Read ASCII digit from buffer
                BRA     DO_UPNB         ;Branch to include upper nibble
NO_UPNB         LDA     #$30            ;Load ASCII "0" (MSD)
DO_UPNB         JSR     ASC2BIN         ;Convert ASCII digits to binary value
                PLY                     ;Get index from stack
                STA     HEXDATAH-1,Y    ;Write byte to indexed buffer location
                TXA                     ;Check for zero, (no digits left)
                BEQ     HINDONE         ;If not, exit
                DEY                     ;Else, decrement to next byte set
                DEX                     ;Decrement index count
                BNE     ASCLOOP         ;Loop back for next byte
HINDONE         LDY     HEXDATAH        ;Get High Byte
                LDA     HEXDATAL        ;Get Low Byte
                LDX     BUFIDX          ;Get input count (set Z flag)
HINEXIT         RTS                     ;And return to caller
;
;RDLINE subroutine: Store keystrokes into buffer until [RETURN] key is struck
; Used for Hex entry, so only (0-9,A-F) are accepted entries. Lower-case alpha characters
; are converted to upper-case. On entry, X reg = buffer length. On exit, X reg = buffer count
; [BACKSPACE] key removes keystrokes from buffer. [ESCAPE] key aborts then re-enters monitor.
RDLINE          STX     BUFLEN          ;Store buffer length
                STZ     BUFIDX          ;Zero buffer index
RDLOOP          JSR     RDCHAR          ;Get character from terminal, convert LC2UC
                CMP     #$1B            ;Check for ESC key
                BEQ     RDNULL          ;If yes, exit back to Monitor
NOTESC          CMP     #$0D            ;Check for C/R
                BEQ     EXITRD          ;Exit if yes
                CMP     #$08            ;Check for Backspace
                BEQ     RDBKSP          ;If yes handle backspace
                CMP     #$30            ;Check for '0' or higher
                BCC     INPERR          ;Branch to error if less than '0'
                CMP     #$47            ;Check for 'G' ('F'+1)
                BCS     INPERR          ;Branch to error if 'G' or higher
                LDX     BUFIDX          ;Get the current buffer index
                CPX     BUFLEN          ;Compare to length for space
                BCC     STRCHR          ;Branch to store in buffer
INPERR          JSR     BEEP            ;Else, error, send Bell to terminal
                BRA     RDLOOP          ;Branch back to RDLOOP
STRCHR          STA     INBUFF,X        ;Store keystroke in buffer
                JSR     B_CHROUT        ;Send keystroke to terminal
                INC     BUFIDX          ;Increment buffer index
                BRA     RDLOOP          ;Branch back to RDLOOP
RDBKSP          LDA     BUFIDX          ;Check if buffer is empty
                BEQ     INPERR          ;Branch if yes
                DEC     BUFIDX          ;Else, decrement buffer index
                JSR     BSOUT           ;Send Backspace to terminal
                BRA     RDLOOP          ;Loop back and continue
EXITRD          LDX     BUFIDX          ;Get keystroke count (Z flag)
                BNE     UCOK            ;If data entered, normal exit
                BBS7    CMDFLAG,UCOK    ;Branch if bit7 of command flag active
RDNULL          JMP     (WRMMNVEC0)     ;Quit to Monitor warm start
;
;RDCHAR subroutine: Waits for a keystroke to be entered.
; if keystroke is a lower-case alphabetical, convert it to upper-case
RDCHAR          JSR     B_CHRIN         ;Request keystroke input from terminal
                CMP     #$61            ;Check for lower case value range
                BCC     UCOK            ;Branch if < $61, control code/upper-case/numeric
                SBC     #$20            ;Subtract $20 to convert to upper case
UCOK            RTS                     ;Character received, return to caller
;
;Routines to update pointers for memory operations. UPD_STL subroutine: Increments Source
; and Target pointers. UPD_TL subroutine: Increments Target pointers only, then drops into
; decrement Length pointer. Used by multiple Memory operation commands.
UPD_STL         INC     SRCL            ;Increment source low byte
                BNE     UPD_TL          ;Check for rollover
                INC     SRCH            ;Increment source high byte
UPD_TL          INC     TGTL            ;Increment target low byte
                BNE     DECLEN          ;Check for rollover
                INC     TGTH            ;Increment target high byte
;
;DECLEN subroutine: decrement 16-bit variable LENL/LENH
DECLEN          LDA     LENL            ;Get length low byte
                BNE     SKP_LENH        ;Test for LENL = zero
                DEC     LENH            ;Else decrement length high byte
SKP_LENH        DEC     LENL            ;Decrement length low byte
                RTS                     ;Return to caller
;
;DECINDEX subroutine: decrement 16 bit variable INDEXL/INDEXH
DECINDEX        LDA     INDEXL          ;Get index low byte
                BNE     SKP_IDXH        ;Test for INDEXL = zero
                DEC     INDEXH          ;Decrement index high byte
SKP_IDXH        DEC     INDEXL          ;Decrement index low byte
                RTS                     ;Return to caller
;
;INCINDEX subroutine: increment 16 bit variable INDEXL/INDEXH
INCINDEX        INC     INDEXL          ;Increment index low byte
                BNE     SKP_IDX         ;If not zero, skip high byte
                INC     INDEXH          ;Increment index high byte
SKP_IDX         RTS                     ;Return to caller
;
;Output routines for formatting, backspace, CR/LF, BEEP, etc.
; all routines preserve the A reg on exit.
;
;BEEP subroutine: Send ASCII [BELL] to terminal
BEEP            PHA                     ;Save A reg on Stack
                LDA     #$07            ;Get ASCII [BELL] to terminal
                BRA     SENDIT          ;Branch to send
;
;BSOUT subroutine: send a Backspace to terminal
BSOUT           JSR     BSOUT2          ;Send an ASCII backspace
                JSR     SPC             ;Send space to clear out character
BSOUT2          PHA                     ;Save character in A reg
                LDA     #$08            ;Send another Backspace to return
BRCHOUT         BRA     SENDIT          ;Branch to send
;
BSOUT3T         JSR     BSOUT2          ;Send a Backspace 3 times
BSOUT2T         JSR     BSOUT2          ;Send a Backspace 2 times
                BRA     BSOUT2          ;Send a Backspace and return
;
;SPC subroutines: Send a Space to terminal 1,2 or 4 times
SPC4            JSR     SPC2            ;Send 4 Spaces to terminal
SPC2            JSR     SPC             ;Send 2 Spaces to terminal
SPC             PHA                     ;Save character in A reg
                LDA     #$20            ;Get ASCII Space
                BRA     SENDIT          ;Branch to send
;
;DOLLAR subroutine: Send "$" to terminal
DOLLAR          PHA                     ;Save A reg on STACK
                LDA     #$24            ;Get ASCII "$"
                BRA     SENDIT          ;Branch to send
;
;Send CR/LF to terminal
CR2             JSR     CROUT           ;Send CR/LF to terminal
CROUT           PHA                     ;Save A reg
                LDA     #$0D            ;Get ASCII Return
                JSR     B_CHROUT        ;Send to terminal
                LDA     #$0A            ;Get ASCII Linefeed
SENDIT          JSR     B_CHROUT        ;Send to terminal
                PLA                     ;Restore A reg
                RTS                     ;Return to caller
;
;GLINE subroutine: Send a horizontal line to console used by memory display only.
GLINE           LDX     #$4F            ;Load index for 79 decimal
                LDA     #$7E            ;Get "~" character
GLINEL          JSR     B_CHROUT        ;Send to terminal (draw a line)
                DEX                     ;Decrement count
                BNE     GLINEL          ;Branch back until done
                RTS                     ;Return to caller
;
;Routines to output 8/16-bit Binary Data and ASCII characters
; PRASC subroutine: Print A-reg as ASCII (Printable ASCII values = $20 - $7E), else print "."
PRASC           CMP     #$7F            ;Check for first 128
                BCS     PERIOD          ;If = or higher, branch
                CMP     #$20            ;Check for control characters
                BCS     ASCOUT          ;If space or higher, branch and print
PERIOD          LDA     #$2E            ;Else, print a "."
ASCOUT          JMP     B_CHROUT        ;Send byte in A-Reg, then return
;
;PRBYTE subroutine: Converts a single Byte to 2 HEX ASCII characters and sends to console on
; entry, A reg contains the Byte to convert/send. Register contents are preserved on entry/exit.
PRBYTE          PHA                     ;Save A register
                PHY                     ;Save Y register
PRBYT2          JSR     BIN2ASC         ;Convert A reg to 2 ASCII Hex characters
                JSR     B_CHROUT        ;Print high nibble from A reg
                TYA                     ;Transfer low nibble to A reg
                JSR     B_CHROUT        ;Print low nibble from A reg
                PLY                     ;Restore Y Register
                PLA                     ;Restore A Register
                RTS                     ;And return to caller
;
;PRINDEX subroutine: Prints a $ sign followed by INDEXH/L
PRINDEX         JSR     DOLLAR          ;Print a $ sign
                LDA     INDEXL          ;Get Index Low byte
                LDY     INDEXH          ;Get Index High byte
;
;PRWORD subroutine: Converts a 16-bit word to 4 HEX ASCII characters and sends to console. On
; entry, A reg contains Low Byte, Y reg contains High Byte. Registers are preserved on entry/exit.
; NOTE: Routine changed for consistency; A reg = Low byte, Y reg = High byte on 2nd May 2020
PRWORD          PHA                     ;Save A register (Low)
                PHY                     ;Save Y register (High)
                PHA                     ;Save Low byte again
                TYA                     ;Xfer High byte to A reg
                JSR     PRBYTE          ;Convert and print one HEX character (00-FF)
                PLA                     ;Get Low byte value
                BRA     PRBYT2          ;Finish up Low Byte and exit
;
;Continue routine: called by commands to confirm execution, when No is confirmed, return address
;is removed from stack and the exit goes back to the Monitor input loop.
;Short version prompts for (Y/N) only.
CONTINUE        LDA     #$00            ;Get msg "cont? (Y/N)" to terminal
                BRA     SH_CONT         ;Branch down
CONTINUE2       LDA     #$01            ;Get short msg "(Y/N)" only
SH_CONT         JSR     PROMPT          ;Send to terminal
TRY_AGN         JSR     RDCHAR          ;Get keystroke from terminal
                CMP     #$59            ;"Y" key?
                BEQ     DOCONT          ;if yes, continue/exit
                CMP     #$4E            ;if "N", quit/exit
                BEQ     DONTCNT         ;Return if not ESC
                JSR     BEEP            ;Send Beep to console
                BRA     TRY_AGN         ;Loop back, try again
DONTCNT         PLA                     ;Else remove return address
                PLA                     ;and discard it
                STZ     CMDFLAG         ;Clear all bits in command flag
DOCONT          RTS                     ;Return
;
;**************************************************************************************************
;*                              Monitor Command Processors                                        *
;**************************************************************************************************
;
;**************************************************************************************************
;*                              Timer/Counter based Monitor functions                             *
;**************************************************************************************************
;
;[,] Delay Setup Routine: This routine gets hex input via the console
;  - first is a hex byte ($00-$FF) for the 10-millisecond count
;  - second is a hex word ($0000-$FFFF) for the delay multiplier
;  - these are stored in variables SETMS, DELLO/DELHI
SET_DELAY       LDA     #$17            ;Get millisecond delay message
                JSR     HEX2            ;Use short cut version for print and input
                STA     SETMS           ;Else store millisecond count in variable
GETMULT         LDA     #$18            ;Get Multiplier message
                JSR     HEX4            ;Use short cut version for print and input
                STA     DELLO           ;Store Low byte
                STY     DELHI           ;Store High byte
                RTS                     ;Return to caller
;
;[\] Execute XL Delay: Get an 8-bit value for extra long delay, execute is entered.
SET_XLDLY       LDA     #$19            ;Get XL Loop message
                JSR     HEX2            ;Use short cut version for print and input
                STA     XDL             ;Save delay value
                LDA     #$0D            ;Get ASCII C/R
                JSR     B_CHROUT        ;Send C/R (show delay started, no L/F)
                JMP     B_EXE_XLDLY     ;Execute Extra Long delay loop
;
;[B] Benchmark Timer: Starts the benchmark timer.
; This routine displays a message to the console. A (y/n) prompt
; allows exit if required. Else it calls the BIOS routine at $FF30. This routine will
; clear the counters in page zero and enable the benchmark count routine.
; NOTE: The Benchmark counter is handled in BIOS and provides a 16-bit count of seconds
; along with a jiffy count with 10ms accuracy.
;
; The companion Q command quits the Benchmark count and displays the elapsed time as
; xxxxx.xx seconds.
BENCH           LDA     #$2A            ;Get message for Bench startup
                JSR     PROMPT          ;Send message to console
                JSR     CONTINUE2       ;Prompt user (y/n)
                JMP     B_CNT_INIT      ;Call BIOS routine to Init and start count, return
;
;[Q] Quit Benchmark timer: Quits the Benchmark timer.
; Benchmark timer is stopped by calling the BIOS routine. Once the Benchmark counter is
; is stopped, the HEX2ASC routine to print the 16-bit seconds count, followed by a period
; and then the HEX8ASC routine prints the hundreds count followed by the "Seconds" message.
QUITB           JSR     B_CNT_STOP      ;Stop Benchmark counter
                LDA     SECL_CNT        ;Get seconds low count
                LDY     SECH_CNT        ;Get seconds high count
                JSR     HEX2ASC         ;Print ASCII number
                JSR     PERIOD          ;Send "." to console
                LDA     MS10_CNT        ;Get hundreds of seconds
                JSR     HEX8ASC         ;Print hundreds of seconds
                LDA     #$49            ;Get message for " Seconds"
                JSR     PROMPT          ;Send to console
                JMP     CROUT           ;Send CR/LF and return
;
;**************************************************************************************************
;*                              Macro functions for testing/looping                               *
;**************************************************************************************************
;
;[(] INIMACRO command: Initialize keystroke input buffer
; initializes buffer head/tail pointers and resets buffer count to zero
; input buffer appears empty so command macro starts at the head of the buffer
INIMACRO        STZ     LPCNTL          ;Zero Loop count low byte
                STZ     LPCNTH          ;Zero Loop count high byte
                RMB6    CMDFLAG         ;Clear Loop Count flag Bit6
;
LP_CNT_FL       LDA     #$21            ;Get Loop Count msg
                JSR     PROMPT          ;Send to console
                LDA     #$01            ;Get short msg "(Y/N)" only
                JSR     PROMPT          ;Send to terminal
                JSR     RDCHAR          ;Get keystroke from terminal
                CMP     #$59            ;"Y" key?
                BEQ     DOLOOPS         ;If yes, set loop flag
                CMP     #$4E            ;"N" key?
                BEQ     NOLOOPS         ;If yes, don't set loop flag
                JSR     BEEP            ;Neither (Y/N) selected, sound bell
                BRA     LP_CNT_FL       ;Branch back, try again
;
DOLOOPS         SMB6    CMDFLAG         ;Set Loop Count flag Bit6
NOLOOPS         STZ     ICNT            ;Zero Input buffer count
                STZ     ITAIL           ;Zero Input buffer tail pointer
MACINI          STZ     IHEAD           ;Zero Input buffer head pointer
DONEFILL        RTS                     ;Return to caller
;
;[)] RUNMACRO command: Run Monitor command macro. This will indicate that there are 128 keystrokes
; in the keystroke input buffer. The Monitor will process these as if they were received from the
; terminal (typed-in by the user). Because the last keystroke stored in the keystroke buffer was
; ")", this will loop continuously. Use [SEND BREAK] to exit Macro
RUNMACRO        LDA     #$7F            ;Get keystroke buffer max length-1
                STA     ITAIL           ;Push tail pointer to end
                INC     A               ;Increment to $80 for buffer count (full)
                STA     ICNT            ;Make count show as full ($80)
                BBR6    CMDFLAG,NOLP_CNT ;If Loop flag clear, branch around it
                INC     LPCNTL          ;Increment loops low byte
                BNE     SKP_LPC         ;If not zero, skip high byte
                INC     LPCNTH          ;Increment loops high byte
SKP_LPC         LDA     #$22            ;Get Loops msg
                JSR     PROMPT          ;Send to console
                LDA     LPCNTL          ;Get Loop count low
                LDY     LPCNTH          ;Get Loop count high
                JSR     HEX2ASC         ;Print Loop count
                JSR     CROUT           ;Send C/R to console
NOLP_CNT        BRA     MACINI          ;Zero Head pointer and exit
;
;**************************************************************************************************
;*                      Basic Memory Operations (includes Ctrl-P)                                 *
;**************************************************************************************************
;
;[C] Compare routine: one memory range to another and display any addresses which do not match
;[M] Move routine: uses this section for parameter input, then branches to MOVER below
;[F] Fill routine: uses this section for parameter input but requires a fill byte value
;[CTRL-P] Program EEPROM: uses this section for parameter input and to write the EEPROM
;Uses source, target and length input parameters. Errors in compare are shown in target space.
FM_INPUT        LDA     #$05            ;Send "val: " to terminal
                JSR     HEX2            ;Use short cut version for print and input
                STA     TEMP2           ;Save fill byte to temp
                JSR     CONTINUE        ;Handle continue prompt
;
;Memory fill routine: parameter gathered below with Move/Fill,
; then a jump to here TEMP2 contains fill byte value
FILL_LP         LDA     LENL            ;Get length low byte
                ORA     LENH            ;OR in length high byte
                BEQ     DONEFILL        ;Exit if zero
                LDA     TEMP2           ;Get fill byte from TEMP2
                STA     (TGTL)          ;Store in target location
                JSR     UPD_TL          ;Update Target/Length pointers
                BRA     FILL_LP         ;Loop back until done
;
;Compare/Move/Fill memory operations ENTER HERE, branches as required
CPMVFL          STA     TEMP2           ;Save command character
                JSR     B_CHROUT        ;Print command character (C/M/F)
                CMP     #$46            ;Check for F - fill memory
                BNE     PRGE_E          ;If not, continue normal parameter input
                LDA     #$03            ;Get msg " addr:"
                BRA     F_INPUT         ;Branch to handle parameter input
;
;EEPROM write operation enters here
PROGEE          LDA     #$1C            ;Get PRG_EE msg
                JSR     PROMPT          ;Send to terminal
                STZ     TEMP2           ;Clear (Compare/Fill/Move)/error flag
;
PRGE_E          LDA     #$06            ;Get " src:" msg
                JSR     HEX4            ;Use short cut version for print and get input
                STA     SRCL            ;Else, store source address in variable SRCL,SRCH
                STY     SRCH            ;Store high address
                LDA     #$07            ;Get " tgt:" msg
F_INPUT         JSR     HEX4            ;Use short cut version for print and get input
                STA     TGTL            ;Else, store target address in variable TGTL,TGTH
                STY     TGTH            ;Store high address
                LDA     #$04            ;Get " len:" msg
                JSR     HEX4            ;Use short cut version for print and get input
                STA     LENL            ;ELSE, store length address in variable LENL,LENH
                STY     LENH            ;Store high address
;
; All input parameters for Source, Target and Length entered
                LDA     TEMP2           ;Get Command character
                CMP     #$46            ;Check for fill memory
                BEQ     FM_INPUT        ;Handle the remaining input
                CMP     #$43            ;Test for Compare
                BEQ     COMPLP          ;Branch if yes
                CMP     #$4D            ;Check for Move
                BEQ     MOVER           ;Branch if yes
;
; Command is to Program EEPROM
PROG_EE         LDA     #$1D            ;Get warning msg
                JSR     PROMPT          ;Send to console
                JSR     CONTINUE2       ;Prompt for (Y/N)
;
;Programming of the EEPROM is now confirmed by user. This routine will copy the core move and test
; routine from ROM to RAM, then call COMPLP to write and compare. As I/O can generate interrupts
; which point to ROM routines, all interrupts must be disabled during the program sequence.
;
;Send message to console for writing EEPROM
                LDA     #$1E            ;Get write message
                JSR     PROMPT          ;Send to console
OC_LOOP         LDA     OCNT            ;Check output buffer count
                BNE     OC_LOOP         ;Loop back until buffer sent
;
;Xfer byte write code to RAM for execution
                JSR     XFER_BYTE_WRT   ;Xfer byte write code to Page Zero
;
;Wait for 1/2 second for RAM/ROM access to settle
                LDA     #$32            ;Set milliseconds to 50(*10 ms)
                JSR     B_SET_DLY       ;Set Delay parameters
                JSR     B_EXE_MSDLY     ;Call delay for 1/2 second
;
PROG_EEP        SMB7    TEMP2           ;Set EEPROM write active mask
                JSR     COMPLP          ;Call routine to write/compare
                BBR6    TEMP2,PRG_GOOD  ;Skip down if no error
                LDA     #$20            ;Get Prog failed message
                BRA     BRA_PRMPT       ;Branch to Prompt routine
;
PRG_GOOD        LDA     #$1F            ;Get completed message
BRA_PRMPT       JMP     PROMPT          ;Send to console and exit
;
COMPLP          LDA     LENL            ;Get low byte of length
                ORA     LENH            ;OR in High byte of length
                BEQ     QUITMV          ;If zero, nothing to compare/write
                BBR7    TEMP2,SKP_BURN  ;Skip burn if bit 7 clear
                JSR     BURN_BYTE       ;Else Burn a byte to EEPROM
SKP_BURN        LDA     (SRCL)          ;Load source byte
                CMP     (TGTL)          ;Compare to target byte
                BEQ     CMP_OK          ;If compare is good, continue
;
                SMB6    TEMP2           ;Set bit 6 of TEMP2 flag (compare error)
                JSR     SPC2            ;Send 2 spaces
                JSR     DOLLAR          ;Print $ sign
                LDA     TGTL            ;Get Low byte of address
                LDY     TGTH            ;Get High byte of address
                JSR     PRWORD          ;Print word
                JSR     SPC             ;Add 1 space for formatting
;
CMP_OK          JSR     UPD_STL         ;Update pointers
                BRA     COMPLP          ;Loop back until done
;
;Parameters for move memory entered and validated, now make decision on which direction
; to do the actual move, if overlapping, move from end to start, else from start to end.
MOVER           JSR     CONTINUE        ;Prompt to continue move
                SEC                     ;Set carry flag for subtract
                LDA     TGTL            ;Get target lo byte
                SBC     SRCL            ;Subtract source lo byte
                TAX                     ;Move to X reg temporarily
                LDA     TGTH            ;Get target hi byte
                SBC     SRCH            ;Subtract source hi byte
                TAY                     ;Move to Y reg temporarily
                TXA                     ;Xfer lo byte difference to A reg
                CMP     LENL            ;Compare to lo byte length
                TYA                     ;Xfer hi byte difference to A reg
                SBC     LENH            ;Subtract length lo byte
                BCC     RIGHT           ;If carry is clear, overwrite condition exists
;
;Move memory block first byte to last byte, no overlap condition
MVNO_LP         LDA     LENL            ;Get length low byte
                ORA     LENH            ;OR in length high byte
                BEQ     QUITMV          ;Exit if zero bytes to move
                LDA     (SRCL)          ;Load source data
                STA     (TGTL)          ;Store as target data
                JSR     UPD_STL         ;Update Source/Target/Length variables
                BRA     MVNO_LP         ;Branch back until length is zero
;
;Move memory block last byte to first byte avoids overwrite in source/target overlap
RIGHT           LDX     LENH            ;Get the length hi byte count
                CLC                     ;Clear carry flag for add
                TXA                     ;Xfer High page to A reg
                ADC     SRCH            ;Add in source hi byte
                STA     SRCH            ;Store in source hi byte
                CLC                     ;Clear carry for add
                TXA                     ;Xfer High page to A reg
                ADC     TGTH            ;Add to target hi byte
                STA     TGTH            ;Store to target hi byte
                INX                     ;Increment high page value for use below in loop
                LDY     LENL            ;Get length lo byte
                BEQ     MVPG            ;If zero no partial page to move
                DEY                     ;Else, decrement page byte index
                BEQ     MVPAG           ;If zero, no pages to move
MVPRT           LDA     (SRCL),Y        ;Load source data
                STA     (TGTL),Y        ;Store to target data
                DEY                     ;Decrement index
                BNE      MVPRT          ;Branch back until partial page moved
MVPAG           LDA     (SRCL),Y        ;Load source data
                STA     (TGTL),Y        ;Store to target data
MVPG            DEY                     ;Decrement page count
                DEC     SRCH            ;Decrement source hi page
                DEC     TGTH            ;Decrement target hi page
                DEX                     ;Decrement page count
                BNE     MVPRT           ;Loop back until all pages moved
QUITMV          RTS                     ;Return to caller
;
;Xfer byte write code to RAM for execution
XFER_BYTE_WRT   LDX     #BYTE_WRE-BYTE_WRS+1 ;Get length of byte write code
BYTE_XFER       LDA     BYTE_WRS-1,X    ;Get code
                STA     BURN_BYTE-1,X   ;Write code to RAM
                DEX                     ;Decrement index
                BNE     BYTE_XFER       ;Loop back until done
                RTS                     ;Return to caller
;
BYTE_WRS        SEI                     ;Disable interrupts
                LDA     (SRCL)          ;Get source byte
                STA     (TGTL)          ;Write to target byte
                LDA     (TGTL)          ;Read target byte (EEPROM)
                AND     #%01000000      ;Mask off bit 6 - toggle bit
BYTE_WLP        STA     TEMP3           ;Store in Temp location
                LDA     (TGTL)          ;Read target byte again (EEPROM)
                AND     #%01000000      ;Mask off bit 6 - toggle bit
                CMP     TEMP3           ;Compare to last read (toggles if write mode)
                BNE     BYTE_WLP        ;Branch back if not done
                CLI                     ;Re-enable interrupts
BYTE_WRE        RTS                     ;Return to caller
;
;[D] HEX/TEXT DUMP command:
; Display in HEX followed by TEXT, the contents of 256 consecutive memory addresses
MDUMP           SMB7    CMDFLAG         ;Set bit7 of command flag
                JSR     SETUP           ;Request HEX address input from terminal
                BNE     LINED           ;Branch if new address entered (Z flag updated)
                LDA     TEMP1L          ;Else, point to next consecutive memory page
                STA     INDEXL          ;address saved during last memory dump
                LDA     TEMP1H          ;Xfer high byte of address
                STA     INDEXH          ;Save in pointer
LINED           JSR     DMPGR           ;Send address offsets to terminal
                JSR     GLINE           ;Send horizontal line to terminal
                JSR     CROUT           ;Send CR,LF to terminal
                LDX     #$10            ;Set line count for 16 rows
DLINE           JSR     SPC4            ;Send 4 Spaces to terminal
                JSR     PRINDEX         ;Print INDEX value
                JSR     SPC2            ;Send 2 Spaces to terminal
                LDY     #$00            ;Initialize line byte counter
GETBYT          JSR     SENGBYT         ;Use Search Engine Get Byte (excludes I/O)
                STA     SRCHBUFF,Y      ;Save in Search buffer (16 bytes)
                JSR     PRBYTE          ;Display byte as a HEX value
                JSR     SPC             ;Send Space to terminal
                JSR     INCINDEX        ;Increment Index to next byte location
                INY                     ;Increment index
                CPY     #$10            ;Check for all 16
                BNE     GETBYT          ;Loop back until 16 bytes have been displayed
                JSR     SPC             ;Send a space
                LDY     #$00            ;Reset index for SRCHBUFF
GETBYT2         LDA     SRCHBUFF,Y      ;Get buffered line (16 bytes)
                JSR     PRASC           ;Print ASCII character
                INY                     ;Increment index to next byte
                CPY     #$10            ;Check for 16 bytes
                BNE     GETBYT2         ;Loop back until 16 bytes have been displayed
                JSR     CROUT           ;Else, send CR,LF to terminal
                LDA     INDEXL          ;Get current index low
                STA     TEMP1L          ;Save to temp1 low
                LDA     INDEXH          ;Get current index high
                STA     TEMP1H          ;Save to temp1 high
                DEX                     ;Decrement line count
                BNE     DLINE           ;Branch back until all 16 done
                JSR     GLINE           ;Send horizontal line to terminal
;
;DMPGR subroutine: Send address offsets to terminal
DMPGR           LDA     #$02            ;Get msg for "addr:" to terminal
                JSR     PROMPT          ;Send to terminal
                JSR     SPC2            ;Add two additional spaces
                LDX     #$00            ;Zero index count
MDLOOP          TXA                     ;Send "00" thru "0F", separated by 1 Space, to terminal
                JSR     PRBYTE          ;Print byte value
                JSR     SPC             ;Add a space
                INX                     ;Increment the count
                CPX     #$10            ;Check for 16
                BNE     MDLOOP          ;Loop back until done
;
;Print the ASCII text header "0123456789ABCDEF"
                JSR     SPC             ;Send a space
                LDX     #$00            ;Zero X reg for "0"
MTLOOP          TXA                     ;Xfer to A reg
                JSR     BIN2ASC         ;Convert Byte to two ASCII digits
                TYA                     ;Xfer the low nibble character to A reg
                JSR     B_CHROUT        ;Send least significant HEX to terminal
                INX                     ;Increment to next HEX character
                CPX     #$10            ;Check for 16
                BNE     MTLOOP          ;Branch back till done
                JMP     CROUT           ;Do a CR/LF and return
;
;[E] Examine/Edit command: Display in HEX then change the contents of a specified memory address
CHANGE          JSR     SETUP           ;Request HEX address input from terminal
CHNG_LP         LDA     INDEXH          ;Test high byte address for RAM (less than $80)
                BMI     NO_RAM          ;Address not in RAM
                JSR     SPC2            ;Send 2 spaces
                LDA     (INDEXL)        ;Read specified address
                JSR     PRBYTE          ;Display HEX value read
                JSR     BSOUT3T         ;Send 3 Backspaces
                JSR     HEXIN2          ;Get input, result in A reg
                STA     (INDEXL)        ;Save entered value at Index pointer
                CMP     (INDEXL)        ;Compare to ensure a match
                BEQ     CHOK            ;Branch if compare is good
                LDA     #$3F            ;Get "?" for bad compare
                JSR     B_CHROUT        ;Send to terminal
CHOK            JSR     INCINDEX        ;Increment Index
                BRA     CHNG_LP         ;Loop to continue command
NO_RAM          LDA     #$2C            ;Get address range message
                JSR     PROMPT          ;Send to console
                LDA     #$2E            ;Get RAM message
                JSR     PROMPT          ;Send to console
                BRA     CHANGE          ;Branch back and try again
;
;[G] GO command: Begin executing program code at a specified address.
; Prompts the user for a start address, places it in COMLO/COMHI. If no address entered,
; uses default address at COMLO/COMHI. Loads the A,X,Y,P registers from presets and does
; a JSR to the routine. Upon return, registers are saved back to presets for display later.
; Also saves the stack pointer and status register upon return.
; Note: Stack pointer is not changed due to IRQ service routines.
GO              SMB7    CMDFLAG         ;Set bit7 of command flag
                JSR     SETUP           ;Get HEX address (A/Y regs hold 16-bit value)
                BEQ     EXEC_GO         ;If not, setup REGs and execute (Z flag updated)
                STA     COMLO           ;Save entered address to pointer low byte
                STY     COMHI           ;Save entered address to pointer hi byte
;
;Preload all 65C02 MPU registers from Monitor's preset/result variables
EXEC_GO         LDA     PREG            ;Load processor status register preset
                PHA                     ;Push it to the stack
                LDA     AREG            ;Load A-Reg preset
                LDX     XREG            ;Load X-Reg preset
                LDY     YREG            ;Load Y-Reg preset
                PLP                     ;Pull the processor status register
;
;Call user program code as a subroutine
                JSR     DOCOM           ;Execute code at specified address
;
;Store all 65C02 MPU registers to Monitor's preset/result variables: store results
                PHP                     ;Save the processor status register to the stack
                STA     AREG            ;Store A-Reg result
                STX     XREG            ;Store X-Reg result
                STY     YREG            ;Store Y-Reg result
                PLA                     ;Get the processor status register
                STA     PREG            ;Store the result
                TSX                     ;Xfer stack pointer to X-reg
                STX     SREG            ;Store the result
                CLD                     ;Clear BCD mode in case of sloppy user code ;-)
TXT_EXT         RTS                     ;Return to caller
DOCOM           JMP     (COMLO)         ;Execute the command
;
;[T] LOCATE TEXT STRING command: search memory for an entered text string
;Memory range scanned is $0800 through $FFFF (setup before SENGINE subroutine is invoked)
;SRCHTXT subroutine: request 1 - 16 character text string from terminal, followed by Return
;[ESCAPE] aborts, [BACKSPACE] erases last keystroke. String will be stored in SRCHBUFF
SRCHTXT         LDA     #$08            ;Get msg " find text:"
                JSR     PROMPT          ;Send to terminal
                LDX     #$00            ;Initialize index/byte counter
STLOOP          JSR     B_CHRIN         ;Get input from terminal
                CMP     #$0D            ;Check for C/R
                BEQ     SRCHRDY1        ;Branch to search engine
                CMP     #$1B            ;Check for ESC
                BEQ     TXT_EXT         ;Exit to borrowed RTS
                CMP     #$08            ;Check for B/S
                BNE     STBRA           ;If not, store character into buffer
                TXA                     ;Xfer count to A reg
                BEQ     STLOOP          ;Branch to input if zero
                JSR     BSOUT           ;Else, send B/S to terminal
                DEX                     ;Decrement index/byte counter
                BRA     STLOOP          ;Branch back and continue
STBRA           STA     SRCHBUFF,X      ;Store character in buffer location
                JSR     B_CHROUT        ;Send character to terminal
                INX                     ;Increment counter
                CPX     #$10            ;Check count for 16
                BNE     STLOOP          ;Loop back for another character
                LDA     #$08            ;Get start address for $0800
                BRA     SRCHRDY1        ;Branch to search engine
;
;[H] LOCATE BYTE STRING command: Search memory for an entered byte string. Memory range
; scanned is $0400 through $FFFF. SRCHBYT subroutine: request 0 - 16 byte string from
; terminal, byte followed by [RETURN]. [ESCAPE] aborts. HEX data will be stored in SRCHBUFF.
; minor update from Mike Barry, saves a byte.
SRCHBYT         SMB7    CMDFLAG         ;Set bit7 of command flag
                LDA     #$09            ;Get msg " find bin:"
                JSR     PROMPT          ;Send to terminal
                LDX     #$00            ;Initialize index
SBLOOP          PHX                     ;Save index on stack
                JSR     HEXIN2          ;Request HEX byte
                JSR     SPC             ;Send space to terminal
                PLX                     ;Restore index from stack
                LDY     BUFIDX          ;Get # of characters entered
                BEQ     SRCHRDY         ;Branch if no characters
                STA     SRCHBUFF,X      ;Else, store in buffer
                INX                     ;Increment index
                CPX     #$10            ;Check for 16 (max)
                BNE     SBLOOP          ;Loop back until done/full
SRCHRDY         LDA     #$04            ;Get start address for $0400
SRCHRDY1        STA     INDEXH          ;Store to Index high byte
                STZ     INDEXL          ;Zero Index low byte ($XX00)
                STX     IDX             ;Save input character count
                TXA                     ;Check buffer count (for zero)
                BEQ     NOWRAP          ;Exit if no bytes in buffer
;
;SENGINE subroutine: Scan memory range $XX00 through $FFFF for exact match to string contained
; in buffer SRCHBUFF (1 to 16 bytes/characters). Display address of first byte/character of
; each match found until the end of memory is reached.
SENGINE         LDA     #$0C            ;Else, get msg "Searching.."
                JSR     PROMPT          ;Send to terminal
SENGBR2         LDX     #$00            ;Initialize buffer index
SENGBR3         JSR     SENGBYT         ;Get the next byte from Index pointer
                CMP     SRCHBUFF,X      ;Compare to search buffer
                BEQ     SENGBR1         ;Branch for a match
                JSR     SINCPTR         ;Increment pointer, test for end of memory
                BRA     SENGBR2         ;Loop back to continue
SENGBR1         JSR     SINCPTR         ;Increment pointer, test for end of memory
                INX                     ;Increment buffer index
                CPX     IDX             ;Compare buffer index to address index
                BNE     SENGBR3         ;Loop back until done
                SEC                     ;Subtract buffer index from memory pointer; Set carry
                LDA     INDEXL          ;Get current address for match lo byte
                SBC     IDX             ;Subtract from buffer index
                STA     INDEXL          ;Save it back to lo address pointer
                LDA     INDEXH          ;Get current address for match hi byte
                SBC     #$00            ;Subtract carry flag
                STA     INDEXH          ;Save it back to hi address pointer
                LDA     #$0B            ;Get msg "found"
                JSR     PROMPT          ;Send to terminal
                LDA     #':'            ;Get Ascii colon
                JSR     B_CHROUT        ;Send to console
                JSR     PRINDEX         ;Print Index address
                LDA     #$0D            ;Get msg "(n)ext? "
                JSR     PROMPT          ;Send to terminal
                JSR     RDCHAR          ;Get input from terminal
                CMP     #$4E            ;Check for "(n)ext"
                BNE     NOWRAP          ;Exit if not requesting next
                JSR     SINCPTR         ;Increment address pointer, test for end of memory
                BRA     SENGBR2         ;Branch back and continue till done
;
;Search Engine GetByte routine: This routine gets the byte value from the current Index pointer
; location. It also checks the Index location FIRST. The I/O page is excluded from the actual data
; search to prevent corrupting any I/O devices which are sensitive to any READ operations outside
; the BIOS which supports it. An example is the NXP UART family, of which the SCC2691 is used here.
; Current I/O Page Range is $FE00 - $FE9F
; NOTE: $FEA0 - $FEFF used for vector/config data - allows searching here
SENGBYT         LDA     INDEXH          ;Get High byte address for current Index
                CMP     #$FE            ;Check for Base I/O page
                BEQ     CHK_UPR         ;If yes, check for I/O range
SENRTBYT        LDA     (INDEXL)        ;Else Get byte from current pointer
                RTS                     ;Return to caller
CHK_UPR         LDA     INDEXL          ;Get Low byte address for current Index
                CMP     #$A0            ;Check for end of I/O addresses
                BCS     SENRTBYT        ;Return ROM data if range is $FEA0 or higher
                LDA     #$FE            ;Get $FE as seed byte instead of I/O device read
NOWRAP          RTS                     ;Return to caller
;
;Increment memory address pointer. If pointer high byte = 00 (end of searchable ROM memory),
; send "not found" to terminal then return to Monitor
SINCPTR         JSR     INCINDEX        ;Increment Index pointer
                LDA     INDEXH          ;Check for wrap to $0000
                BNE     NOWRAP          ;If not, return
                PLA                     ;Else, Pull return address from stack
                PLA                     ;and exit with msg
                LDA     #$0A            ;Get msg "not found"
                JMP     PROMPT          ;Send msg to terminal and return
;
;[I] command: TEXT ENTRY enter ASCII text beginning at a specified address
TEXT            JSR     SETUP           ;Send "I" command, handle setup
EDJMP1          JSR     CROUT           ;Send CR,LF to terminal
                STA     TEMP2L          ;Save current edit address
                STY     TEMP2H          ;Save high byte
EDJMP2          JSR     B_CHRIN         ;Request a keystroke from terminal
                CMP     #$1B            ;Check for end text entry
                BEQ     EDITDUN         ;Branch and close out if yes
                CMP     #$0D            ;Else, check for Return key
                BNE     ENOTRET         ;Branch if not
                STA     (INDEXL)        ;Save CR to current Index pointer
                JSR     INCINDEX        ;Increment edit memory address pointer
                LDA     #$0A            ;Get a LF character
                STA     (INDEXL)        ;Store it in memory
                JSR     INCINDEX        ;Increment edit memory address pointer
                LDA     INDEXL          ;Get Start of next line
                LDY     INDEXH          ;and the high byte
                BRA     EDJMP1          ;Loop back to continue
ENOTRET         CMP     #$08            ;Check for backspace character
                BEQ     EDBKSPC         ;Branch if yes
                STA     (INDEXL)        ;Else, save to current Index pointer
                JSR     B_CHROUT        ;Send keystroke to terminal
                JSR     INCINDEX        ;Increment edit memory address pointer
                BRA     EDJMP2          ;Loop back to EDJMP2
;
;Handle Backspace, don't allow past starting address
EDBKSPC         LDA     INDEXL          ;Get current index low byte
                CMP     TEMP2L          ;Compare to initial start address
                BNE     EDDOBKS         ;If not equal, perform backspace
                LDA     INDEXH          ;Get current index high byte
                CMP     TEMP2H          ;Compare to initial start address
                BEQ     EDJMP2          ;If same, branch to input loop
EDDOBKS         JSR     BSOUT           ;Send backspace to terminal
                JSR     DECINDEX        ;Decrement edit memory address pointer
                LDA     #$00            ;Get a null character
                STA     (INDEXL)        ;Store in place of character
                BRA     EDJMP2          ;LOOP back to EDJMP2
EDITDUN         JSR     CR2             ;Send 2 CR,LF to terminal
                JMP     PRINDEX         ;Print INDEX value and return
;
;**************************************************************************************************
;*                              Processor Register Operations                                     *
;**************************************************************************************************
;
;[P] Processor Status command: Display then change PS preset/result
PRG             LDA     #$0E            ;Get MSG # for Processor Status register
                BRA     REG_UPT         ;Finish register update
;
;[S] Stack Pointer command: Display then change SP preset/result
SRG             LDA     #$0F            ;Get MSG # for Stack register
                BRA     REG_UPT         ;Finish Register update
;
;[Y] Y-Register command: Display then change Y-reg preset/result
YRG             LDA     #$10            ;Get MSG # for Y Reg
                BRA     REG_UPT         ;Finish register update
;
;[X] X-Register command: Display then change X-reg preset/result
XRG             LDA     #$11            ;Get MSG # for X Reg
                BRA     REG_UPT         ;Finish register update
;
;[A] A-Register command: Display then change A-reg preset/result
ARG             LDA     #$12            ;Get MSG # for A reg
;
REG_UPT         PHA                     ;Save MSG # to stack
                PHA                     ;Save MSG # to stack again
                JSR     PROMPT          ;Print Register message
                PLX                     ;Get Index to registers
                LDA     PREG-$0E,X      ;Read Register (A,X,Y,S,P) preset/result
                JSR     PRBYTE          ;Display HEX value of register
                JSR     SPC             ;Send [SPACE] to terminal
                JSR     HEXIN2          ;Get up to 2 HEX characters
                PLX                     ;Get MSG # from stack
                STA     PREG-$0E,X      ;Write register (A,X,Y,S,P) preset/result
MNE_QUIT        RTS                     ;Return to caller
;
;[R] REGISTERS command: Display contents of all preset/result memory locations
PRSTAT          JSR     B_CHROUT        ;Send "R" to terminal
PRSTAT1         LDA     #$13            ;Get Header msg
                JSR     PROMPT          ;Send to terminal
                LDA     PCL             ;Get PC Low byte
                LDY     PCH             ;Get PC High byte
                JSR     PRWORD          ;Print 16-bit word
                JSR     SPC             ;Send 1 space
;
                LDX     #$04            ;Set for count of 4
REGPLOOP        LDA     PREG,X          ;Start with A reg variable
                JSR     PRBYTE          ;Print it
                JSR     SPC             ;Send 1 space
                DEX                     ;Decrement count
                BNE     REGPLOOP        ;Loop back till all 4 are sent
;
                LDA     PREG            ;Get Status register preset
                LDX     #$08            ;Get the index count for 8 bits
SREG_LP         ASL     A               ;Shift bit into Carry
                PHA                     ;Save current (shifted) SR value
                LDA     #$30            ;Load an Ascii zero
                ADC     #$00            ;Add zero (with Carry)
                JSR     B_CHROUT        ;Print bit value (0 or 1)
                PLA                     ;Get current (shifted) SR value
                DEX                     ;Decrement bit count
                BNE     SREG_LP         ;Loop back until all 8 printed
                JMP     CROUT           ;Send CR/LF and return to caller
;
;**************************************************************************************************
;*                              Control Key Operations (Ctrl-?)                                   *
;**************************************************************************************************
;
;[CTRL-A] Start Assembler:
ASSEMBLER       LDA     #$2F            ;Get Intro message
                JSR     PROMPT          ;Send to console
                LDA     #$03            ;Get Msg 03 -" addr:"
                JSR     HEX4            ;Print msg and get address
MNE_RETRY
                JSR     CROUT           ;Send CR,LF to Console
                JSR     PRINDEX         ;Send starting address to Console
                JSR     SPC4            ;Send 4 spaces
;
; Now get an Instruction entry and put into SRCHBUFF.
; This allows for a full instruction line entry which is buffered. This routine also allows
; editing of the entry input and uses a C/R to exit to the next routine to start instruction
; decode.
; NOTE: SRCHBUFF is used by multiple routines, so it won't show the entered data if you leave
; the assembler and use the "D" (display memory) command. 
;
                LDX     #$00            ;Set input count to zero
MNE_LOOP        JSR     RDCHAR          ;Get input from terminal
                CMP     #$0D            ;Check for CR
                BEQ     MNE_CRUNCH      ;Branch to parse routine
                CMP     #$1B            ;Check for ESC
                BEQ     MNE_QUIT        ;Exit to borrowed RTS
                CMP     #$08            ;Check for Backspace
                BNE     MNE_BRA         ;If not, store character into buffer
                TXA                     ;Xfer count to A reg
                BEQ     MNE_LOOP        ;Branch to input if zero
                JSR     BSOUT           ;Else, send Backspace to terminal
                DEX                     ;Decrement index/byte counter
                BRA     MNE_LOOP        ;Branch back and continue
MNE_BRA         STA     SRCHBUFF,X      ;Store character in buffer location
                JSR     B_CHROUT        ;Send character to terminal
                INX                     ;Increment counter
                CPX     #$0F            ;Check count for 15
                BNE     MNE_LOOP        ;Loop back for another character
                JSR     BEEP            ;Send Beep to Console (buffer overflow)
                BRA     MNE_RETRY       ;Branch back and try again
;
MNE_CRUNCH
                STX     SRCHBUFF+15     ;Save Input count at end of buffer
;
; Just for test purposes, we're storing the entered line at $0700
;
                STX     $070F           ;Save Input buffer count
                LDX     #$0E            ;Set count of 14
MNE_XFER        LDA     SRCHBUFF,X      ;Load input buffer
                STA     $0700,x         ;Save it to a temp area
                DEX                     ;Decrement count
                BPL     MNE_XFER        ;Branch back until done
;
;                BRK                     ;Debug entry
;
                JMP     $0800           ;Jump to RAM development code
;
;[CTRL-B] Boot DOS/65 ROM Version:
EHBASIC         JMP     DOS_65          ;Start Address for DOS/65 ROM
;
;[CTRL-D] Disassembler: Table-Driven Disassembler. Supports ALL W65C02 Opcodes and Address modes.
DISASSEMBLER    LDA     #$29            ;Intro Message
                JSR     PROMPT          ;Send to terminal
                LDA     #$03            ;Msg 03 -" addr:"
                JSR     HEX4            ;Print msg and get address
                JSR     CROUT           ;Send CR,LF to terminal
RPT_LIST        LDX     #$16            ;Set list count to 22
DIS_LOOP        PHX                     ;Push count to stack
                JSR     DIS_LINE        ;Disassemble 1 instruction
                PLX                     ;Pull count from stack
                DEX                     ;Decrement count
                BNE     DIS_LOOP        ;Loop back till list count is zero
LST_LOOP        JSR     B_CHRIN         ;Get input from terminal
                CMP     #$0D            ;Check for Return key
                BEQ     RPT_LIST        ;If yes, repeat list
                CMP     #$1B            ;Check for Escape
                BEQ     EXT_LIST        ;If yes, exit
                CMP     #$20            ;Check for space
                BEQ     LIST_ONE        ;If yes, disassemble 1 line
                JSR     BEEP            ;Else, beep for error
                BRA     LST_LOOP        ;Loop back again
LIST_ONE        JSR     DIS_LINE        ;Else, Disassemble one line
                BRA     LST_LOOP        ;Branch back and continue
;
;DISASSEMBLE LINE: disassemble 1 instruction from working address
DIS_LINE        STZ     TEMP2           ;Clear all flag bits
                JSR     PRINDEX         ;Print working address
                JSR     SPC2            ;Send 2 spaces to terminal
                LDA     (INDEXL)        ;Read opcode from working memory pointer
                STA     OPXMDM          ;Save opcode
                JSR     PRB_SPC2        ;Print byte, 2 spaces
                LSR     A               ;Divide by 2 / shift low order bit into carry flag
                TAX                     ;Xfer Opcode /2 to X reg
                LDA     HDLR_IDX,X      ;Get Pointer to handler table
                BCS     USE_RGHT        ;If carry set use low nibble (odd)
                LSR     A               ;Else shift upper nibble to lower nibble (even)
                LSR     A
                LSR     A
                LSR     A
USE_RGHT        AND     #$0F            ;Mask off high nibble
                ASL     A               ;Multiply by 2 for index
                TAX                     ;Use handler pointer to index handler table
                JSR     DODISL          ;Call disassembler handler
                JSR     CROUT           ;Send CR,LF to terminal
;
;INCNDX routine: increment working address pointer then read it
INCNDX          JSR     INCINDEX        ;Increment working address pointer
                LDA     (INDEXL)        ;Read from working memory address
EXT_LIST        RTS                     ;Done, return to caller/exit
;
DODISL          JMP     (HDLR_TAB,X)    ;Execute address mode handler
;
;THREE BYTE routine: display operand bytes then mnemonic for three-byte instruction
; TWO BYTE routine: display operand byte then mnemonic for two-byte instruction
TRI_BYTE        SMB7    TEMP2           ;Set Flag bit for 3-byte instruction
TWO_BYTE        JSR     GET_NEXT        ;Read, display operand byte
                STA     CRCLO           ;Save operand byte in CRCLO
                BBR7    TEMP2,2BYTSPC   ;Branch for 2-byte is clear
                JSR     GET_NEXT        ;Read, display operand high byte
                STA     CRCHI           ;Save operand high byte in CRCHI
                BRA     3BYTSPC         ;Send 2 spaces, send Mnemonic, return
;
;IMPLIED disassembler handler: single byte instructions: implied mode
; (note: ACC_MODE handler calls this)
IMPLIED         JSR     SPC4            ;Send 4 spaces
2BYTSPC         JSR     SPC4            ;Send 4 spaces
3BYTSPC         JSR     SPC2            ;Send 2 spaces
;
;PRT_MNEM subroutine: send 3 character mnemonic to terminal
; Mnemonic indexed by opcode byte. Sends "???" if byte is not a valid opcode
PRT_MNEM        LDY     OPXMDM          ;Get current Opcode as index
                LDX     MNE_PTAB,Y      ;Get opcode pointer from table
                LDA     DIS_NMEM,X      ;Get left byte
                STA     PTRL            ;Store it to pointer
                LDA     DIS_NMEM+1,X    ;Get right byte
                STA     PTRH            ;Store it to pointer
                LDX     #$03            ;Set count for 3 characters
NEXT_NME        LDA     #$00            ;Zero A reg
                LDY     #$05            ;Set count for 5 bits per character
LOOP_NME        ASL     PTRH            ;Shift right byte into carry
                ROL     PTRL            ;Rotate left byte into A reg
                ROL     A               ;Rotate into A reg
                DEY                     ;Decrement bit count
                BNE     LOOP_NME        ;Loop back till 5 bits in A reg
                ADC     #$3F            ;Add $3F to convert to Ascii
                JSR     B_CHROUT        ;Send the character to terminal
                DEX                     ;Decrement character count
                BNE     NEXT_NME        ;Loop back till 3 characters sent
                BRA     BR_SPC2         ;Send 2 spaces to terminal, return
;
;GET_NEXT subroutine: increment/read working address
; Display byte, send 2 spaces to terminal (displays operand byte(s))
GET_NEXT        JSR     INCNDX          ;Increment working index
PRB_SPC2        JSR     PRBYTE          ;Display Byte from working index
BR_SPC2         JMP     SPC2            ;Send 2 spaces to terminal and return
;
;Disassembler handlers:
;
;LF_BRKT subroutine: send "(" to terminal
LF_BRKT         LDA     #$28            ;Get "("
                BRA     BR_COUT         ;Send to terminal and return
;
;ZP_IMMEDIATE: two byte instructions: zero-page immediate mode
ZP_IMED         JSR     TWO_BYTE        ;Display operand byte, then mnemonic
                LDA     #$23            ;Get "#" character
                JSR     B_CHROUT        ;Send to terminal
                BRA     PRT1_OP         ;Display operand byte again, return
;
;ACC_MODE: single byte A reg mode instructions: implied mode
ACC_MODE        JSR     IMPLIED         ;Send 10 spaces to terminal then display mnemonic
                LDA     #$41            ;Get "A" character
BR_COUT         JMP     B_CHROUT        ;Send it and return
;
;ABSOLUTE: three byte instructions: absolute mode
ABSOLUTE        JSR     TRI_BYTE        ;Display operand bytes, then mnemonic
;
;Print 2 Operands: display operand bytes of a three-byte instruction
PRT2_OP         JSR     DOLLAR          ;Send "$" to terminal
                LDA     CRCHI           ;Load operand high byte
                JSR     PRBYTE          ;Send to terminal
BR_PRBTE        LDA     CRCLO           ;Load operand low byte
                JMP     PRBYTE          ;Send to terminal and return
;
;ZP_ABS: two byte instructions: zero-page absolute
ZP_ABS          JSR     TWO_BYTE        ;Display operand byte, then mnemonic
;
;Print 1 Operand byte: display operand byte of a two-byte instruction
PRT1_OP         JSR     DOLLAR          ;Send "$" to terminal
                BRA     BR_PRBTE        ;Branch to complete
;
;INDIRECT: two or three byte instructions: indirect modes
INDIRECT        LDA     OPXMDM          ;Read saved opcode byte
                CMP     #$6C            ;Check for JMP(INDIRECT)
                BNE     ZP_IND          ;Branch if not
;
                JSR     TRI_BYTE        ;Display operand bytes, then mnemonic
                JSR     LF_BRKT         ;Send "(" to terminal
                JSR     PRT2_OP         ;Display operand bytes again
                BRA     RT_BRKT         ;Send ")" to terminal, return
;
;Following group is used multiple times, space savings
DSPLY3          JSR     TWO_BYTE        ;Display operand byte, then mnemonic
                JSR     LF_BRKT         ;Send "(" to terminal
                BRA     PRT1_OP         ;Display operand byte again, return
;
;this is for a two byte instruction: zero page indirect mode
ZP_IND          JSR     DSPLY3          ;Do the 3 routines
;
;RT_BRKT subroutine: send ")" to terminal
RT_BRKT         LDA     #$29            ;Get ")"
                BRA     BR_COUT         ;Send to terminal and return
;
;ZP_ABS_X: two byte instructions: zero-page absolute indexed by X mode
ZP_ABS_X        JSR     ZP_ABS          ;Display operand byte, mnemonic, operand byte
;
;Print Comma,X: send ",X" to terminal
COM_X           LDA     #$2C            ;Get ","
                JSR     B_CHROUT        ;Send to terminal
                LDA     #$58            ;Get "X"
                BRA     BR_COUT         ;Send to terminal, return
;
;ZP_ABS_Y: two byte instructions: zero-page absolute indexed by Y mode
ZP_ABS_Y        JSR     ZP_ABS          ;Display operand byte, mnemonic, operand byte
;
;Print Comma,Y: send ",Y" to terminal
COM_Y           LDA     #$2C            ;Get ","
                JSR     B_CHROUT        ;Send to terminal
                LDA     #$59            ;Get "Y"
                BRA     BR_COUT         ;Send to terminal, return
;
;ABS_Y: three byte instructions: absolute indexed by Y mode
;ABS_X: three byte instructions: absolute indexed by X mode
ABS_Y           SMB6    TEMP2
ABS_X           JSR     TRI_BYTE        ;Display operand bytes, then mnemonic
                JSR     PRT2_OP         ;Display operand bytes again
                BBS6    TEMP2,COM_Y
                BRA     COM_X           ;Send ",X" to terminal, return
;
;ZP_IND_X: two byte instructions: zero-page indirect pre-indexed by X mode
ZP_IND_X        JSR     DSPLY3          ;Do the 3 routines
                JSR     COM_X           ;Send ",X" to terminal
                BRA     RT_BRKT         ;Send ")" to terminal, return
;
;ZP_IND_Y: two byte instructions: zero-page indirect post-indexed by Y mode
ZP_IND_Y        JSR     DSPLY3          ;Do the 3 routines
                JSR     RT_BRKT         ;Send ")" to terminal
                BRA     COM_Y           ;Send ",Y" to terminal, return
;
;IND_ABS_X: three byte instruction: JMP (INDIRECT,X) 16 bit indirect
IND_ABS_X       JSR     TRI_BYTE        ;Display operand bytes, then mnemonic
                JSR     LF_BRKT         ;Send "(" to terminal
                JSR     PRT2_OP         ;Display operand bytes again
                JSR     COM_X           ;Send ",X" to terminal
                BRA     RT_BRKT         ;Send ")" to terminal,done w/INDABSX handler, return
;
;ZP_XMB: two byte instructions: zero page set/clear memory bit
ZP_XMB          JSR     SRMB            ;Display operand/mnemonic, isolate bit selector from opcode
                CMP     #$08            ;Check if 0-7 or 8-F
                BCC     SRBIT           ;Just add $30 (0-7)
                SBC     #$08            ;Subtract $08 - convert $8-$F to $0-$7
SRBIT           CLC                     ;Convert bit selector value to an ASCII decimal digit
                ADC     #$30            ;Add "0" to bit selector value
                JSR     B_CHROUT        ;Send digit to terminal
                JSR     SPC             ;Send a space to terminal
                BRA     PRT1_OP         ;Display operand byte again then return
;
;ZP_BBX: three byte instruction: branch on zero-page bit set/clear
ZP_BBX          JSR     SRMB2           ;Display operand/mnemonic, isolate bit selector from opcode
                CMP     #$08            ;Check if $0-$7 or $8-$F
                BCC     SRBIT2          ;Just add $30 ($0-$7)
                SBC     #$08            ;Subtract $08 - convert $8-$F to $0-$7
SRBIT2          JSR     SRBIT           ;Convert and display bit selector digit
                LDA     CRCHI           ;Move second operand to first operand position:
                STA     CRCLO           ;CRCLO = branch offset
                JSR     SPC             ;Send a space to terminal
                BRA     BBX_REL         ;Display branch target address then return
;
;RELATIVE BRANCH: two byte relative branch mode BBX_REL: three byte relative branch mode
; Both calculate then display relative branch target address
;
; Update 12th February 2021: Fix offset page for Branch. High byte address would not
; update properly and could show the wrong page.
REL_BRA         JSR     TWO_BYTE        ;Display operand byte, then mnemonic
BBX_REL         JSR     DOLLAR          ;Send "$" to terminal
                JSR     INCINDEX        ;Increment address, ref for branch offset
                LDA     CRCLO           ;Get branch operand value
                BMI     BRA_MINUS       ;Check for $80 or higher (branch is + / -)
                CLC                     ;Clear carry for add
                ADC     INDEXL          ;Add to Index lo
                PHA                     ;Save result to Stack
                LDA     INDEXH          ;Get Index Hi
                ADC     #$00            ;Add result from Carry flag to A reg
                TAY                     ;Xfer Hi address to Y Reg
                PLA                     ;Get Lo Address from Stack
                BRA     REL_EXT         ;Print offset, cleanup, return
BRA_MINUS       EOR     #$FF            ;Get 1's complement of offset
                INC     A               ;Increment by 1
                STA     TEMP3           ;Save result
                SEC                     ;Set carry for subtract
                LDA     INDEXL          ;Get address Low byte
                SBC     TEMP3           ;Subtract branch offset
                PHA                     ;Save result to Stack
                LDA     INDEXH          ;Get address High byte
                SBC     #$00            ;Subtract carry flag
                TAY                     ;Xfer Hi address to Y reg
                PLA                     ;Get Lo address from Stack
REL_EXT         JSR     PRWORD          ;Send address to terminal
                JMP     DECINDEX        ;Decrement working address, return
;
;SRMB2 subroutine: display 2 operand bytes, mnemonic, isolate bit selector from opcode
; SRMB subroutine: display 1 operand byte, mnemonic, isolate bit selector from opcode
SRMB2           LDA     (INDEXL)        ;Read from working index
                PHA                     ;Save byte to stack
                JSR     TRI_BYTE        ;Display operand bytes and mnemonic
                BRA     SRM             ;Skip down
SRMB            LDA     (INDEXL)        ;Read from working index
                PHA                     ;Save byte on STACK
                JSR     TWO_BYTE        ;Display operand byte and mnemonic
SRM             JSR     BSOUT2T         ;Send 2 Backspaces
                PLA                     ;Restore byte from stack
                LSR     A               ;Shift high nibble to low nibble
                LSR     A
                LSR     A
                LSR     A
                RTS                     ;Done SRMB2/SRMB, return
;
;[CNTRL-E] EEPROM Edit
; Allows editing of a single byte location within the EEPROM starting at $8000.
; Note that this routines tests for target addresses at $8000 or above, but does NOT
; test for I/O address ranges. One should be cautious about editing the EEPROM directly
; to ensure it doesn't become unusable by changing things wrongly.
EEDIT           LDA     #<TEMP2         ;Get address offset for TEMP2
                STA     SRCL            ;Store in Source address Low
                STZ     SRCH            ;Zero Source address High (Page Zero)
;
;Xfer byte write code to RAM for execution
                JSR     XFER_BYTE_WRT   ;Xfer byte write code to Page Zero
;
                LDA     #$2B            ;Get EEPROM edit msg
TRY_AGAIN       JSR     PROMPT          ;Send to console
;
CMP_OK2         JSR     CONTINUE        ;Prompt User for Y/N
                JSR     CROUT           ;Send CR/LF to console
;
                LDA     #$03            ;Get " addr:" msg
                JSR     HEX4            ;Display msg and get address
;
; Note: address in INDEXL/INDEXH and A/Y regs
                STA     TGTL            ;Store in Target location Low
                STY     TGTH            ;Store in Target location High
                CPY     #$80            ;Check for ROM start or higher
                BCC     NOT_ROM         ;Address is not in EEPROM
;
                JSR     SPC2            ;Send 2 spaces
                LDA     (INDEXL)        ;Get memory data
                JSR     PRBYTE          ;Send to console
                JSR     BSOUT3T         ;Backspace 3 times
                JSR     HEXIN2          ;Get new data
;
; Note: new data is in A reg
                STA     TEMP2           ;Store in Temp location
;
                JSR     BURN_BYTE       ;Update EEPROM Data
                LDA     (SRCL)          ;Get Source byte
                CMP     (TGTL)          ;Compare to Target in EEPROM
                BEQ     CMP_OK2         ;Branch if okay
                LDA     #$20            ;Else, get error msg for failed write
                BRA     PRMPTSC         ;Send to console and exit
;
NOT_ROM         LDA     #$2C            ;Get address range msg
                JSR     PROMPT          ;Send to console
                LDA     #$2D            ;Get EEPROM msg
                BRA     TRY_AGAIN       ;Branch back and try again
;
;[CNTRL-Q] Query command:
QUERY           LDA     #$4A            ;Get Query Commands Message
                BRA     PRMPTSC         ;Send to console and return
;
;[CNTL-T] TIME/DATE command: Sends a string to the console showing the current Date and Time
; Displays RTC values for Date and Time as:
;  Date: SUN Sep 15, 2020 Time: 12:34:56
;
TIME            LDA     #$31            ;Get "Date: " message
                JSR     PROMPT          ;Send to console
;
                LDA     DAY_DATE        ;Get Day and Date data
                LSR     A               ;Move Day to lower 3 bits
                LSR     A
                LSR     A
                LSR     A
                LSR     A
                CLC                     ;Clear carry
                ADC     #$31            ;Add offset to MSG #
                JSR     PROMPT          ;Send to console
                JSR     SPC             ;Send an ASCII space
;
                LDA     MONTH_CENTURY   ;Get Month and Century
                LSR     A               ;Move Month to lower 4 bits
                LSR     A
                LSR     A
                LSR     A
                CLC                     ;Clear Carry
                ADC     #$38            ;Add offset to MSG #
                JSR     PROMPT          ;Send to console
                JSR     SPC             ;Send an ASCII space
;
                LDA     DAY_DATE        ;Get Day and Date data
                AND     #%00011111      ;Mask off Day of week
                LDX     #$46            ;Get MSG for ", "
                JSR     DO8TIME         ;Convert and send to console
;
                LDA     MONTH_CENTURY   ;Get Month and Year data
                AND     #%00001111      ;Mask off Month
                TAY                     ;Xfer to Y reg
                LDA     YEAR            ;Get Year low byte
                LDX     #$47            ;Get MSG for " Time: "
                JSR     DO16TIME        ;Convert and send to console
;
                LDX     #$45            ;Get ":" message
                LDA     HOURS           ;Get Current Hours (low byte)
                JSR     DO8LZ           ;Convert and send to console
;
                LDX     #$45            ;Get ":" message
                LDA     MINS            ;Get Current Minutes (low byte)
                JSR     DO8LZ           ;Convert and send to console
;
                LDX     #$48            ;Get " " message
                LDA     SECS            ;Get Current Seconds (low byte)
                BRA     DO8LZ           ;Convert to console and return
;
DO8TIME         LDY     #$00            ;Zero high byte
DO16TIME        PHX                     ;Push message number to stack
                JSR     HEX2ASC         ;Convert and print ASCII string
                PLA                     ;Pull message number from stack
                BRA     PRMPTSC         ;Send to console and return
;
DO8LZ           PHX                     ;Push message number to stack
                JSR     HEX8ASC         ;Convert and print ASCII string with leading zero
                PLA                     ;Pull message number from stack
                BRA     PRMPTSC         ;Send to console and return
;
;[CNTRL-V] Version command:
VER             LDY     #>BIOS_MSG      ;Get high offset
                LDA     #<BIOS_MSG      ;Get low offset
                JSR     PROMPTR         ;Show BIOS version
                LDA     #$15            ;Get Intro substring (version)
PRMPTSC         JMP     PROMPT          ;Send to console and return
;
;[CNTRL-L] Xmodem/CRC Load command: receives a file from console via Xmodem protocol. no cable
; swapping needed, uses Console port and buffer via the terminal program. Not a full Xmodem/CRC
; implementation, only does CRC-16 checking, no fallback. Designed for direct attach to host
; machine via com port. Can handle full 8-bit binary transfers without errors.
; Tested with: ExtraPutty (Windows 7 Pro) and Serial (OSX).
;
;Added support for Motorola S-Record formatted files automatically. Default load address is $0800.
; An input parameter is used as a Load Address (for non-S-Record files) or as a positive offset for
; any S-Record formatted file. The supported S-Record format is S19 as created by WDC Tools Linker.
; Note: this code supports the execution address in the final S9 record, but WDC Tools does not
; provide any ability to put this into their code build. WDC are aware of this.
XMODEML         SMB7    CMDFLAG         ;Set bit7 of command flag
                STZ     OPXMDM          ;Clear Xmodem flag
                LDA     #$01            ;Set block count to one
                STA     BLKNO           ;Save it for starting block #
;
                LDA     #$23            ;Get Xmodem intro msg
                JSR     HEX4            ;Print Msg, get Hex load address/S-record Offset
                BNE     XLINE           ;Branch if data entered (Z flag set from HEX4/HEXINPUT)
                TXA                     ;Xfer to A reg (LDA #$00)
                LDY     #$08            ;Set High byte ($0800)
XLINE           STA     PTRL            ;Store to Lo pointer
                STY     PTRH            ;Store to Hi pointer
;
XMDM_LOAD ;Entry point for an external program to load data via Xmodem CRC
; To use this routine, the external program must setup the variables above which include
; the starting address (PTRL/H), clear the OPXMDM flag and set the Block count to one.
; Once completed, the message to setup the terminal program is displayed and the user
; needs to setup the terminal to send data via a filename.
;
; A 5 seconds delay is started to allow the user time to navigate to the file to be sent.
                LDA     #$25            ;Get Terminal Setup msg
                JSR     PROMPT          ;Send to console
;
;Wait for 5 seconds for user to setup xfer from terminal
                LDA     #$01            ;Set milliseconds to 1(*10 ms)
                LDX     #$01            ;Set 16-bit multiplier
                LDY     #$F4            ;to 500 decimal ($1F4)
                JSR     B_SET_DLY       ;Set Delay parameters
                JSR     B_EXE_LGDLY     ;Call long delay for 5 seconds
;
STRT_XFER       LDA     #"C"            ;Send "C" character for CRC mode
                JSR     B_CHROUT        ;Send to terminal
                LDY     #50             ;Set loop count to 50
CHR_DLY         JSR     B_EXE_MSDLY     ;Delay 1*(10ms)
                LDA     ICNT            ;Check input buffer count
                BNE     STRT_BLK        ;If a character is in, branch
                DEY                     ;Decrement loop count
                BNE     CHR_DLY         ;Branch and check again
                BRA     STRT_XFER       ;Else, branch and send another "C"
;
XM_END          RTS                     ;Cancelled by user, return
;
XDONE           LDA     #ACK            ;Last block, get ACK character
                JSR     B_CHROUT        ;Send final ACK
                LDY     #$02            ;Get delay count
                LDA     #$26            ;Get Good xfer message number
FLSH_DLY        JSR     NOLOOPS         ;Zero input buffer pointers
                PHA                     ;Save Message number
                LDA     #$19            ;Load milliseconds = 250 ms (25x10ms)
                LDX     #$00            ;Load High multiplier to 0 decimal
                JSR     B_SET_DLY       ;Set Delay parameters
                JSR     B_EXE_LGDLY     ;Execute delay, (wait to get terminal back)
                PLA                     ;Get message number back
                CMP     #$27            ;Check for error msg#
                BEQ     SHRT_EXIT       ;Do only one message
                PHA                     ;Save MSG number
                BBR7    OPXMDM,END_LOAD ;Branch if no S-Record
                LDA     #$28            ;Get S-Record load address msg
                JSR     PROMPT          ;Printer header msg
                LDA     SRCL            ;Get source Low byte
                LDY     SRCH            ;Get source High byte
                JSR     PRWORD          ;Print Hex address
END_LOAD        PLA                     ;Get Message number
SHRT_EXIT       JMP     PROMPT          ;Print Message and exit
;
STRT_BLK        JSR     B_CHRIN         ;Get a character
                CMP     #$1B            ;Is it escape - quit?
                BEQ     XM_END          ;If yes, exit
                CMP     #SOH            ;Start of header?
                BEQ     GET_BLK         ;If yes, branch and receive block
                CMP     #EOT            ;End of Transmission?
                BEQ     XDONE           ;If yes, branch and exit
                BRA     STRT_ERR        ;Else branch to error
;
GET_BLK         LDX     #$00            ;Zero index for block receive
;
GET_BLK1        JSR     B_CHRIN         ;Get a character
                STA     RBUFF,X         ;Move into buffer
                INX                     ;Increment buffer index
                CPX     #$84            ;Compare size (<01><FE><128 bytes><CRCH><CRCL>)
                BNE     GET_BLK1        ;If not done, loop back and continue
;
                LDA     RBUFF           ;Get block number from buffer
                CMP     BLKNO           ;Compare to expected block number
                BNE     RESTRT          ;If not correct, restart the block
                EOR     #$FF            ;Create one's complement of block number
                CMP     RBUFF+1         ;Compare with rcv'd value for block number
                BEQ     BLK_OKAY        ;Branch if compare is good
;
RESTRT          LDA     #NAK            ;Get NAK character
RESTRT2         JSR     B_CHROUT        ;Send to xfer program
                BRA     STRT_BLK        ;Restart block transfer
;
BLK_OKAY        LDA     #$0A            ;Set retry value to 10
                STA     CRCCNT          ;Save it to CRC retry count
;
                JSR     CRC16_GEN       ;Generate CRC16 from Buffer data
;
                LDA     RBUFF+2,Y       ;Get received CRC hi byte (4)
                CMP     CRCHI           ;Compare against calculated CRC hi byte (3)
                BNE     BADCRC          ;If bad CRC, handle error (2/3)
                LDA     RBUFF+3,Y       ;Get CRC lo byte (4)
                CMP     CRCLO           ;Compare against calculated CRC lo byte (3)
                BEQ     GOODCRC         ;If good, go move frame to memory (2/3)
;
;CRC was bad! Need to retry and receive the last frame again. Decrement the CRC retry count,
; send a NAK and try again. Count allows up to 10 retries, then cancels the transfer.
BADCRC          DEC     CRCCNT          ;Decrement retry count
                BNE     CRCRTRY         ;Retry again if count not zero
STRT_ERR        LDA     #CAN            ;Else get Cancel code
                JSR     B_CHROUT        ;Send it to terminal program
                LDY     #$08            ;Set delay multiplier
                LDA     #$27            ;Get message for receive error
                JMP     FLSH_DLY        ;Do a flush, delay and exit
CRCRTRY         JSR     NOLOOPS         ;Zero Input buffer pointers
                BRA     RESTRT          ;Send NAK and retry
;
;Block has been received, check for S19 record transfer
GOODCRC         BBS7    OPXMDM,XFER_S19 ;Branch if bit 7 set (active S-record)
                LDA     BLKNO           ;Else, check current block number
                DEC     A               ;Check for block 1 only (first time thru)
                BEQ     TEST_S19        ;If yes, test for S19 record
;
MOVE_BLK        LDX     #$00            ;Zero index offset to data
COPYBLK         LDA     RBUFF+2,X       ;Get data byte from buffer
                STA     (PTRL)          ;Store to target address
                INC     PTRL            ;Increment low address byte
                BNE     COPYBLK2        ;Check for hi byte loop
                INC     PTRH            ;Increment hi byte address
COPYBLK2        INX                     ;Point to next data byte
                BPL     COPYBLK         ;Loop back until done (128)
INCBLK          INC     BLKNO           ;Increment block number
                LDA     #ACK            ;Get ACK character
                BRA     RESTRT2         ;Send ACK and continue xfer
;
TEST_S19        LDA     RBUFF+2         ;Get first character
                CMP     #"S"            ;Check for S character
                BNE     MOVE_BLK        ;If not equal, no S-record, move block
                LDA     RBUFF+3         ;Get second character
                CMP     #"1"            ;Check for 1 character
                BNE     MOVE_BLK        ;If not equal, no S-record, move block
                SMB7    OPXMDM          ;Set bit 7 for S-record xfer
                STZ     IDY             ;Zero index for SRBUFF
;
;S-Record transfer routine: Xmodem is a 128 byte data block, S-Record is variable, up to
; 44 bytes needed to move a record at a time to the SRBUFF based on length, check as valid,
; then calculate the address and transfer to that location. Once the Xmodem buffer is empty,
; loop back to get the next block and continue processing S-Records until completed.
;
;RBUFF is the full Xmodem block, which starts with the block number, one's compliment of the
; block number, followed by the 128-bytes of data. The data is confirmed as "S1", which validates
; the start of a S-Record format.
;
;At first entry here, pointer IDY is zero. At all entries here, a 128 byte block has been received.
; The S-record type and length length needs to be calculated, then the proper count moved to the
; SRBUFF location and both pointers (IDX/IDY) are updated.
;
;S-Record format is as follows (44 bytes max):
; 2 bytes for type: "S1" or "S9" (ASCII text)
; 2 bytes for length (ASCII Hex) - includes load address, data and checksum (not CR/LF)
; 4 bytes for load address (ASCII Hex - 16-bit load address)
; 2-32 bytes for data (ASCII Hex - 1-16 bytes of data) - always an even number
; 2 bytes for checksum (ASCII Hex - 1 byte for checksum)
; 2 bytes for CR/LF
;
;First grab the 2 bytes for the length, convert to binary and transfer the correct count of
; data from RBUFF to SRBUFF. Note: increment count by two additional for CR/LF
; then update the running index into the 128 byte record (IDX) which points to the next record.
; minor update from Mike Barry, saves a byte.
XFER_S19        STZ     IDX             ;Zero offset to RBUFF
;
S19_LOOP2       LDX     IDX             ;Load current offset to RBUFF
                LDY     IDY             ;Get current offset SRBUFF
                BNE     FIL_SRBUFF      ;Branch to complete RBUFF to SRBUFF xfer
;
                LDA     RBUFF+4,X       ;Get first ASCII length character
                LDY     RBUFF+5,X       ;Get second ASCII length character
                JSR     ASC2BIN         ;Convert to binary length
                INC     A               ;Increment length for "S1" or "S9"
                INC     A               ;Increment length for "length characters"
                INC     A               ;Increment length for "CR/LF"
                ASL     A               ;Multiply by two for 2-characters per byte
                STA     TEMP3           ;Save total bytes to move to SRBUFF
                LDY     IDY             ;Get offset to SRBUFF
;
FIL_SRBUFF      LDA     RBUFF+2,X       ;Get S-Record data
                STA     SRBUFF,Y        ;Move into SREC buffer
                INX                     ;Increment index to RBUFF
                CPX     #$81            ;Check for end of buffer
                BEQ     NXT_FRAME       ;If yes, go receive another block into the buffer
                INY                     ;Increment index to SRBUFF
                CPY     TEMP3           ;Compare to length
                BNE     FIL_SRBUFF      ;Loop back until the full record is moved to SRBUFF
;
                STX     IDX             ;Update running offset to RBUFF
                STZ     IDY             ;Reset SRBUFF index pointer (for next S-record xfer)
                JSR     SREC_PROC       ;Process the S-Record in SRBUFF
                BRA     S19_LOOP2       ;Branch back and get another S-Record
;
NXT_FRAME       STY     IDY             ;Save SRBUFF offset
INCBLK2         BRA     INCBLK          ;Increment block and get next frame
;
SREC_PROC       LDA     SRBUFF+1        ;Get the Record type character
                CMP     #"1"            ;Check for S1 record
                BEQ     S1_PROC         ;Process a S1 record
                CMP     #"9"            ;Check for S9 (final) record
                BEQ     S9_PROC         ;Process a S9 record
SREC_ERR        PLA                     ;Else, pull return address
                PLA                     ;of two bytes from stack
                JMP     STRT_ERR        ;Branch to Xmodem error/exit routine
;
;This routine will decode the SRBUFF ASCII data to binary data.
; As each byte is two ASCII characters, the result is half the length.
; TEMP3 contains the overall length from above, plus extra to add in the "S1" or "S9" and CR/LF
; so we need to decrement TEMP3 by two to correct the required length.
SR_PROC         DEC     TEMP3           ;Decrement length
                DEC     TEMP3           ;Decrement length
;
SR_COMP         LDX     #$00            ;Zero Index
                LDY     #$00            ;Zero Index
SR_CMPLP        PHY                     ;Save Y reg index
                LDY     SRBUFF+3,X      ;Get LS character
                LDA     SRBUFF+2,X      ;Get MS character
                JSR     ASC2BIN         ;Convert two ASCII characters to HEX byte
                PLY                     ;Restore Y reg index
                STA     SRBUFF,Y        ;Store in SRBUFF starting at front
                INX                     ;Increment X reg twice
                INX                     ;Points to next character pair
                INY                     ;Increment Y reg once for offset to SRBUFF
                DEC     TEMP3           ;Decrement character count
                BNE     SR_CMPLP        ;Branch back until done
;
;SRBUFF now has the decoded HEX data, which is:
; 1 byte for length, 2 bytes for the load address, up to 16 bytes for data and 1 byte checksum
; Now calculate the checksum and ensure valid S-Record content
                STZ     CRCLO           ;Zero Checksum location
                LDX     SRBUFF          ;Load index with record length
                LDY     #$00            ;Zero index
SR_CHKSM        CLC                     ;Clear carry for add
                LDA     SRBUFF,Y        ;Get S-Record byte
                ADC     CRCLO           ;Add in checksum Temp
                STA     CRCLO           ;Update checksum Temp
                INY                     ;Increment offset
                DEX                     ;Decrement count
                BNE     SR_CHKSM        ;Branch back until done
;
                LDA     #$FF            ;Get all bits on
                EOR     CRCLO           ;Exclusive OR TEMP for one's complement
                CMP     SRBUFF,Y        ;Compare to last byte (which is checksum)
                BNE     SREC_ERR        ;If bad, exit out
                RTS                     ;Return to caller
;
S9_PROC         JSR     SR_PROC         ;Process the S-Record and checksum
                LDA     SRBUFF+1        ;Get MSB load address
                STA     COMHI           ;Store to execution pointer
                LDA     SRBUFF+2        ;Get LSB load address
                STA     COMLO           ;Store to execution pointer
                PLA                     ;Pull return address
                PLA                     ;second byte
                BRA     INCBLK2         ;Branch back to close out transfer
;
S1_PROC         JSR     SR_PROC         ;Process the S-Record and checksum
;
;Valid binary S-Record decoded at SRBUFF. Calculate offset from input, add to specified load
; address and store into memory, then loop back until done. Offset is stored in PTR L/H from
; initial input. If no input entered, BUFIDX is zero and PTR L/H is preset to $0800, so checking
; for BUFIDX being zero bypasses adding the offset, if BUFIDX is non zero, then PTR L/H contains
; the offset address which is added to TGT L/H moving the S-Record data to memory.
                LDA     SRBUFF+1        ;Get MS load address
                STA     TGTH            ;Store to target pointer
                LDA     SRBUFF+2        ;Get LS load address
                STA     TGTL            ;Store to target pointer
                LDA     BUFIDX          ;Check input count for offset required
                BEQ     NO_OFFSET       ;If Zero, no offset was entered
;
;Add in offset contained at PTR L/H to TGT L/H
                CLC                     ;Clear carry for add
                LDA     PTRL            ;Get LS offset
                ADC     TGTL            ;Add to TGTL address
                BCC     SKIP_HB         ;Skip increment HB if no carry
                INC     TGTH            ;Else increment TGTH by one
SKIP_HB         STA     TGTL            ;Save TGTL
                LDA     PTRH            ;Get MS offset
                ADC     TGTH            ;Add to TGTH
                STA     TGTH            ;Save it
;
;Check for first Block and load SRC H/L with load address
NO_OFFSET       LDA     BLKNO           ;Get Block number
                DEC     A               ;Decrement to test for block one
                BNE     NO_OFFST2       ;If not first block, skip around
                LDA     IDX             ;Get running count for first block
                CMP     #$2C            ;First S-record?
                BNE     NO_OFFST2       ;If yes, setup load address pointer
                LDA     TGTL            ;Get starting address Lo byte
                STA     SRCL            ;Save it as Source Lo byte
                LDA     TGTH            ;Get starting address Hi byte
                STA     SRCH            ;Save it as Source Hi byte
;
NO_OFFST2       LDX     SRBUFF          ;Get record length
                DEX                     ;Decrement by 3
                DEX                     ;to only transfer the data
                DEX                     ;and not the count/load address
                LDY     #$00            ;Zero index
MVE_SREC        LDA     SRBUFF+3,Y      ;Get offset to data in record
                STA     (TGTL),Y        ;Store it to memory
                INY                     ;Increment index
                DEX                     ;Decrement record count
                BNE     MVE_SREC        ;Branch back until done
XMDMQ           RTS                     ;Return to caller
;
;[CNTRL-S] Xmodem/CRC Save command: Sends data to the Terminal program via Xmodem CRC.
; No cable swapping is required, uses the console UART for all transfers. User must
; enter a starting address for the data and a length, both 16-bit. Once entered, the
; user will need to setup the terminal program to receive data via Xmodem CRC protocol.
;
; NOTE: Tested with ExtraPutty (Win7) and Serial (OSX). Serial (OSX) has a problem with EOT.
; Serial does not release the port after completed, so the last message does not show!
; I submitted a problem report with their support, they have asked for more detail which I
; provided. Further note: Zoc7 also works properly on OSX, so the problem is with Serial.
;
; 20/06/2019 Serial Update!
;
;A workaround has been implemented for the problem with Serial:
; by implementing a 15 seconds delay to allow the user to name the saved file, Serial will
; release the port nicely and the Monitor code can display the exit message and return with
; the Monitor prompt as normal. This is basically an anomaly with Serial and I have sent this
; in to them. So far, no response from them, which is disappointing ;-(
;
; 10/09/2019 Serial Update!
;
; Version 1.4.0 has been released. The problem noted here has been fixed, so all workarounds
; have been removed and the code functions as designed without issues.
;
XMODEMS         STZ     OPXMDM          ;Clear Xmodem flag
                LDA     #$01            ;Set block count to one
                STA     BLKNO           ;Save it
;
                LDA     #$24            ;Get Xmodem Save intro msg
                JSR     HEX4            ;Send msg and get Source address
                STA     SRCL            ;Save Source low byte
                STY     SRCH            ;Save Source high byte
                LDA     #$04            ;Get " len:" msg
                JSR     HEX4            ;Send msg and get length to send
                STA     LENL            ;Save Length low byte
                STY     LENH            ;Save length high byte
;
XMDM_SAVE       ;Entry point for an external program to save data via Xmodem CRC
; Entry at this point assumes that the variables have been setup properly.
; This includes the Source address (SCRL/H), Length (LENL/H), and that the
; Block count has been set to one and the OPXMDM flag has been cleared.
                LDA     #$25            ;Get Terminal Setup message
                JSR     PROMPT          ;Send to console
;
; User now needs to setup terminal program to receive data via Xmodem/CRC.
;  Once setup, the terminal program will send an ASCII "C" character to signal the start
;  of data transfer. We can prompt the user to terminate the transfer by hitting ESC.
;  Once the transfer starts however, it is basically controlled by the receiving end.
                JSR     B_CHRIN         ;Wait for a key from the terminal program
                CMP     #$1B            ;Check for ESC key
                BEQ     XMDMQ           ;If yes, quit Xmodem save function
                CMP     #'C'            ;Check for upper case ASCII "C"
                BNE     XMDMQ           ;If not, quit Xmodem save function
;
; Terminal has sent an ASCII "C" character to initiate Xmodem/CRC transfer.
;  Note that per protocol, if receiver does not receive data within 3 seconds,
;  it will send another "C" to the device. This dictates the maximum time to
;  assemble the 133-byte block with 16-bit checksum must be less than 3 seconds.
SND_LP          LDA     BLKNO           ;Get current block number
                STA     RBUFF           ;Store into buffer
                EOR     #$FF            ;One's compliment of BLKNO
                STA     RBUFF+1         ;Store into buffer
;
; Now assemble 128 byte buffer and 16-bit CRC to send to terminal
;  SRCL/H is the start of the data to be sent. Start by moving 128 bytes into RBUFF
                LDX     #$00            ;Zero index count
XMD_FIL         LDA     LENL            ;Get length low byte
                ORA     LENH            ;Or in high byte
                BEQ     XM_BLK          ;If length is zero, finish last block
                LDA     (SRCL)          ;Get a byte from the source
                STA     RBUFF+2,X       ;Save it into the buffer
                JSR     UPD_STL         ;Update source and length variables
                INX                     ;Increment count
                CPX     #$80            ;Compare to 128
                BNE     XMD_FIL         ;Loop back until buffer is filled
                BRA     DO_UP_CRC       ;Calculate CRC and send block
;
;As each Xmodem block must contain 128 bytes, if the total amount data to be sent is less than
; 128 bytes, then the block must be padded to fill up the remaining space. By protocol
; definition, the default fill byte is hex $1E. The routine above checks the length during
; each byte transfer into the block buffer and will arrive here upon the length going to zero.
; Having arrived here with no more data to put into a block, we must determine if it's the
; last block and ensure that the block is a full 128 bytes with the proper $1E padding
; character for any unused data space.
XM_BLK          CPX     #$80            ;Check for X reg at 128 bytes
                BEQ     LAST_BLK        ;If yes, complete last block and exit
LST_BKLP        LDA     #$1A            ;Get padding character
                STA     RBUFF+2,X       ;Place into buffer
                INX                     ;Increment count
                BRA     XM_BLK          ;Loop back until done
LAST_BLK        SMB1    OPXMDM          ;Set bit in flag for last block
;
; Now calculate the 16-bit CRC value and add into buffer
DO_UP_CRC       JSR     CRC16_GEN       ;Generate CRC for block
;
                LDA     CRCHI           ;Get calculated CRC hi byte
                STA     RBUFF+2,Y       ;Save into buffer
                LDA     CRCLO           ;Get calculated CRC lo byte
                STA     RBUFF+3,Y       ;Save into buffer
;
; Buffer now has Block number/block compliment, 128 bytes of data and a 16-bit CRC
RE_SEND         LDA     #SOH            ;Get start of header code
                JSR     B_CHROUT        ;Send to terminal
;
                LDX     #$00            ;Zero index count
BLK_SND         LDA     RBUFF,X         ;Get Buffer data
                JSR     B_CHROUT        ;Send to terminal
                INX                     ;Increment count
                CPX     #$84            ;Check for all bytes sent
                BNE     BLK_SND         ;Branch back till buffer sent
;
; Buffer sent, now check for either an ACK or NAK from the terminal to confirm receive
CHK_LAST        JSR     B_CHRIN         ;Get a character back from the terminal
                CMP     #ACK            ;Check for good receive
                BNE     BAD_RCV         ;Branch if error
                BBS1    OPXMDM,XMUPDON  ;If last block sent, exit
                LDA     LENL            ;Check for zero length left
                ORA     LENH            ; - Even block size will be zero
                BEQ     ZERO_BLK        ;If zero, just set bit and finish up
                INC     BLKNO           ;Increment block count to next
                BRA     SND_LP          ;Branch back to send next block
;
; ACK not received from terminal, check for NAK
BAD_RCV         CMP     #NAK            ;Check for NAK
                BEQ     RE_SEND         ;Resend block
;
; Unknown error at the this point... ACK or NAK not received.
;  Cancel the transfer with two CAN characters and return to Monitor
                LDA     #CAN            ;Get cancel code
                JSR     B_CHROUT        ;Send to terminal (A reg preserved)
                JSR     B_CHROUT        ;Send to terminal again
                BRA     XMDM_ERR        ;Finish up with error message and exit
;
ZERO_BLK        SMB1    OPXMDM          ;Set bit for last block
;
;Transfer is now completed. Last block was received properly. So we should
; send a EOT character and return to the Monitor.
XMUPDON         LDA     #EOT            ;Get end of transmission character
                JSR     B_CHROUT        ;Sent to terminal
;
                JSR     B_CHRIN         ;Get a character back from terminal
                CMP     #ACK            ;Check for completion of transfer
                BNE     XMDM_ERR        ;Branch if an error occurred
;
                LDA     #$26            ;Else, get complete msg
XMDM_NE         JMP     PROMPT          ;Send to console and return
;
XMDM_ERR        LDA     #$27            ;Get xfer fail msg
                BRA     XMDM_NE         ;Send message and return to Monitor
;
;CRC-16 Generation program. This routine generates the 16-bit CRC for the 128 byte
;  data block stored in RBUFF. It is a separate routine as it's used in both the
;  Xmodem load and save routines. It saves 31 bytes with a small penalty in speed.
CRC16_GEN       STZ     CRCLO           ;Reset the CRC value by
                STZ     CRCHI           ;putting all bits off
                LDY     #$00            ;Set index for data offset
CALCCRC         LDA     RBUFF+2,Y       ;Get data byte
                PHP                     ;Save status reg
                LDX     #$08            ;Load index for 8 bits
                EOR     CRCHI           ;XOR High CRC byte
CRCLOOP         ASL     CRCLO           ;Shift carry to CRC low byte
                ROL     A               ;Shift bit to carry flag
                BCC     CRCLP1          ;Branch if MSB is 1
                EOR     #$10            ;Exclusive OR with polynomial
                PHA                     ;Save result on stack
                LDA     CRCLO           ;Get CRC low byte
                EOR     #$21            ;Exclusive OR with polynomial
                STA     CRCLO           ;Save it back
                PLA                     ;Get previous result
CRCLP1          DEX                     ;Decrement index
                BNE     CRCLOOP         ;Loop back for all 8 bits
                STA     CRCHI           ;Update CRC high byte
                PLP                     ;Restore status reg
                INY                     ;Increment index to the next data byte
                BPL     CALCCRC         ;Branch back until all 128 fed to CRC routine
                RTS                     ;Return to caller
;
;[CNTL-R] Reset System command: Resets system by calling Coldstart routine. Page zero is
; cleared, vectors and config data re-initialized from ROM. All I/O devices are reset from
; initial ROM parameters. Monitor cold start is entered.
;
; Using extended baud rates in the C02BIOS now requires an additional check for jumping to the
; COLDSTRT routine in BIOS. An odd number of accesses to the BRG register effectively toggles
; the BRG Test register, so checking for extended baud rates allows for an additional access
; to ensure the total number of accesses is "even", not "odd".
; Bits 4,5 of the MATCH flag byte are used for current status of the BRG Test register.
; The Reset/Zero RAM routines now checks for the current mode and will either toggle the
; BRG Test Register or not, to ensure it's inactive before calling the COLDSTRT routine.
;
SYS_RST         LDA     #$1B            ;Get msg "Reset System"
                SMB0    CMDFLAG         ;Set bit0 of command flag
                BRA     RST_ONLY        ;Branch below and handle reset
;
;[CNTL-Z] Zero command: zero RAM from $0100-$7FFF and Reset
ZERO            LDA     #$1A            ;Get msg "Zero RAM/Reset System"
RST_ONLY        JSR     PROMPT          ;Send to terminal
                JSR     CONTINUE        ;Prompt for Continue
                BBS0    CMDFLAG,CHK_EBR ;Branch if reset only
                SEI                     ;Disable IRQs
                LDA     #$01            ;Initialize address pointer to $0100
                STA     $01             ;Store to pointer high byte
                STZ     $00             ;Zero address low byte
                DEC     A               ;LDA #$00
ZEROLOOP        STA     ($00)           ;Write $00 to current address
                INC     $00             ;Increment address pointer
                BNE     ZEROLOOP        ;Loop back until done
                INC     $01             ;Increment page
                BPL     ZEROLOOP        ;Loop back IF address pointer < $8000
CHK_EBR         BBR4    MATCH,DO_COLD   ;Check if BRG toggled, branch if no EBR in use
                BIT     UART_BRGTST     ;Else, toggle BRG Test register to normal
DO_COLD         JMP     B_COLDSTRT      ;Jump to coldstart vector
;
;END OF MONITOR CODE
;**************************************************************************************************
;                               START OF MONITOR DATA                                             *
;**************************************************************************************************
;Monitor command & jump table
; There are two parts to the Monitor command and jump table; First is the list of commands, which
; are one byte each. Alpha command characters are upper case. Second is the 16-bit address table
; that corresponds to the command routines for each command character.
MONCMD  .DB     $01             ;[CNTRL-A] Start Assembler
        .DB     $02             ;[CNTRL-B] Boot DOS/65 ROM Version
        .DB     $04             ;[CNTRL-D] Start Disassembler
        .DB     $05             ;[CNTRL-E] Edit EEPROM byte location
        .DB     $0C             ;[CNTRL-L] Xmodem/CRC Load
        .DB     $10             ;[CNTRL-P] Program EEPROM
        .DB     $11             ;[CNTRL-Q] Query Monitor Commands
        .DB     $12             ;[CNTRL-R] Reset - same as power up
        .DB     $13             ;[CNTRL-S] Xmodem/CRC Save
        .DB     $14             ;[CNTRL-T] Time/Date display
        .DB     $16             ;[CNTRL-V] Display Monitor Version
        .DB     $1A             ;[CNTRL-Z] Zero Memory - calls reset
        .DB     $28             ;(         Init Macro
        .DB     $29             ;)         Run Macro
        .DB     $2C             ;,         Setup Delay parameters
        .DB     $2E             ;.         Execute Millisecond Delay
        .DB     $2F             ;/         Execute Long Delay
        .DB     $5C             ;\         Load and Go Extra Long Delay
        .DB     $41             ;A         Display/Edit A register
        .DB     $42             ;B         Benchmark Timer clear/start
        .DB     $43             ;C         Compare memory block
        .DB     $44             ;D         Display Memory contents in HEX/TEXT
        .DB     $45             ;E         Examine/Edit memory
        .DB     $46             ;F         Fill memory block
        .DB     $47             ;G         Go execute to <addr>
        .DB     $48             ;H         Hex byte string search
        .DB     $49             ;I         Input Text string
        .DB     $4D             ;M         Move memory block
        .DB     $50             ;P         Display/Edit CPU status reg
        .DB     $51             ;Q         Quit Benchmark timer
        .DB     $52             ;R         Display Registers
        .DB     $53             ;S         Display/Edit stack pointer
        .DB     $54             ;T         Text character string search
        .DB     $58             ;X         Display/Edit X register
        .DB     $59             ;Y         Display/Edit Y register
;
MONTAB  .DW     ASSEMBLER       ;[CNTRL-A] $01 Start Assembler
        .DW     EHBASIC         ;[CNTRL-B] $02 Boot DOS/65 Rom Version
        .DW     DISASSEMBLER    ;[CNTRL-D] $04 Start Disassembler
        .DW     EEDIT           ;[CNTRL-E] $05 Edit EEPROM byte location
        .DW     XMODEML         ;[CNTRL-L] $0C Xmodem Download. Uses Console Port
        .DW     PROGEE          ;[CNTRL-P] $10 Program the EEPROM
        .DW     QUERY           ;[CNTRL-Q] $11 Query Monitor Commands
        .DW     SYS_RST         ;[CNTRL-R] $12 Reset CO2Monitor
        .DW     XMODEMS         ;[CNTRL-S] $13 Xmodem Upload. Uses Console Port
        .DW     TIME            ;[CNTRL-T] $14 System Date and Time
        .DW     VER             ;[CNTRL-V] $16 Display Monitor Version level
        .DW     ZERO            ;[CNTRL-Z] $1A Zero memory ($0100-$7FFF) then Reset
        .DW     INIMACRO        ;(         $28 Clear input buffer/reset pointers
        .DW     RUNMACRO        ;)         $29 Run Macro from start of input buffer
        .DW     SET_DELAY       ;.         $2C Setup Delay Parameters
        .DW     B_EXE_MSDLY     ;,         $2E Perform Millisecond Delay
        .DW     B_EXE_LGDLY     ;/         $2F Execute Long Delay
        .DW     SET_XLDLY       ;\         $5C Load and execute Extra Long Delay
        .DW     ARG             ;A         $41 Examine/Edit ACCUMULATOR preset/result
        .DW     BENCH           ;B         $42 Clear count and start Benchmark timer
        .DW     CPMVFL          ;C         $43 Compare command - new
        .DW     MDUMP           ;D         $44 HEX/TEXT dump from specified memory address
        .DW     CHANGE          ;E         $45 Examine/change a memory location's contents
        .DW     CPMVFL          ;F         $46 Fill specified memory range with a value
        .DW     GO              ;G         $47 Execute program code at specified address
        .DW     SRCHBYT         ;H         $48 Search memory for a specified byte string
        .DW     TEXT            ;I         $49 Input text string into memory
        .DW     CPMVFL          ;M         $4D Copy memory from Source to Target space
        .DW     PRG             ;P         $50 Examine/Edit CPU STATUS REGISTER preset/result
        .DW     QUITB           ;Q         $51 Quit Benchmark timer and display count
        .DW     PRSTAT          ;R         $52 Display all preset/result contents
        .DW     SRG             ;S         $53 Examine/Edit STACK POINTER preset/result
        .DW     SRCHTXT         ;T         $54 Search memory for a specified text string
        .DW     XRG             ;X         $58 Examine/Edit X-REGISTER preset/result
        .DW     YRG             ;Y         $59 Examine/Edit Y-REGISTER preset/result
;
;**************************************************************************************************
;       C02Monitor message strings used with PROMPT routine, terminated with $00                  *
;**************************************************************************************************
MSG_00  .DB     " cont?"
MSG_01  .DB     "(y/n)"
        .DB     $00
MSG_02  .DB     $0D,$0A
        .DB     "   "
MSG_03  .DB     " addr:"
        .DB     $00
MSG_04  .DB     " len:"
        .DB     $00
MSG_05  .DB     " val:"
        .DB     $00
MSG_06  .DB     " src:"
        .DB     $00
MSG_07  .DB     " tgt:"
        .DB     $00
MSG_08  .DB     " find txt:"
        .DB     $00
MSG_09  .DB     " find bin:"
        .DB     $00
MSG_0A  .DB     "not "
MSG_0B  .DB     "found"
        .DB     $00
MSG_0C  .DB     $0D,$0A
        .DB     "search- "
        .DB     $00
MSG_0D  .DB     $0D,$0A
        .DB     "(n)ext? "
        .DB     $00
MSG_0E  .DB     "SR:$"
        .DB     $00
MSG_0F  .DB     "SP:$"
        .DB     $00
MSG_10  .DB     "YR:$"
        .DB     $00
MSG_11  .DB     "XR:$"
        .DB     $00
MSG_12  .DB     "AC:$"
        .DB     $00
MSG_13  .DB     $0D,$0A
        .DB      "   PC  AC XR YR SP NV-BDIZC",$0D,$0A
        .DB     "; "
        .DB     $00
MSG_14  .DB     $0D,$0A
        .DB     "C02Monitor (c)2013-2021 K.E.Maier",$07
        .DB     $0D,$0A
        .DB     "CTRL-Q for command list"
MSG_15  .DB     $0D,$0A
        .DB     "Version 3.04"
        .DB     $0D,$0A
        .DB     "14/05/2021"
        .DB     $0D,$0A
        .DB     $00
MSG_16  .DB     $0D,$0A
        .DB     ";-"
        .DB     $00
MSG_17  .DB     " delay ms:"
        .DB     $00
MSG_18  .DB     " mult:"
        .DB     $00
MSG_19  .DB     " delay xl:"
        .DB     $00
MSG_1A  .DB     "Zero RAM/"
MSG_1B  .DB     "Reset,"
        .DB     $00
MSG_1C  .DB     "Program EEPROM",$0D,$0A
        .DB     $00
MSG_1D  .DB     $0D,$0A
        .DB     "Are you sure? "
        .DB     $00
MSG_1E  .DB     $0D,$0A
        .DB     "Writing EEPROM..."
        .DB     $00
MSG_1F  .DB     $0D,$0A
        .DB     "EEPROM write complete."
        .DB     $00
MSG_20  .DB     $0D,$0A
        .DB     "EEPROM write failed!",$0D,$0A
        .DB     "Check R/W jumper."
        .DB     $00
MSG_21  .DB     $0D,$0A
        .DB     "Show Loop count "
        .DB     $00
MSG_22  .DB     $0D,$0A
        .DB     "Loops: "
        .DB     $00
MSG_23  .DB     "Xmodem Download, <ESC> to abort, or"
        .DB     $0D,$0A
        .DB     "Load Address/S-Record offset:"
        .DB     $00
MSG_24  .DB     "Xmodem Upload, <ESC> to abort, or"
        .DB     $0D,$0A
        .DB     "Enter Start address:"
        .DB     $00
MSG_25  .DB     $0D,$0A
        .DB     "Setup Terminal program for Data transfer."
        .DB     $0D,$0A
        .DB     $00
MSG_26  .DB     $0D,$0A
        .DB     "Data transfer complete."
        .DB     $00
MSG_27  .DB     $0D,$0A
        .DB     "Data transfer error!"
        .DB     $00
MSG_28  .DB     $0D,$0A
        .DB     "S-Record load at:$"
        .DB     $00
MSG_29  .DB     $0D,$0A
        .DB     "Disassembly from"
        .DB     $00
MSG_2A  .DB     "Start Benchmark counter? "
        .DB     $00
MSG_2B  .DB     "Caution! Editing EEPROM data."
        .DB     $00
MSG_2C  .DB     $0D,$0A
        .DB     "Address not in "
        .DB     $00
MSG_2D  .DB     "EEPROM!"
        .DB     $0D,$0A
        .DB     $00
MSG_2E  .DB     "RAM!"
        .DB     $0D,$0A
        .DB     $00
MSG_2F  .DB     $0D,$0A
        .DB     "Assembly from"
        .DB     $00
MSG_30  .DB     $0D,$0A
        .DB     "branch out of range",$0D, $0A
        .DB     $00
;
MSG_31  .DB     "Date: "
        .DB     $00
MSG_32  .DB     "Sat"
        .DB     $00
MSG_33  .DB     "Sun"
        .DB     $00
MSG_34  .DB     "Mon"
        .DB     $00
MSG_35  .DB     "Tue"
        .DB     $00
MSG_36  .DB     "Wed"
        .DB     $00
MSG_37  .DB     "Thu"
        .DB     $00
MSG_38  .DB     "Fri"
        .DB     $00
MSG_39  .DB     "Jan"
        .DB     $00
MSG_3A  .DB     "Feb"
        .DB     $00
MSG_3B  .DB     "Mar"
        .DB     $00
MSG_3C  .DB     "Apr"
        .DB     $00
MSG_3D  .DB     "May"
        .DB     $00
MSG_3E  .DB     "Jun"
        .DB     $00
MSG_3F  .DB     "Jul"
        .DB     $00
MSG_40  .DB     "Aug"
        .DB     $00
MSG_41  .DB     "Sep"
        .DB     $00
MSG_42  .DB     "Oct"
        .DB     $00
MSG_43  .DB     "Nov"
        .DB     $00
MSG_44  .DB     "Dec"
        .DB     $00
MSG_45  .DB     ":"
        .DB     $00
MSG_46  .DB     ", "
        .DB     $00
MSG_47  .DB     " Time: "
        .DB     $00
MSG_48  .DB     " "
        .DB     $00
MSG_49  .DB     " Seconds"
        .DB     $00
MSG_4A  .DB     $0D,$0A
        .DB     "Memory Ops: "
        .DB     "[C]ompare, "
        .DB     "[D]isplay, "
        .DB     "[E]dit, "
        .DB     "[F]ill, "
        .DB     "[G]o Exec,",$0D,$0A
        .DB     "[H]ex Find, "
        .DB     "[I]nput Text, "
        .DB     "[M]ove, "
        .DB     "[T]ext Find",$0D,$0A,$0A
        .DB     "Register Ops: "
        .DB     "R,A,X,Y,S,P",$0D,$0A,$0A
        .DB     "Counter/Timer Ops: "
        .DB     ",= set ms|mult, "
        .DB     ".= exe ms, "
        .DB     "/= exe ms*mult, "
        .DB     "\= exe (?)*ms*mult",$0D,$0A
        .DB     "[B]enchmark clear/start, "
        .DB     "[Q]uit benchmark/display elapsed time",$0D,$0A,$0A
        .DB     "Macro: "
        .DB     "(= Init "
        .DB     ")= Run",$0D,$0A,$0A
        .DB     "CTRL[?]: "
        .DB     "[A]ssemble, "
        .DB     "[B]oot DOS/65, "
        .DB     "[D]isassemble, "
        .DB     "[E]dit EEPROM, "
        .DB     "[L]oad",$0D,$0A
        .DB     "[P]rogram, "
        .DB     "[Q]uery Cmds ,"
        .DB     "[R]eset, "
        .DB     "[S]ave, "
        .DB     "[T]ime/Date, "
        .DB     "[V]ersion",$0D,$0A
        .DB     "[Z]ero RAM/Reset",$0A
        .DB     $00
;
MSG_TABLE       ;Message table: contains message addresses sent via the PROMPT routine
        .DW     MSG_00, MSG_01, MSG_02, MSG_03, MSG_04, MSG_05, MSG_06, MSG_07
        .DW     MSG_08, MSG_09, MSG_0A, MSG_0B, MSG_0C, MSG_0D, MSG_0E, MSG_0F
        .DW     MSG_10, MSG_11, MSG_12, MSG_13, MSG_14, MSG_15, MSG_16, MSG_17
        .DW     MSG_18, MSG_19, MSG_1A, MSG_1B, MSG_1C, MSG_1D, MSG_1E, MSG_1F
        .DW     MSG_20, MSG_21, MSG_22, MSG_23, MSG_24, MSG_25, MSG_26, MSG_27
        .DW     MSG_28, MSG_29, MSG_2A, MSG_2B, MSG_2C, MSG_2D, MSG_2E, MSG_2F
        .DW     MSG_30, MSG_31, MSG_32, MSG_33, MSG_34, MSG_35, MSG_36, MSG_37
        .DW     MSG_38, MSG_39, MSG_3A, MSG_3B, MSG_3C, MSG_3D, MSG_3E, MSG_3F
        .DW     MSG_40, MSG_41, MSG_42, MSG_43, MSG_44, MSG_45, MSG_46, MSG_47
        .DW     MSG_48, MSG_49, MSG_4A
;
;**************************************************************************************************
;                               START OF DISASSEMBLER DATA                                        *
;**************************************************************************************************
; Pointer for address mode handlers. Each byte contains handler pointer for two opcodes;
; Upper nibble for odd, lower nibble for even
HDLR_IDX
        .DB     $26,$00,$33,$3E,$02,$10,$88,$8F
        .DB     $C7,$B0,$34,$4E,$0A,$10,$89,$9F
        .DB     $86,$00,$33,$3E,$02,$10,$88,$8F
        .DB     $C7,$B0,$44,$4E,$0A,$10,$99,$9F
        .DB     $06,$00,$03,$3E,$02,$10,$88,$8F
        .DB     $C7,$B0,$04,$4E,$0A,$00,$09,$9F
        .DB     $06,$00,$33,$3E,$02,$10,$B8,$8F
        .DB     $C7,$B0,$44,$4E,$0A,$00,$D9,$9F
        .DB     $C6,$00,$33,$3E,$02,$00,$88,$8F
        .DB     $C7,$B0,$44,$5E,$0A,$00,$89,$9F
        .DB     $26,$20,$33,$3E,$02,$00,$88,$8F
        .DB     $C7,$B0,$44,$5E,$0A,$00,$99,$AF
        .DB     $26,$00,$33,$3E,$02,$00,$88,$8F
        .DB     $C7,$B0,$04,$4E,$0A,$00,$09,$9F
        .DB     $26,$00,$33,$3E,$02,$00,$88,$8F
        .DB     $C7,$B0,$04,$4E,$0A,$00,$09,$9F
;
;Disassembler handler table: Handler address index: (referenced in table HDLR_IDX)
HDLR_TAB
        .DW     IMPLIED         ;$00
        .DW     ACC_MODE        ;$01
        .DW     ZP_IMED         ;$02
        .DW     ZP_ABS          ;$03
        .DW     ZP_ABS_X        ;$04
        .DW     ZP_ABS_Y        ;$05
        .DW     ZP_IND_X        ;$06
        .DW     ZP_IND_Y        ;$07
        .DW     ABSOLUTE        ;$08
        .DW     ABS_X           ;$09
        .DW     ABS_Y           ;$0A
        .DW     INDIRECT        ;$0B
        .DW     REL_BRA         ;$0C
        .DW     IND_ABS_X       ;$0D
        .DW     ZP_XMB          ;$0E
        .DW     ZP_BBX          ;$0F
;
;Disassembler mnemonic pointer table. This is indexed by the instruction opcode.
; The values in this table are an index to the mnemonic data used to print:
MNE_PTAB        ;Mnemonic pointer index table
        .DB     $1C,$4C,$00,$00,$82,$4C,$06,$5E,$50,$4C,$06,$00,$82,$4C,$06,$08
        .DB     $18,$4C,$4C,$00,$80,$4C,$06,$5E,$22,$4C,$38,$00,$80,$4C,$06,$08
        .DB     $40,$04,$00,$00,$12,$04,$60,$5E,$58,$04,$60,$00,$12,$04,$60,$08
        .DB     $14,$04,$04,$00,$12,$04,$60,$5E,$6A,$04,$30,$00,$12,$04,$60,$08
        .DB     $64,$36,$00,$00,$00,$36,$48,$5E,$4E,$36,$48,$00,$3E,$36,$48,$08
        .DB     $1E,$36,$36,$00,$00,$36,$48,$5E,$26,$36,$54,$00,$00,$36,$48,$08
        .DB     $66,$02,$00,$00,$7A,$02,$62,$5E,$56,$02,$62,$00,$3E,$02,$62,$08
        .DB     $20,$02,$02,$00,$7A,$02,$62,$5E,$6E,$02,$5C,$00,$3E,$02,$62,$08
        .DB     $1A,$72,$00,$00,$78,$72,$76,$70,$34,$12,$86,$00,$78,$72,$76,$0A
        .DB     $0C,$72,$72,$00,$78,$72,$76,$70,$8A,$72,$88,$00,$7A,$72,$7A,$0A
        .DB     $46,$42,$44,$00,$46,$42,$44,$70,$7E,$42,$7C,$00,$46,$42,$44,$0A
        .DB     $0E,$42,$42,$00,$46,$42,$44,$70,$28,$42,$84,$00,$46,$42,$44,$0A
        .DB     $2E,$2A,$00,$00,$2E,$2A,$30,$70,$3C,$2A,$32,$8C,$2E,$2A,$30,$0A
        .DB     $16,$2A,$2A,$00,$00,$2A,$30,$70,$24,$2A,$52,$74,$00,$2A,$30,$0A
        .DB     $2C,$68,$00,$00,$2C,$68,$38,$70,$3A,$68,$4A,$00,$2C,$68,$38,$0A
        .DB     $10,$68,$68,$00,$00,$68,$38,$70,$6C,$68,$5A,$00,$00,$68,$38,$0A
;
DIS_NMEM        ;Mnemonic compressed table
; Uses two bytes per 3-character Mnemonic. 5-bits per character uses 15-bit total
; Characters are left to right. 5-bits shifted into A reg, add in $3F and print
; "?" starts with "00000", "A" starts with "00010", "B" starts with "00011", etc.
;
; A-00010 B-00011 C-00100 D-00101 E-00110 F-00111 G-01000 H-01001 I-01010
; J-01011 K-01100 L-01101 M-01110 N-01111 O-10000 P-10001 Q-10010 R-10011
; S-10100 T-10101 U-10110 V-10111 W-11000 X-11001 Y-11010 Z-11011
        .DBYTE  %0000000000000000       ;???    $00
        .DBYTE  %0001000101001000       ;ADC    $02
        .DBYTE  %0001001111001010       ;AND    $04
        .DBYTE  %0001010100011010       ;ASL    $06
        .DBYTE  %0001100011100110       ;BBR    $08
        .DBYTE  %0001100011101000       ;BBS    $0A
        .DBYTE  %0001100100001000       ;BCC    $0C
        .DBYTE  %0001100100101000       ;BCS    $0E
        .DBYTE  %0001100110100100       ;BEQ    $10
        .DBYTE  %0001101010101010       ;BIT    $12
        .DBYTE  %0001101110010100       ;BMI    $14
        .DBYTE  %0001101111001100       ;BNE    $16
        .DBYTE  %0001110001011010       ;BPL    $18
        .DBYTE  %0001110011000100       ;BRA    $1A
        .DBYTE  %0001110011011000       ;BRK    $1C
        .DBYTE  %0001110111001000       ;BVC    $1E
        .DBYTE  %0001110111101000       ;BVS    $20
        .DBYTE  %0010001101001000       ;CLC    $22
        .DBYTE  %0010001101001010       ;CLD    $24
        .DBYTE  %0010001101010100       ;CLI    $26
        .DBYTE  %0010001101101110       ;CLV    $28
        .DBYTE  %0010001110100010       ;CMP    $2A
        .DBYTE  %0010010001110010       ;CPX    $2C
        .DBYTE  %0010010001110100       ;CPY    $2E
        .DBYTE  %0010100110001000       ;DEC    $30
        .DBYTE  %0010100110110010       ;DEX    $32
        .DBYTE  %0010100110110100       ;DEY    $34
        .DBYTE  %0011010000100110       ;EOR    $36
        .DBYTE  %0101001111001000       ;INC    $38
        .DBYTE  %0101001111110010       ;INX    $3A
        .DBYTE  %0101001111110100       ;INY    $3C
        .DBYTE  %0101101110100010       ;JMP    $3E
        .DBYTE  %0101110100100110       ;JSR    $40
        .DBYTE  %0110100101000100       ;LDA    $42
        .DBYTE  %0110100101110010       ;LDX    $44
        .DBYTE  %0110100101110100       ;LDY    $46
        .DBYTE  %0110110100100110       ;LSR    $48
        .DBYTE  %0111110000100010       ;NOP    $4A
        .DBYTE  %1000010011000100       ;ORA    $4C
        .DBYTE  %1000101001000100       ;PHA    $4E
        .DBYTE  %1000101001100010       ;PHP    $50
        .DBYTE  %1000101001110010       ;PHX    $52
        .DBYTE  %1000101001110100       ;PHY    $54
        .DBYTE  %1000101101000100       ;PLA    $56
        .DBYTE  %1000101101100010       ;PLP    $58
        .DBYTE  %1000101101110010       ;PLX    $5A
        .DBYTE  %1000101101110100       ;PLY    $5C
        .DBYTE  %1001101110000110       ;RMB    $5E
        .DBYTE  %1001110000011010       ;ROL    $60
        .DBYTE  %1001110000100110       ;ROR    $62
        .DBYTE  %1001110101010100       ;RTI    $64
        .DBYTE  %1001110101101000       ;RTS    $66
        .DBYTE  %1010000011001000       ;SBC    $68
        .DBYTE  %1010000110001000       ;SEC    $6A
        .DBYTE  %1010000110001010       ;SED    $6C
        .DBYTE  %1010000110010100       ;SEI    $6E
        .DBYTE  %1010001110000110       ;SMB    $70
        .DBYTE  %1010010101000100       ;STA    $72
        .DBYTE  %1010010101100010       ;STP    $74
        .DBYTE  %1010010101110010       ;STX    $76
        .DBYTE  %1010010101110100       ;STY    $78
        .DBYTE  %1010010101110110       ;STZ    $7A
        .DBYTE  %1010100010110010       ;TAX    $7C
        .DBYTE  %1010100010110100       ;TAY    $7E
        .DBYTE  %1010110011101000       ;TRB    $80
        .DBYTE  %1010110100000110       ;TSB    $82
        .DBYTE  %1010110100110010       ;TSX    $84
        .DBYTE  %1010111001000100       ;TXA    $86
        .DBYTE  %1010111001101000       ;TXS    $88
        .DBYTE  %1010111010000100       ;TYA    $8A
        .DBYTE  %1100000010010100       ;WAI    $8C
;
;**************************************************************************************************
;                               END OF DISASSEMBLER DATA                                          *
;**************************************************************************************************
;                               END OF MONITOR DATA                                               *
;**************************************************************************************************
        .END