;**************************************************************************************************
;*                                                                                                *
;*              C02 Constants used for the 3.xx releases of C02BIOS3 and C02Monitor3              *
;*                                                                                                *
;*                                                                                                *
;*                                  03/02/2021 (Day/Month/Year)                                   *
;*                                                                                                *
;**************************************************************************************************
; C02BIOS Version 3.03                                                                            *
; - All Constants and Variables are now defined in a single source file (this one) for assembling *
; - both the C02BIOS3 and the C02Monitor3. It is also used for the Template for writing code to   *
; - be used for the C02 Pocket SBC and Adapters.                                                  *
;                                                                                                 *
; - Be sure to include this file at the start of any source file that needs it.                   *
;                                                                                                 *
;**************************************************************************************************
;
;       - Page Zero definitions ($00 to $9F reserved for user routines)
PGZERO_ST       .EQU    $A0                     ;Start of Page Zero usage for C02 Monitor
;
BUFF_PG0        .EQU    PGZERO_ST+00            ;Default Page zero location for Monitor buffers
;
INBUFF          .EQU    BUFF_PG0+00             ;Input Buffer - 4 bytes ($A0-$A3)
DATABUFF        .EQU    BUFF_PG0+04             ;Data Buffer - 6 bytes ($A4-$A9)
;
;       - 16-bit variables:
HEXDATAH        .EQU    PGZERO_ST+10            ;Hexadecimal input
HEXDATAL        .EQU    PGZERO_ST+11
BINVALL         .EQU    PGZERO_ST+12            ;Binary Value for HEX2ASC
BINVALH         .EQU    PGZERO_ST+13
COMLO           .EQU    PGZERO_ST+14            ;User command address
COMHI           .EQU    PGZERO_ST+15
INDEXL          .EQU    PGZERO_ST+16            ;Index for address - multiple routines
INDEXH          .EQU    PGZERO_ST+17
TEMP1L          .EQU    PGZERO_ST+18            ;Index for word temp value used by Memdump
TEMP1H          .EQU    PGZERO_ST+19
TEMP2L          .EQU    PGZERO_ST+20            ;Index for Text entry
TEMP2H          .EQU    PGZERO_ST+21
PROMPTL         .EQU    PGZERO_ST+22            ;Prompt string address
PROMPTH         .EQU    PGZERO_ST+23
SRCL            .EQU    PGZERO_ST+24            ;Source address for memory operations
SRCH            .EQU    PGZERO_ST+25
TGTL            .EQU    PGZERO_ST+26            ;Target address for memory operations
TGTH            .EQU    PGZERO_ST+27
LENL            .EQU    PGZERO_ST+28            ;Length address for memory operations
LENH            .EQU    PGZERO_ST+29
;
;       - 8-bit variables and constants:
BUFIDX          .EQU    PGZERO_ST+30            ;Buffer index
BUFLEN          .EQU    PGZERO_ST+31            ;Buffer length
IDX             .EQU    PGZERO_ST+32            ;Temp Indexing
IDY             .EQU    PGZERO_ST+33            ;Temp Indexing
TEMP1           .EQU    PGZERO_ST+34            ;Temp - Code Conversion routines
TEMP2           .EQU    PGZERO_ST+35            ;Temp - Memory/EEPROM/SREC routines - Disassembler
TEMP3           .EQU    PGZERO_ST+36            ;Temp - EEPROM/SREC routines
CMDFLAG         .EQU    PGZERO_ST+37            ;Command Flag, bit specific, used by many routines
OPXMDM          .EQU    PGZERO_ST+38            ;Saved Opcode/Xmodem Flag variable
;
;       - Xmodem transfer variables
CRCHI           .EQU    PGZERO_ST+39            ;CRC hi byte  (two byte variable)
CRCLO           .EQU    PGZERO_ST+40            ;CRC lo byte - Operand in Disassembler
CRCCNT          .EQU    PGZERO_ST+41            ;CRC retry count - Operand in Disassembler
PTRL            .EQU    PGZERO_ST+42            ;Data pointer lo byte - Mnemonic in Disassembler
PTRH            .EQU    PGZERO_ST+43            ;Data pointer hi byte - Mnemonic in Disassembler
BLKNO           .EQU    PGZERO_ST+44            ;Block number
;
;        - Macro Loop Counter variables
LPCNTL          .EQU    PGZERO_ST+45            ;Loop Count low byte
LPCNTH          .EQU    PGZERO_ST+46            ;Loop Count high byte
;
;       - Spare Monitor byte for future use
SPARE_M0        .EQU    PGZERO_ST+47            ;Spare Monitor page zero byte
;
IDE_STATUS_RAM  .EQU    PGZERO_ST+48            ;IDE RAM-Based Status
;
;       - BIOS variables, pointers, flags located at top of Page Zero
BIOS_PG0        .EQU    PGZERO_ST+56            ;Start of BIOS page 0 use ($D8-$FF, 40 bytes total)
;
;       - BRK handler routine
PCL             .EQU    BIOS_PG0+00             ;Program Counter Low index
PCH             .EQU    BIOS_PG0+01             ;Program Counter High index
PREG            .EQU    BIOS_PG0+02             ;Temp Status Reg
SREG            .EQU    BIOS_PG0+03             ;Temp Stack ptr
YREG            .EQU    BIOS_PG0+04             ;Temp Y Reg
XREG            .EQU    BIOS_PG0+05             ;Temp X Reg
AREG            .EQU    BIOS_PG0+06             ;Temp A Reg
;
;       - 2691 IRQ handler pointers and status
ICNT            .EQU    BIOS_PG0+07             ;Input buffer count
IHEAD           .EQU    BIOS_PG0+08             ;Input buffer head pointer
ITAIL           .EQU    BIOS_PG0+09             ;Input buffer tail pointer
OCNT            .EQU    BIOS_PG0+10             ;Output buffer count
OHEAD           .EQU    BIOS_PG0+11             ;Output buffer head pointer
OTAIL           .EQU    BIOS_PG0+12             ;Output buffer tail pointer
UART_IRT        .EQU    BIOS_PG0+13             ;2691 Interrupt Status byte
UART_SRT        .EQU    BIOS_PG0+14             ;2691 Status Register byte
;
;       - Real-Time Clock variables
; These are repurposed for adding a Realtime clock chip DS1511Y
; The Ticks, Seconds, Minutes and Hours remain the same in function.
; The 16-bit Days variable is replaced however.
; - The DAY_DATE is a new variable. To minimize Page Zero usage, it has two functions
;       Bits 0-4 represent the days of the Month 1-31
;       Bits 5-7 represent the Day of the Week, 1-7 (Saturday=1)
; The Months are handled by the upper 4 bits of the MONTH_YEAR variable
; The Century is handled by a the Year (0-255) and the lower 4 bits of the MONTH_YEAR variable
TICKS           .EQU    BIOS_PG0+15             ;Number of timer countdowns = 1 second (100)
SECS            .EQU    BIOS_PG0+16             ;Seconds: 0-59
MINS            .EQU    BIOS_PG0+17             ;Minutes: 0-59
HOURS           .EQU    BIOS_PG0+18             ;Hours: 0-23
DAY_DATE        .EQU    BIOS_PG0+19             ;Day: (bits 5-7) Date: (bits 0-4)
MONTH_CENTURY   .EQU    BIOS_PG0+20             ;Month: (bits 4-7) Century: (bits 0-3)
YEAR            .EQU    BIOS_PG0+21             ;Century 0-255 plus 4 bits as noted above
RTC_TEMP        .EQU    BIOS_PG0+22             ;Temp work byte for updating shared variables
;
;       - Delay Timer variables
MSDELAY         .EQU    BIOS_PG0+23             ;Timer delay countdown byte (255 > 0)
SETMS           .EQU    BIOS_PG0+24             ;Set timeout for delay routines - BIOS use only
DELLO           .EQU    BIOS_PG0+25             ;Delay value BIOS use only
DELHI           .EQU    BIOS_PG0+26             ;Delay value BIOS use only
XDL             .EQU    BIOS_PG0+27             ;XL Delay count
;
;       - Count variables for 10ms benchmark timing
MS10_CNT        .EQU    BIOS_PG0+28             ;10ms Count variable
SECL_CNT        .EQU    BIOS_PG0+29             ;Seconds Low byte count
SECH_CNT        .EQU    BIOS_PG0+30             ;Second High byte count
;
;       - Adddress and pointers for CF-Card IDE Interface
LBA_ADDR_LOW    .EQU    BIOS_PG0+31             ;LBA Transfer Address low byte
LBA_ADDR_HIGH   .EQU    BIOS_PG0+32             ;LBA Transfer Address high byte
LBA_XFER_CNT    .EQU    BIOS_PG0+33             ;LBA Transfer Count

LBA_LOW_BYTE    .EQU    BIOS_PG0+34             ;LBA Block number 0-7
LBA_HIGH_BYTE   .EQU    BIOS_PG0+35             ;LBA Block number 8-15
LBA_EXT_BYTE    .EQU    BIOS_PG0+36             ;LBA Block number 16-23
;
BIOS_XFERL      .EQU    BIOS_PG0+37             ;BIOS Move Routine low byte
BIOS_XFERH      .EQU    BIOS_PG0+38             ;BIOS Move Routine high byte
;
;       - Timer/Counter Match flag for Delay/Benchmark
MATCH           .EQU    BIOS_PG0+39             ;Bit7 used for Delay, Bit6 used for Benchmark
                                                ;Bits 4,5 used for BRG Test register status
                                                ;Bits 3,2,1 used for CF Card Interrupt Handler
;
;       - Default for RTC tick count - number of IRQs for 1 second
DF_TICKS        .EQU    100                     ;Timer is 10 milliseconds (100 x 10ms = 1 second)
;
;**************************************************************************************************
IBUF            .EQU    $0200                   ;Console Input Buffer - 128 bytes
OBUF            .EQU    $0280                   ;Console Output Buffer - 128 bytes
;**************************************************************************************************
SOFTVEC         .EQU    $0300                   ;Start of soft vectors
;The Interrupt structure is vector based. During startup, Page $03 is loaded from ROM.
; The soft vectors are structured to allow inserting additional routines either before
; or after the ROM based routines. This allows flexibility and changing of routine priority.
;
;The main set of vectors occupy the first 16 bytes of Page $03. The ROM handler for
; NMI, BRK and IRQ jump to the first 3 vectors. The following 3 vectors are loaded with
; return addresses to the ROM handler for each. The following 2 vectors are the cold and
; warm entry points for the Monitor. After the basic initialization, the monitor is entered.
;
;The following vector set allows inserts, pre or post for NMI/BRK/IRQ. There a total of 8 inserts
; which occupy 16 bytes. They can be used as required.
; Currently, VECINSRT0 will be used if a CF-Card IDE Controller is detected.
;
NMIVEC0         .EQU    SOFTVEC+00              ;NMI Vector Entry 0
BRKVEC0         .EQU    SOFTVEC+02              ;BRK Vector Entry 0
IRQVEC0         .EQU    SOFTVEC+04              ;IRQ Vector Entry 0
;
NMIRTVEC0       .EQU    SOFTVEC+06              ;NMI Vector Return 0
BRKRTVEC0       .EQU    SOFTVEC+08              ;BRK Vector Return 0
IRQRTVEC0       .EQU    SOFTVEC+10              ;IRQ Vector Return 0
;
CLDMNVEC0       .EQU    SOFTVEC+12              ;Monitor Cold Entry Vector 0
WRMMNVEC0       .EQU    SOFTVEC+14              ;Monitor Warm Entry Vector 0
;
VECINSRT0       .EQU    SOFTVEC+16              ;1st Vector Insert
VECINSRT1       .EQU    SOFTVEC+18              ;2nd Vector Insert
VECINSRT2       .EQU    SOFTVEC+20              ;3rd Vector Insert
VECINSRT3       .EQU    SOFTVEC+22              ;4th Vector Insert
VECINSRT4       .EQU    SOFTVEC+24              ;5th Vector Insert
VECINSRT5       .EQU    SOFTVEC+26              ;6th Vector Insert
VECINSRT6       .EQU    SOFTVEC+28              ;7th Vector Insert
VECINSRT7       .EQU    SOFTVEC+30              ;8th Vector Insert
;
;**************************************************************************************************
SOFTCFG         .EQU    SOFTVEC+32              ;Start of hardware config parameters
;Soft Config values below are loaded from ROM and are the default I/O setup configuration data that
; the INIT_x routines use. As a result, you can write a routine to change the I/O configuration
; data and use the standard ROM routines to initialize the I/O without restarting or changing ROM
; A Reset (HW or coded) will reinitialize the I/O with the ROM default I/O configuration.
;
;There are a total of 32 Bytes configuration data reserved starting at $0320
;
LOAD_2691       .EQU    SOFTCFG+00              ;SCC2691 Soft Config Data
;
LOAD_IDE        .EQU    SOFTCFG+16              ;IDE/CF-Card Soft Config Data
;
;Search Buffer is 16 bytes in length. Used to hold search string for text or hex data
SRCHBUFF        .EQU    SOFTCFG+32              ;Located in Page $03 following Hardware config data
;
;Xmodem/CRC Loader also provides Motorola S19 Record sense and load. Designed to handle the S19
; records from the WDC Assembler/Linker package. This requires a 44 byte buffer to parse each valid
; S1 record, located just before the 132 Byte Xmodem frame buffer. Total Buffer space for the
; Xmodem/CRC Loader is 176 bytes
;
;Valid S-record headers are "S1" and "S9". For S1, the maximum length is "$19" hex. The last S1
; record can be less. S9 record is always the last record with no data. WDC Linker also appends
; a CR/LF to the end of each record for a total of 44 bytes.
SRBUFF          .EQU    SOFTCFG+48              ;S-Record buffer, up to 44 bytes in length
;
;Xmodem frame buffer. The entire Xmodem frame is buffered here and then checked for proper header
; and frame number, CRC-16 on the data, then moved to user RAM.
RBUFF           .EQU    SOFTCFG+92              ;Xmodem temp 132 byte receive buffer
;
;Page $03 is completely allocated for Buffers, Config Data and Vector pointers.
; Some of the buffer space can be used as needed, provided any required Monitor functions are NOT
; being used concurrently.
;
;Page $04 is Reserved for a second UART Receive/Transmit buffer (future SC28L92)
IBUF2           .EQU    $0400                   ;Alternate Input Buffer - 128 bytes
OBUF2           .EQU    $0480                   ;Alternate Output Buffer - 128 bytes
;
;Page $05 is used for the Realtime Clock NVRAM read and write routines
NVRAM_DATA      .EQU    $0500                   ;NVRAM Data Buffer address
;
;Pages $06 - $07 are used for the IDE device Block Buffer (512 bytes)
LBA_BUFFER      .EQU    $0600                   ;Default IDE Block Buffer address
; 
;XMODEM Control Character Constants
SOH             .EQU    $01                     ;Start of Block Header
EOT             .EQU    $04                     ;End of Text marker
ACK             .EQU    $06                     ;Good Block Acknowledge
NAK             .EQU    $15                     ;Bad Block acknowledged
CAN             .EQU    $18                     ;Cancel character
;
;**************************************************************************************************
;RAM location used for the EEPROM Byte Write routine
; Note: location moved from $00 to $88 to avoid conflict with EhBasic Page 0 usage
;
BURN_BYTE       .EQU    $0088                   ;Page 0 RAM for EEPROM BYTE write routine
;**************************************************************************************************
;EnHanced Basic is called from the Monitor via the Ctrl-B comand. The location can be anywhere in
; EEPROM from $8000 to $B000. The start location is just added here for convienience. If anyone
; decides to change the location, just update the start address here:
EH_BASIC        .EQU    $B000                   ;Default location for EnHanced BASIC (optional)
;**************************************************************************************************
IOPAGE          .EQU    $FE00                   ;I/O Page Base Start Address
;**************************************************************************************************
SCC2691_BASE    .EQU    IOPAGE+$80              ;Beginning of Console UART address
;
UART_MODEREG    .EQU    SCC2691_BASE+$00        ;MR1/MR2 same address, sequential read/write
UART_STATUS     .EQU    SCC2691_BASE+$01        ;UART Status Register (READ)
UART_CLKSEL     .EQU    SCC2691_BASE+$01        ;UART Clock Select Register (WRITE)
UART_BRGTST     .EQU    SCC2691_BASE+$02        ;UART BRG Test Register (READ)
UART_COMMAND    .EQU    SCC2691_BASE+$02        ;UART Command Register (WRITE)
UART_RECEIVE    .EQU    SCC2691_BASE+$03        ;UART Receive Register (READ)
UART_TRANSMIT   .EQU    SCC2691_BASE+$03        ;UART Transmit Register (WRITE)
UART_CLKTEST    .EQU    SCC2691_BASE+$04        ;X1/X16 Test Register (READ)
UART_AUXCR      .EQU    SCC2691_BASE+$04        ;Aux Command Register (WRITE)
UART_ISR        .EQU    SCC2691_BASE+$05        ;Interrupt Status Register (READ)
UART_IMR        .EQU    SCC2691_BASE+$05        ;Interrupt Mask Register (WRITE)
UART_CNTU       .EQU    SCC2691_BASE+$06        ;Counter/Timer Upper Register (READ)
UART_CNTUP      .EQU    SCC2691_BASE+$06        ;Counter/Timer Upper Preset Register (WRITE)
UART_CNTL       .EQU    SCC2691_BASE+$07        ;Counter/Timer Lower Register (READ)
UART_CNTLP      .EQU    SCC2691_BASE+$07        ;Counter/Timer Lower Preset Register (WRITE)
;
;Additional Hardware
; Adding BIOS definitions for Realtime Clock chip - DS1511Y
; uses the first 16 addresses for RTC registers and basic operation
; uses two addresses for extended RAM of 256 bytes
;
; upper addresses are used for a 16-bit IDE interface (below)
; NOTE: offset $11 and $12 are unused (reserved per the datasheet).
;
RTC_IDE_BASE    .EQU    IOPAGE+$60              ;Beginning of Realtime Clock address
;
RTC_SECONDS     .EQU    RTC_IDE_BASE+$00        ;Seconds in BCD 00-59
RTC_MINUTES     .EQU    RTC_IDE_BASE+$01        ;Minutes in BCD 00-59
RTC_HOURS       .EQU    RTC_IDE_BASE+$02        ;Hours in BCD 00-23
RTC_DAY         .EQU    RTC_IDE_BASE+$03        ;Day in BCD 1-7
RTC_DATE        .EQU    RTC_IDE_BASE+$04        ;Date in BCD 1-31
RTC_MONTH       .EQU    RTC_IDE_BASE+$05        ;Month in BCD 1-12
RTC_YEAR        .EQU    RTC_IDE_BASE+$06        ;Year in BCD 00-99
RTC_CENTURY     .EQU    RTC_IDE_BASE+$07        ;Century in BCD 00-39
RTC_ALARM_SEC   .EQU    RTC_IDE_BASE+$08        ;Alarm Seconds in BCD 00-59
RTC_ALARM_MIN   .EQU    RTC_IDE_BASE+$09        ;Alarm Minutes in BCD 00-59
RTC_ALARM_HRS   .EQU    RTC_IDE_BASE+$0A        ;Alarm Hours in BCD 00-23
RTC_ALARM_DYDT  .EQU    RTC_IDE_BASE+$0B        ;Alarm Day/Date in BCD 0-7 1-31
RTC_WTCHDOG_01  .EQU    RTC_IDE_BASE+$0C        ;Watchdog 0.1 / 0.01 Seconds in BCD 00-99
RTC_WTCHDOG_10  .EQU    RTC_IDE_BASE+$0D        ;Watchdog 10 / 1 Seconds in BCD 00-99
RTC_CONTROL_A   .EQU    RTC_IDE_BASE+$0E        ;Control A
RTC_CONTROL_B   .EQU    RTC_IDE_BASE+$0F        ;Control B
RTC_RAM_ADDR    .EQU    RTC_IDE_BASE+$10        ;Extended RAM address
RTC_RAM_DATA    .EQU    RTC_IDE_BASE+$13        ;Extended RAM data
;
; Adding BIOS definitions for 16-bit IDE interface
; uses two addresses for Upper Byte Latch read / write
; uses eight addresses for Command Block Registers
; uses two addresses for Control Block Registers
;
IDE_16_READ     .EQU    RTC_IDE_BASE+$14        ;Upper byte Read address
IDE_16_WRITE    .EQU    RTC_IDE_BASE+$15        ;Upper byte Write address
;
; Adding BIOS definitions for IDE Controller (HARD DISK, Flash Module, etc.)
; Hardware Adapter provides a 16-bit IDE Port per:
;        Seagate ATA Interface Reference Manual 36111-001, Rev. C (21st May 1993)
;
; Compact Flash Adapter BIOS is based on documentation from SanDisk:
;       OEM Product Manual Version 12.0 Doc # 20-10-00038m 02/2007
;
; Control Block Registers
IDE_ALT_STATUS  .EQU    RTC_IDE_BASE+$16        ;Alternate Status Register (READ)
IDE_DEV_CTRL    .EQU    RTC_IDE_BASE+$16        ;Device Control Register (WRITE)
IDE_DRV_ADDR    .EQU    RTC_IDE_BASE+$17        ;Drive Address Register (READ)
;
; Command Block Registers
IDE_DATA        .EQU    RTC_IDE_BASE+$18        ;Data Register (R/W)
IDE_ERROR       .EQU    RTC_IDE_BASE+$19        ;Error Register (READ)
IDE_FEATURE     .EQU    RTC_IDE_BASE+$19        ;Feature Register (WRITE)
IDE_SCT_CNT     .EQU    RTC_IDE_BASE+$1A        ;Sector Count Register
IDE_SCT_NUM     .EQU    RTC_IDE_BASE+$1B        ;Sector Number Register
IDE_CYL_LOW     .EQU    RTC_IDE_BASE+$1C        ;Cylinder Low Register
IDE_CYL_HIGH    .EQU    RTC_IDE_BASE+$1D        ;Cylinder High Register
IDE_DRV_HEAD    .EQU    RTC_IDE_BASE+$1E        ;Drive/Head Register
IDE_STATUS      .EQU    RTC_IDE_BASE+$1F        ;Status Register (READ)
IDE_COMMAND     .EQU    RTC_IDE_BASE+$1F        ;Command Register (WRITE)
;
;**************************************************************************************************
        .END