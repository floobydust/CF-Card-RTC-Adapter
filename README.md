# CF-Card-RTC-Adapter

An Adapter to be used with the C02 Pocket SBC.

This adapter provides a Maxim DS1511Y Realtime Clock module and a Compact Flash adapter on a single PCB.
It uses a single I/O Select (32 bytes wide) from the Pocket SBC and a ATF16V8BQL PLD provides the chip selects for the Realtime CLock and the Compact Flash interface. All registers for the Compact Flash are available and 16-bit Data transfers are enabled by using a pair of 74HC573 Latches for reading and writing the upper 8-bits.

Jumpers for interrupt are provided for the DS1511Y and the Compact Flash interface.
 - The schematic image has been replaced with a corrected one, sorry for the oops.
 - Also, a new BIOS and Monitor have been added under Software, now at Version 3.04, which is the latest and greatest.

Note: BIOS and Monitor version 3.03 or later support this adapter. Any 2.xx versions will NOT provide any access for this adapter.

