# elite-beebasm
A port of the original BBC Elite source code from BASIC assembler to BeebAsm assembler for modern PC development environment.

## Background
The original source files for BBC Elite can be found on [Ian Bell's personal web](http://blah).

The following archives are available:

- [Cassette sources](link) as DFS disk image
- [Cassette sources](link) as text files
- [BBC 2nd processor sources](link) as DFS disk image
- [Original Elite ship sources](link) as DFS disk image
- [Elite 2 ship sources](link) as DFS disk image
- [Original BBC Disk version](link) of Elite (game only)
- [Master 128 and 2nd Processor versions](link) of Elite (game only)

As the game was written on 8-bit machines with very limited RAM and disk storage (the game started life on an Acorn Atom) the source code is incredibly terse, densely packed and effectively unreadable to anyone but the original authors (even then, I'd imagine both would struggle some 30+ years later..!)

This project aims to develop a readable, fast and reproducible build of Elite that can be used for learning and non-profit modification purposes.

## Version

The BBC Cassette version of the game but built for disk was chosen as the initial starting point for simplicity. It generates just two binary executable files `ELITE` (the loader) and `ELTcode` (the game) and will run on a standard issue Model B with DFS, which is the most common configuration of any BBC system and easily emulated.

Future versions may include BBC Disk, Master and 2nd processor configurations.

## Files

### elite-source.asm

This is the main source for the Elite game. It is made up of 7 original BASIC source files concatenated and converted to BeebAsm assembler syntax:

- `ELITEA` outputting `ELTA` binary
- `ELITEB` outputting `ELTB` binary
- `ELITEC` outputting `ELTC` binary
- `ELITED` outputting `ELTD` binary
- `ELITEE` outputting `ELTE` binary
- `ELITEF` outputting `ELTF` binary
- `ELITEG` outputting `ELTG` binary

It totals ~10,000 lines of 6502 assembler.

### elite-bcfs.asm

The BASIC source file `S.BCFS` is responsible for creating the Big Code File, i.e. concatenating the `ELTA`..`ELTG` binaries plus the `SHIPS` data into a single executable for the Elite main game called `ELTcode`.

There is a simple checksum test added to the start of the code. The checksum function cannot be performed in the BeebAsm source so has been reproduced in the `elite-checksum.py` Python script described below.

### elite-loader.asm

The BASIC source file `ELITES` creates the executable Elite loader `ELITE`. This is responsible for displaying the title screen and planet, drawing the initial (static) HUD, setting up interrupt routines (for the MODE 4/5 split in the HUD), relocating many routines to lower memory (below `PAGE`) and loading the main executable.

There are a number of checksum and protection routines that XOR the code and data with other parts of memory in an attempt to obfuscate and protect the game from tampering. This cannot be done in the BeebAsm source so has been reproduced in the `elite-checksum.py` Python script below.

### elite-checksum.py

There are a number of checksum and simple XOR encryption routines that form part of the Elite build process. These are trivial to interleave with the assembly process in BBC BASIC but have had to be converted to a Python script to run as part of a modern development environment.

The script has two parts. Firstly performing some functions from the `S.BCFS` source to generate a protected version of the `ELTcode` binary:

- Concatenate all Elite game binaries
- Compute checksum for Commander data
` Poke Commander checksum value into binary
- Compute checksum for all game code except boot header
- Poke checksum value into binary
- Encrypt all game code except boot header with cycling XOR value (0-255)
- Compute final checksum for game code
- Output `ELTcode` binary (protected)

Secondly it performs the checksum and encryption functions from the `ELITES` loader source:

- Reverse the bytes for a block of code that is placed on the stack
- Compute checksum for MAINSUM
` Poke checksum value into binary
- Comuter checksum for CHECKbyt
- Poke checksum value into binary
- Encrypt a block of code by XOR'ing with the code to be placed on the stack
- Encrypt all code destined for lower RAM by XOR'ing with loader boot code
- Encrypt binary data (HUD graphics etc.) by XOR'ing with loader boot code
- Output `ELITE` binary (protected)

## Build

For now a simple DOS Batch file `make.bat` will perform the following commands:

```
%BEEBASM% -i elite-source.asm -v
%BEEBASM% -i elite-bcfs.asm -v
%BEEBASM% -i elite-loader.asm -v
%PYTHON% elite-checksum.py
%BEEBASM% -i elite-disc.asm -do elite.ssd -boot ELITE
```
Simply define the location of your `beebasm.exe` and `python.exe` at the head of the batch file. All being well this will output `elite.ssd` which will boot the game.

## Verify

The `crc32dos.exe` is included to verify that the output files are binary identical with the original sources.

The following built binaries were extracted from the [Cassette sources disk image](link):

```
 CRC 32     File Size   Filename
-------------------------------------------------
a88ca82b         5426   \extracted\ELITE.bin
0f1ad255         2228   \extracted\ELTA.bin
0d6f4900         2600   \extracted\ELTB.bin
97e338e8         2735   \extracted\ELTC.bin
01a00dce        20712   \extracted\ELTcode.bin
322b174c         2882   \extracted\ELTD.bin
29f7b8cb         2663   \extracted\ELTE.bin
8a4cecc2         2721   \extracted\ELTF.bin
7a6a5d1a         2340   \extracted\ELTG.bin
```
The following binaries are output from the BeebAsm build process:
```
 CRC 32     File Size   Filename
-------------------------------------------------
a88ca82b         5426   \output\ELITE.bin
f40816ec         5426   \output\ELITE.unprot.bin
0f1ad255         2228   \output\ELTA.bin
3f107178         2600   \output\ELTB.bin
97e338e8         2735   \output\ELTC.bin
01a00dce        20712   \output\ELTcode.bin
8de09f8f        20712   \output\ELTcode.unprot.bin
322b174c         2882   \output\ELTD.bin
29f7b8cb         2663   \output\ELTE.bin
8a4cecc2         2721   \output\ELTF.bin
7a6a5d1a         2340   \output\ELTG.bin
00d5bb7a           40   \output\ELThead.bin
```
## Differences

### ELITEC

It was discovered that the Text Files archive does not contain the identical source to the Disk Image archive. A couple of small differences in code in the `ELITEC` source:

```
.WARP LDAMANY+AST
CLC
ADCMANY+ESC
CLC         ; NOT IN ELITEC.TXT but is in ELITE SOURCE IMAGE

ADCMANY+OIL
TAX
LDAFRIN+2,X
ORASSPR
ORAMJ
BNEWA1
LDYK%+8
BMIWA3
TAY
JSRMAS2
;LSRA
;BEQWA1
CMP #2
BCC WA1     ; NOT IN ELITEC.TXT but is in ELITE SOURCE IMAGE
.WA3 LDYK%+NI%+8
BMIWA2
LDY#NI%
JSRm
;LSRA
;BEQWA1
CMP #2
BCC WA1     ; NOT IN ELITEC.TXT but is in ELITE SOURCE IMAGE
```

### ELTB

The `ELTB` binary output is not identical to the extracted file yet the final `ELTcode` binary is. What gives?

This comes down to a single byte in the default Commander data and is related to whether the BASIC variable `Q%` is TRUE or FALSE. This appears to be a cheat flag used during testing.

The implication is that the output binary files on the Cassette source disk were not produced at the same time but one was modified after the `ELTcode` file was created. (We can see that running the build process in an emulator results in a different output and checksum values.)

A decision was made to structure the `elite-source.asm` file so that the final `ELTcode` executable output is binary identical to the file extracted from the Cassette Source disk image.

## Next Steps

Although the binary files output are identical, the build process is *brittle* meaning that the source cannot be altered. The main problem is that the encrytion process does not have knowledge of the symbols produced by the assembler, so these values have been hard coded for temporary convenience.

The next steps are:

- Remove code requiring checksums and copy protection to allow source to be modified freely
- Improve whitespacing for readability
- Improve label names for readaibility
- Commenting of critical functions
- Add BBC Disk, Master and 2nd processor versions to build

I am fully open to PR's if anyone feels like contributing to this project!

---
#### Kieran Connell | July 2017
