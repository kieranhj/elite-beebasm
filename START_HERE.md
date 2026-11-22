# elite-beebasm: start here

The original BBC Micro Elite (cassette version, built for disc) source converted from BBC BASIC assembler to BeebAsm, so it builds reproducibly on a PC. 6502; a source port / reference build. (Mark Moxon's later fully commented Elite projects grew out of this; his Paul Brink commentary PR is merged here.)

| | |
|---|---|
| GitHub | https://github.com/kieranhj/elite-beebasm (public) |
| Upstream | Ian Bell's original Elite sources (links in README.md) |
| Local path | D:\beeb\mine\elite-beebasm (moved from OneDrive\BEEB\Repos\elite-beebasm on 2026-11-22) |
| Branch | comment-game-code (checked out); others: master, loader-comments, remove-encryption |
| Last commit | 2020-05-26: Merge pull request #4 from markmoxon/add-paul-brink-commentary |

## What's in here
- `elite-source.asm` - the game (original ELITEA-G concatenated); `elite-bcfs.asm` - joins the binaries into ELTcode.
- `elite-loader.asm` - the loader (`ELITE`); `elite-disc.asm` - builds `elite.ssd`.
- `elite-checksum.py`, `crc32.py` - checksums/encryption and verification (Python 2 or 3).
- `extracted/` - the original binaries; `output/` - built binaries (tracked in git); `sources/`, `data/`.
- `make.exe` - a copy of GNU make checked into the repo (for cmd.exe users).

## Build and run
- `make`: default target `build` - assembles the unencrypted build into `output/` and `elite.ssd`. Works. `make encrypt` builds the encrypted (original-identical) version; `make verify` compares CRCs of `extracted/` and `output/`.
- Note: building rewrites tracked files (`elite.ssd`, `output/*.bin`), because the committed outputs are from the encrypted build. Run `make encrypt` (or `git restore`) to get back to the committed state.
- Tools: beebasm (the Makefile now defaults to `..\..\Bin\beebasm.exe` on Windows; override with `BEEBASM=`), Python 3 as `python`.
- `make.bat` / `verify.bat` set `PYTHON=C:\Home\Python27\python.exe`, which no longer exists; use `make` / `make encrypt verify` from PowerShell instead. `make.bat` also launches b-em from `..\..\b-em\b-emV2.2Win` (old path).
- Run: `elite.ssd` in any BBC Model B emulator, SHIFT+BREAK.

## Local changes
- 2026-11-22: `Makefile` gained a Windows default for `BEEBASM` (uncommitted).
- No stashes, no snapshot branches. `comment-game-code` holds the commenting work (2018) and the 2020 Python 3 / Paul Brink merges; master stops at 2018-07-17.

## Status and next steps
- Dormant since 2020. README says the aim was readable, reproducible builds, with disc/Master/2nd-processor versions as future work. Mark Moxon's separate repos carry this on.

## See also
- README.md (background, file-by-file description, original archive links).
