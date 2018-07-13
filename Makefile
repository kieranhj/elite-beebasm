BEEBASM?=beebasm
PYTHON?=python

.PHONY:build
build:
	$(BEEBASM) -i elite-source.asm -v > compile.txt
	$(BEEBASM) -i elite-bcfs.asm -v >> compile.txt
	$(BEEBASM) -i elite-loader.asm -v >> compile.txt
	$(PYTHON) elite-checksum.py
	$(BEEBASM) -i elite-disc.asm -do elite.ssd -boot ELITE

.PHONY:verify
verify:
	@$(PYTHON) crc32.py extracted
	@$(PYTHON) crc32.py output
