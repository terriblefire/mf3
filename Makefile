all:
	z80asm mf3.asm -o mf3.rom ; md5sum mf3.rom ; md5sum original.rom
clean:
	rm -f mf3.rom *~
diff:
	cmp -l mf3.rom original.rom 