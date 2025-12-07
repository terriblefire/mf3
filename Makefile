all:
	docker run --platform linux/amd64 --rm -v $(PWD):/work -w /work z88dk/z88dk z80asm -b mf3.asm
	mv mf3.bin mf3.rom
	md5sum mf3.rom original.rom
clean:
	rm -f mf3.rom mf3.bin mf3.o *~
diff:
	cmp -l mf3.rom original.rom 