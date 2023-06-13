.PHONY: test

test: ./zig-out/bin/d15_zig input.txt
	 ./zig-out/bin/d15_zig input.txt

./zig-out/bin/d15_zig: src/main.zig
	zig build -Drelease-fast
