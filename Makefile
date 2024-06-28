##
# monolith
#
# @file
# @version 0.1

monolith:
	mkdir -p build
	gcc -std=c99 -Wall -lcurl monolith.c -o build/monolith
	./build/monolith

# end
