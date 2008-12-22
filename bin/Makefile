SRC = src
allsource = $(wildcard $(SRC)/*.c)

all:	$(notdir $(basename $(allsource)))

place-holder:	src/place-holder.c
	gcc -Wall -o $@.tmp $< -lcurses
	mv $@.tmp $@

%:	src/%.c
	gcc -Wall -o $@.tmp $<
	mv $@.tmp $@

echo:
	echo $(basename $(allsource))
