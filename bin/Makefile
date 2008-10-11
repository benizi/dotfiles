SRC = src
allsource = $(wildcard $(SRC)/*.c)

all:	$(notdir $(basename $(allsource)))

%:	src/%.c
	gcc -Wall -o $@.tmp $<
	mv $@.tmp $@

echo:
	echo $(basename $(allsource))
