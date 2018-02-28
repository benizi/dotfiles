MINIFIER ?= goog
JARPATH = $(HOME)/bin.local

ifeq ($(MINIFIER),yui)
	JAR = ${JARPATH}/yuicompressor-2.4.9.jar
	ARGS = --type js
else
	JAR = ${JARPATH}/google-closure-compiler-1.0-SNAPSHOT.jar
	ARGS = -O ADVANCED
endif

%:	%.js
	@echo minifying...
	@echo
	@java -jar $(JAR) $(ARGS) < $< | sed '1s/^/javascript:/' | pbcopy
	@echo
	@echo ...copied to clipboard
