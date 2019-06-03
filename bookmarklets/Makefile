MINIFIER ?= goog
JARPATH = $(HOME)/bin.local

BABEL = babel --presets=env
MINIFY = java -jar $(JAR) $(ARGS)

ifeq ($(MINIFIER),yui)
	PREP = $(BABEL)
	JAR = ${JARPATH}/yuicompressor-2.4.9.jar
	ARGS = --type js
else
	PREP = cat
	JAR = ${JARPATH}/google-closure-compiler-1.0-SNAPSHOT.jar
	ARGS =
endif

%:	%.js
	@echo minifying...
	@echo
	@$(PREP) < $< | $(MINIFY) | sed '1s/^/javascript:/' | pbcopy
	@echo
	@echo ...copied to clipboard
