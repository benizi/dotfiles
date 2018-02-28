JAR = $(HOME)/bin.local/google-closure-compiler-1.0-SNAPSHOT.jar
ARGS = -O ADVANCED

%:	%.js
	@echo minifying...
	@echo
	@java -jar $(JAR) $(ARGS) < $< | sed '1s/^/javascript:/' | pbcopy
	@echo
	@echo ...copied to clipboard
