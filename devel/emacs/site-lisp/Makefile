all: $(patsubst %.el,%.elc,$(wildcard *.el))

%.elc: %.el
	emacs --batch --eval '(add-to-list '"'"'load-path ".")' --eval '(byte-compile-file "$<")'

clean:
	rm -f *.elc

.PHONY: all clean
