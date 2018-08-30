el = $(addprefix ~/.emacs.d/, config.el)
byte-compiled-files = $(addprefix ~/.emacs.d/, config.elc init.elc early-init.elc)

all: ${byte-compiled-files}

${byte-compiled-files}: ${el} init.el early-init.el
	emacs --batch -l init.el -f nox-byte-compile-init

${el}: config.org
	emacs --batch -l ob-tangle --eval "(org-babel-tangle-file \"config.org\" \"$@\")"

clean:
	@rm -f ${byte-compiled-files} ${el}

.PHONY: clean
