EMACS ?= cask emacs

EMACS_BATCH = $(EMACS) -Q --batch --eval "(add-hook 'load-path \".\")"

FILES = org-starter.el helm-org-starter.el

default: package-lint byte-compile

run-package-lint.el:
	curl -OL https://gist.githubusercontent.com/akirak/f53d7a09b36ef40fa216631672e06582/raw/d6945f0a4690e2df2df3850d1816bdf6bf476dc7/run-package-lint.el

package-lint: run-package-lint.el
	$(EMACS_BATCH) \
	-l package-lint.el $(FILES)

byte-compile:
	$(EMACS_BATCH) \
	--eval '(setq byte-compile-error-on-warn t)' \
-f batch-byte-compile $(FILES)
