EMAKE_SHA1            := 9095599536e5b3ad8c34a3dd3362dbb92ebf701f
PACKAGE_BASENAME      := counsel-org-starter
PACKAGE_LISP          := org-starter.el counsel-org-starter.el helm-org-starter.el
# override defaults
PACKAGE_ARCHIVES      := gnu melpa
#PACKAGE_TESTS         := test-sample.el # normally, EMake would discover these in the test/ directory
PACKAGE_TEST_DEPS     := dash
PACKAGE_TEST_ARCHIVES := gnu melpa

include emake.mk

.DEFAULT_GOAL: help

### Bootstrap and convenience targets

emake.mk:                       ## download the emake Makefile
	curl -O 'https://raw.githubusercontent.com/vermiculus/emake.el/$(EMAKE_SHA1)/emake.mk'

#test: test-ert test-buttercup   ## run tests
lint: lint-package-lint lint-checkdoc ## run lints
