# Space-separated list of the dependencies of your project (include package-lint if
# you want makel to use this linting tool):
ELPA_DEPENDENCIES=dash package-lint ivy swiper

# List of package archives to download above dependencies
# from. Available archives are: gnu, melpa, melpa-stable and org:
ELPA_ARCHIVES=melpa

# List of ERT test files:
TEST_ERT_FILES=$(wildcard test/*.el)

# List of files to check for Emacs conventions:
LINT_CHECKDOC_FILES=$(wildcard *.el) $(wildcard test/*.el)

# List of files to check for packaging guidelines:
LINT_PACKAGE_LINT_FILES=$(wildcard *.el) $(wildcard test/*.el)

# List of files to check for compilation errors and warnings:
LINT_COMPILE_FILES=org-starter.el org-starter-swiper.el

makel.mk:
	@if [ -f ../makel/makel.mk ]; then \
		ln -s ../makel/makel.mk .; \
	else \
		curl \
		--fail --silent --show-error --insecure --location \
		--retry 9 --retry-delay 9 \
		-O https://gitlab.petton.fr/DamienCassou/makel/raw/v0.5.1/makel.mk; \
	fi

# Include makel.mk if present
-include makel.mk
