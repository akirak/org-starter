[![Build Status](https://travis-ci.org/akirak/org-starter.svg?branch=master)](https://travis-ci.org/akirak/org-starter)

# org-starter

Org-starter is a framework for basic configuration of Emacs Org Mode. It lets you configure Org Mode in the following manners:

- Declarative:
- Incremental:
- Orientated to files and directories: 

In other words, org-starter allows you to configure Org Mode in a manner similar
to use-package. The following is an example file configuration which is possible
with org-starter:

``` emacs-lisp
(org-starter-define-file "subjects.org"
  :agenda t
  :refile '(:maxlevel . 9))
```

## Prerequisites

- Emacs 25.1

## Installation

Not available on MELPA yet

## Usage

FIXME

## License

GPL v3
