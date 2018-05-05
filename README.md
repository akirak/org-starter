[![Build Status](https://travis-ci.org/akirak/org-starter.svg?branch=master)](https://travis-ci.org/akirak/org-starter)

# org-starter

Org-starter is a framework for basic configuration of Emacs Org Mode. It allows you to configure Org Mode easily even with many files and directories. 

The standard way to configure Org Mode is set a bunch of variables such as `org-agenda-files` and `org-refile-targets`. This makes it hard to add/delete files to/from the configuration. Org-starter lets you configure Org Mode in a file-centric and incremental manner, which scales well especially if you have many Org files and sometimes tweak the file list. 

In other words, org-starter allows you to configure Org Mode in a manner that is similar
to use-package. The following is an example file configuration with org-starter:

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
