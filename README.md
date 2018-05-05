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

## Configuration

### Load path for org files

You can set up a "load path" for Org files. This path is later used to find an Org file
with a specific file name. To set the load path, customize `org-starter-path`.

## Usage

### Configure directories

Use `org-starter-define-directory` function to define a directory that contains Org files. You can add agenda files and refile targets via its options. You can also add it the load path via `:add-to-path` option. Use `describe-function` to view the details. 

### Configure files

Use `org-starter-define-file` function to define an Org file. As with directory definitions, you can add agenda files and refile targets. Use `describe-function` to view the details. 

You can also define Org files inside `org-starter-define-directory` form as `:files` option.

### Extras

#### Locate a file

You can use `org-starter-locate-file` function to find an Org file in the load path. 

    (org-starter-locate-file "tasks.org")

#### Load all known files

It is sometimes convenient to load a specific set of Org files into Emacs as buffers. For example, you can search headings in the Org buffers using `counsel-org-goto-all`.

Use `org-starter-load-all-known-files` command to load all files registered by `org-starter-define-file` into Emacs.

Use `org-starter-load-all-files-in-path` command to load all files in the load path into Emacs.

## License

GPL v3
