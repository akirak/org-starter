[![Build Status](https://travis-ci.org/akirak/org-starter.svg?branch=master)](https://travis-ci.org/akirak/org-starter)

# org-starter

Org-starter is a framework for basic configuration of Emacs Org Mode. It allows you to configure Org Mode easily even with many files and directories. 

The standard way to configure Org Mode is set a bunch of variables such as `org-agenda-files` and `org-refile-targets`. This makes it hard to add/delete files to/from the configuration. Org-starter lets you configure Org Mode in a file-centric and incremental manner, which scales well especially if you have many Org files and sometimes have to tweak the file list. 

In other words, org-starter allows you to configure Org Mode in a manner similar
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

Use `org-starter-define-directory` function to define a directory that contains Org files. You can add the directory to agenda files and/or refile targets via its options. 

You can also add it to the load path via `:add-to-path` option. 

For details of options, run `C-h f org-starter-define-directory`.

### Configure files

Use `org-starter-define-file` function to define an Org file. As with directory definitions, you can add the file to agenda files and/or refile targets. 

You can also define Org files inside `org-starter-define-directory` form as `:files` option.

For details, run `C-h f org-starter-define-file`.

### Extras

#### Locate a file

You can use `org-starter-locate-file` function to find an Org file contained in one of the directories in `org-starter-path`:

    (org-starter-locate-file "tasks.org")

#### Load all known files

It is sometimes convenient to load a specific set of Org files into Emacs as buffers. For example, you can search headings in the live Org buffers using `counsel-org-goto-all`.

- To load all files registered by `org-starter-define-file` into Emacs, use `org-starter-load-all-known-files` command.
- To load all files in `org-starter-path` into Emacs, use `org-starter-load-all-files-in-path` command.

### Helm interface to visit an Org file

A separate package `helm-org-starter.el` provides a Helm command named `helm-org-starter` which you can use to select an Org file in various categories or create a new one in a known directory.

## License

GPL v3
