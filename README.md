[![Build Status](https://travis-ci.org/akirak/org-starter.svg?branch=master)](https://travis-ci.org/akirak/org-starter)
[![MELPA](https://melpa.org/packages/org-starter-badge.svg)](https://melpa.org/#/org-starter)

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

Install from MELPA.

`counsel-org-starter` and `helm-org-starter` in this repository are not available on MELPA yet. Install them individually after installing `org-starter` if you need them.

## Configuration

### Load path for org files

You can set up a "load path" for Org files. This path is later used to find an Org file
with a specific file name. To set the load path, customize `org-starter-path`.

## Usage

See [my package configuration for org-starter](https://github.com/akirak/emacs.d/blob/master/org/init-org-starter.el) and [my configuration file for files and directories](https://github.com/akirak/my-org/blob/master/my-org.el) for an example.

### Configure directories

Use `org-starter-define-directory` function to define a directory that contains Org files. You can add the directory to agenda files and/or refile targets via its options. 

You can also add it to the load path via `:add-to-path` option. 

For details of options, run `C-h f org-starter-define-directory`.

### Configure files

Use `org-starter-define-file` function to define an Org file. As with directory definitions, you can add the file to agenda files and/or refile targets. 

You can also define Org files inside `org-starter-define-directory` form as `:files` option.

For details, run `C-h f org-starter-define-file`.

### `org-starter-def` macro

You can also use `org-starter-def` define either a file or a directory. Depending on the type of the argument, it calls either `org-starter-define-file` or `org-starter-define-directory`. It supports the same functionality as the two functions, but it allows you to define files and directories slightly more concisely.

It basically supports the same as options as `org-starter-define-file`/`org-starter-define-directory`, but it doesn't need quoting. For example, the following two directives are equivalent:

``` emacs-lisp
(org-starter-define-file "~/hello.org"
:refile '(:maxlevel . 3))

(org-starter-def "~/hello.org"
:refile (:maxlevel . 3))
```

When you define a directory using `org-starter-def`, you can pass multiple arguments as `:files` option:

``` emacs-lisp
(org-starter-def "~/my-directory"
  :files
  ("file1.org" :agenda t)
  ("file2.org" :agenda nil :required nil))
```

It also supports an additional option `:config`. Its argument is evaluated after the other options are applied, as in `use-package`. This is executed if and only if the file/directory exists. It can take multiple arguments:

``` emacs-lisp
(org-starter-def "~/my-directory"
  :config
  (do-something)
  (do-another-thing))
```

### Extras

#### Locate a file

You can use `org-starter-locate-file` function to find an Org file contained in one of the directories in `org-starter-path`:

    (org-starter-locate-file "tasks.org")

To locate a file which is not in the path but already registered (defined) as a known file, use the function with an extra third argument. This function first tries to find a file in the list of known files:

``` emacs-lisp
(org-starter-locate-file "file-not-in-path.org" nil t)
```

#### Load all known files

It is sometimes convenient to load a specific set of Org files into Emacs as buffers. For example, you can search headings in the live Org buffers using `counsel-org-goto-all`.

- To load all files registered by `org-starter-define-file` into Emacs, use `org-starter-load-all-known-files` command.
- To load all files in `org-starter-path` into Emacs, use `org-starter-load-all-files-in-path` command.

### Helm interface to visit an Org file

A separate package `helm-org-starter.el` provides a Helm command named `helm-org-starter` which you can use to select an Org file in various categories or create a new one in a known directory.

### Counsel interface

`counsel-org-starter.el` file provides the following commands:

- `counsel-org-starter-known-file`, which lets you choose a known file
- `counsel-org-starter`, which lets you choose a known file or a file in `org-agenda-files`

## License

GPL v3
