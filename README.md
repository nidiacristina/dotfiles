# Greg Thomsen's Dotfiles
This collection of configuration files has been refined to support scientific
computing (C/MATLAB/Fortran/Python) on Linux systems, usually without graphical
interfaces.  Development is centered around per-project `screen` sessions
hosting an instance of Emacs that drives development, debugging, and testing for
it.

# Highlights
There are a couple of things worth mentioning in this collection of
configuration files:

* An emphasis on driving everything from the keyboard.  Graphical tools are used
  when they're better than text-based alternatives, though aren't the default
  approach.

* Having consistently setup, self-contained `screen` instances.  Almost all
  project work starts with:

  ```shell
     $ screen code project-name
  ```

  An Emacs instance is started with a unique server name (`project-name`) and
  the environment is configured so all editing goes through it.  This allows
  multiple projects to be worked on concurrently, without co-mingling files and
  buffers.

* XTerms are configured with 8-bit color when available.  This provides a wider
  range of colors for Emacs to work with.

* Split shell configuration making intent clear by the fragment's name/contents.
  This is easy to adapt into existing shell configurations (sourcing the
  bootstrap piece) or dropping in wholesale.

* `screen` is configured with a prefix key of `C-j` instead of the normal `C-a`
  to spare my poor pinky when working in the shell and Emacs.

# Installation
A simple installation script, `install-as-symlinks.sh`, is available which
backs up existing configurations and creates symbolic links to the files in
this repository.  This enables easy updates as any changes made will visible
to Git without additional effort.

## Cloning the Repository
Since this repository includes other repositories as submodules, you'll need to
either 1) recursively clone this repository or 2) fetch each of the submodules
individually.

Recursive cloning is available for those who have setup SSH keys with their
Github account via:

```shell
  $ git clone --recursive git@github.com:gthomsen/dotfiles.git
```

While fetching each submodule can be done like so:

```shell
  $ git clone https://github.com/gthomsen/dotfiles.git
  $ cd dotfiles
  $ git config -f .gitmodules submodule.".emacs.d/elisp/align-f90".url \
                  https://github.com/gthomsen/align-f90.git
  $ for MODULE in align-f90 matlab; do \
       git submodule update --init .emacs.d/elisp/${MODULE}; \
    done
```

## Install Via Symbolic Links
Change into the repository's working directory and run `install-as-symlinks.sh`.
Existing configurations will be moved into
`${HOME}/.user-configuration-backups/` and new symbolic links will be made to
the working copy's contents.

## Install External Software
Several Emacs packages require external commands to function properly.  Below
are a list of external packages that should be installed for each Emacs package:

### `Markdown-mode`

* [Pandoc](http://pandoc.org/) is needed to render Markdown files into HTML for
  previews.  Optional if the mode is only used for syntax highlighting.

## Post-Installation Setup
Some setup is required before development can be done with this user
configuration.  See below for details.

### Git
The default `user.name` and `user.email` in the provided `.gitconfig are
hardcoded for the repository owner's convenience.  Please change them
prior to making commits so changes are not attributed to them!

``` shell
$ git config --global user.name "John Doe"
$ git config --global user.email "john.doe@domain.com"
```

### Emacs
The following packages need to be installed via MELPA:
- [dockerfile-mode](https://melpa.org/#/dockerfile-mode)
- [Magit](https://melpa.org/#/magit)
- [markdown-mode](https://melpa.org/#/markdown-mode)

Launch Emacs and run the following:

```
M-x package-refresh-contents RET
M-x package-install RET dockerfile-mode RET
M-x package-install RET magit RET
M-x package-install RET markdown-mode RET
```

Restart Emacs to pick up the full configuration.
