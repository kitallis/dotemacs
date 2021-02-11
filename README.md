# dotemacs

Primarily optimized for Clojure. Tons of other editing goodness.

Uses the [straight](https://github.com/raxod502/straight.el) package manager. Install [Fira Code](https://github.com/tonsky/FiraCode) for text and [Inconsolata Nerd Font](https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/Inconsolata/complete/Inconsolata%20Nerd%20Font%20Complete.otf) for modeline.

## Maintainence

* Straight, general appearance and package configuration goes in `init.el`
* Keybindings, themes go in `init-user.el`
* All code/programming related package configuration goes in `init-code.el`
* Helper functions go in `init-efuns.el`
* Run `M-x straight-freeze-dependencies` to keep the lockfile updated for all `straight` recipes


## Installing

#### OS X

Install vanilla Emacs as a Mac app from http://emacsformacosx.com. Other options, like Aquamacs, are supposed to make Emacs more “Mac-like,” but they’re problematic in the long run because they’re set up so differently from standard Emacs that it’s difficult to use the Emacs manual or follow along with tutorials.

#### Ubuntu
Follow the instructions at https://launchpad.net/~cassou/+archive/emacs.

#### Windows
You can find a binary at http://ftp.gnu.org/gnu/emacs/windows. After you download and unzip the latest version, you can run the Emacs executable under bin\runemacs.exe.

## Setup
```
git clone https://github.com/kitallis/dotemacs.git ~/.emacs.d
```

On the first run, Emacs will install and compile any packages handled by the package manager.
