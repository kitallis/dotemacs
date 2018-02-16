## dotemacs

A short and sweet emacs setup for Clojure brought to you by the folks at Nilenso. This is intended to get folks up and running with Emacs for editing Clojure code with zero configuration. If you are interested in further functionality, feel free to go down that rabbithole :)

To get a taste of what Emacs has to offer, you might want to check out the short screencasts at http://emacsrocks.com/.

## Installing

### Get Emacs

#### OS X

Install vanilla Emacs as a Mac app from http://emacsformacosx.com. Other options, like Aquamacs, are supposed to make Emacs more “Mac-like,” but they’re problematic in the long run because they’re set up so differently from standard Emacs that it’s difficult to use the Emacs manual or follow along with tutorials.

#### Ubuntu
Follow the instructions at https://launchpad.net/~cassou/+archive/emacs.

#### Windows 
You can find a binary at http://ftp.gnu.org/gnu/emacs/windows/. After you download and unzip the latest version, you can run the Emacs executable under bin\runemacs.exe.

### Setup Configuration
```
git clone https://gitlab.com/nilenso/dotemacs ~/.emacs.d
```

### User customizations
Create a file init-user.el under your emacs.d. Add any customizations you need there.

Open emacs and you're good to go. Happy Hacking!

## Packages Used
- use-package => *Package Manager*
- exec-path-from-shell => *Sets up your path and environment variables from your shell's configuration (.bashprofile, .zshenv, ...)*
- company => *The autocompletion engine*
- cider => *Connecting a Clojure buffer to a REPL*
- clojure-mode-extra-font-locking => *Better syntax highlighting for Clojure code*
- aggressive-indent => *Indenting code as you type*
- paredit => *Treats s-expressions as blocks for easier navigation/editing*
- rainbow-delimiters => *Sets the same color for matching parentheses. Icing on the cake :)*

## Expand your horizon
 If you are looking to expand your emacs configuration, take a take a look at the
 configuration of our fellow ensonians:

 - https://github.com/kitallis/dotemacs
 - https://github.com/kirang89/.emacs.d
 - https://github.com/samrat/dotemacs
