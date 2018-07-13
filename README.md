## dotemacs

Fork of: https://gitlab.com/nilenso/dotemacs

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
- smex => *M-x enhancement*
- projectile => *Project interaction - finding files / grepping through projects etc.*
- magit => *Git inside emacs*
- go-mode => *Major mode for Golang - syntax highlighting / gofmt etc.*
- gotest => *Run Golang tests from within emacs*
- undo-tree => *Visualize or walk undo/redo trees*
- flycheck => *Syntax checking for tons of languages*
- monokai => *Dark and fruity color theme*
- spaceline => *Better Modeline - the popular spacemacs powerline in regular emacs (theme)
- neotree => *Side-pane file browser - NerdTree (ViM) like file browser with icons

## Recommended side-installs

- get [all-the-icons](https://github.com/domtronn/all-the-icons.el) for a great neotree experience