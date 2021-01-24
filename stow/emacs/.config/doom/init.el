;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       company           ; the ultimate code completion backend
       ivy               ; a search engine for love and life

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       fill-column       ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ligatures         ; ligatures and symbols to make your code pretty again
       modeline          ; snazzy, Atom-inspired modeline, plus API
       ophints           ; highlight the region an operation acts on
       (popup +all +defaults)   ; tame sudden yet inevitable temporary windows
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       workspaces        ; tab emulation, persistence & separate workspaces

       :editor
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format +onsave)  ; automated prettiness
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ibuffer         ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       ;;spell             ; tasing you for misspelling mispelling

       :tools
       ;;debugger          ; FIXME stepping through code, to help you add bugs
       ;;direnv
       (eval +overlay)     ; run code, run (also, repls)
       lookup              ; navigate your code and its documentation
       lsp
       magit             ; a git porcelain for Emacs
       pdf               ; pdf enhancements
       rgb               ; creating color strings

       :lang
       data              ; config/data formats
       emacs-lisp        ; drown in parentheses
       haskell
       json              ; At least it ain't XML
       (julia +lsp)             ; a better, faster MATLAB
       (latex +latexmk +fold)   ; writing papers in Emacs has never been so fun
       markdown          ; writing docs for people to ignore
       nix               ; I hereby declare "nix geht mehr!"
       ocaml             ; an objective camel
       org               ; organize your plain life in plain text
       python            ; beautiful is better than ugly
       sh                ; she sells {ba,z,fi}sh shells on the C xor
       web               ; the tubes

       :config
       (default +bindings +smartparens))
