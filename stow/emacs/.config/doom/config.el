;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Ross Viljoen"
      user-mail-address "ross@viljoen.co.uk"
      doom-scratch-intial-major-mode 'lisp-interaction-mode
      doom-theme 'doom-solarized-light
      display-line-numbers-type t
      load-prefer-newer t
      company-idle-delay nil)

(setq org-directory "~/org/")

(after! dired
  (setq dired-listing-switches "-aBhl  --group-directories-first"
        dired-dwim-target t
        dired-recursive-copies (quote always)
        dired-recursive-deletes (quote top)))

(map! "C-x C-j" #'dired-jump)

(remove-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'message-mode-hook #'word-wrap-mode)
