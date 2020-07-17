(add-to-list 'load-path "~/.emacs.d/lisp")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("99c86852decaeb0c6f51ce8bd46e4906a4f28ab4c5b201bdc3fdf85b24f88518" "527df6ab42b54d2e5f4eec8b091bd79b2fa9a1da38f5addd297d1c91aa19b616" default)))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq straight-use-package-by-default t)

;; Bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; (use-package better-defaults
;;   :config
;;   (ido-mode nil))
(use-package pdf-tools
  :config
  (pdf-tools-install))
(use-package auctex
  :defer t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "PDF Tools")
     (output-html "xdg-open"))))
  (setq-default TeX-master nil)
   ;; revert pdf-view after compilation TODO: I don't think this works
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))
(use-package transpose-frame)
(use-package smooth-scrolling
	     :config
	     (smooth-scrolling-mode t))

;; Nix/NixOS development
(use-package nix-mode)
;; (use-package direnv
;;   :config
;;   (direnv-mode))

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

;; OCaml development
(use-package tuareg
  :config
  (setq tuareg-prettify-symbols-full t)
  (add-hook 'tuareg-mode-hook #'prettify-symbols-mode))
(use-package merlin
  :hook (tuareg-mode . merlin-mode)
  :config
  (setq merlin-command "ocamlmerlin"))
(use-package ocp-indent)
(use-package utop
  :config
  (add-hook 'tuareg-mode-hook 'utop-minor-mode)
  (setq utop-command "dune utop . -- -emacs")
  (defun utop--supports-company ()      ; Fixes dune utop not using company
    (featurep 'company)))

;; Go development
(use-package go-mode
  :config
  (setenv "GOPATH" (concat (getenv "HOME") "/programming/go"))
  (add-to-list 'exec-path (concat (file-name-as-directory (getenv "GOPATH")) "bin") t)
  (add-hook 'go-mode-hook (lambda ()
                            (which-function-mode)
                            (add-hook 'before-save-hook 'gofmt-before-save)))
  )

(use-package sudo-edit)
(use-package base16-theme
  :config
  (load-theme (intern (or (getenv "THEME") "base16-solarized-light")) t)) ;; Try to load a theme from env variable
(use-package visual-fill-column)
(use-package speed-type)
(use-package counsel
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (add-to-list 'ivy-display-functions-alist '(counsel-company . ivy-display-function-overlay))
  :bind
  (("C-s" . swiper)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x f" . counsel-find-file)
   ("C-x C-d" . counsel-dired)
   ("C-x d" . counsel-dired)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-find-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ("C-c g" . counsel-git)
   ("C-c c" . counsel-compile)
   ("C-c j" . counsel-git-grep)
   ("C-c b" . counsel-bookmark)
   ("C-c L" . counsel-git-log)
   ("C-x l" . counsel-locate)
   ("C-S-o" . counsel-rhythmbox)
   ("<C-tab>" . counsel-company)
   ;; ("C-c k" . counsel-ag)
   ))
(use-package magit
  :bind
  (("C-x g" . magit-status)))
;; (use-package frames-only-mode
;;   :config
;;   (frames-only-mode t))
(use-package company ;; Autocomplete
  :config
  (global-company-mode t)
  (setq company-show-numbers 't)
  ;; :hook
  ;; (nix-mode . (lambda() (setq-local company-backends (company-lsp :with company-dabbrev :with company-yasnippet :with company-files))))
  )
(use-package ivy-prescient
  :config
  (ivy-prescient-mode t)
  (prescient-persist-mode t))
;; (use-package company-nixos-options ;; Seems pretty broken
;;   :config
;;   (add-to-list 'company-backends 'company-nixos-options))
;; (use-package flycheck)
;; (use-package elpy
;;   :init
;;   (elpy-enable))
(use-package julia-mode)
(use-package jupyter
  :bind
  (("C-c C-e" . jupyter-eval-line-or-region)
   ("C-c C-r" . jupyter-eval-region)
   ("C-c C-b" . jupyter-eval-buffer)
   ("C-c C-f" . jupyter-eval-defun)))
(use-package rainbow-mode)
(use-package native-complete
  :init
  (with-eval-after-load 'shell (native-complete-setup-bash)))

(require 'jupyter)
(require 'dired-x)

;; Set better defaults
(setq user-full-name "Ross Viljoen"
      user-mail-address "ross@viljoen.co.uk")
(setq gc-cons-threshold (* 100 1024 1024))
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(delete-selection-mode +1)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq dired-dwim-target t)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(global-set-key (kbd "M-SPC") 'cycle-spacing)
(show-paren-mode t)
(electric-pair-mode t) ; auto close parens
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(connection-local-set-profile-variables
 'remote-bash
 '((shell-file-name . "/bin/bash")
   (shell-command-switch . "-c")
   (shell-interactive-switch . "-i")
   (shell-login-switch . "-l")))

(connection-local-set-profiles
 '(:application tramp :protocol "ssh" :machine "dfly")
 'remote-bash)

;; Ibuffer configuration
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      '(("Home"
         ("Dired" (mode . dired-mode))
         ("Org" (or (name . "^.*org$")
                    (mode . org-mode)))
         ("Shell" (or (mode . eshell-mode) (mode . shell-mode)))
         ("Emacs" (or
                   (name . "^\\*scratch\\*$")
                   (name . "^\\*Messages\\*$")))
         ("Magit" (or (name . "^magit*") (mode . magit-mode)))
         ("Help" (or (name . "\*Help\*")
                     (name . "\*Apropos\*")
                     (name . "\*info\*")))
         )))
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-switch-to-saved-filter-groups "Home")))

(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(add-hook 'text-mode-hook #'visual-line-mode)

;; TODO: use advice/some other method?
;; Treat [] as an indentation level in TeX mode
(defun TeX-brace-count-line ()
  "Count number of open/closed braces."
  (save-excursion
    (let ((count 0) (limit (line-end-position)) char)
      (while (progn
               (skip-chars-forward "^{}[]\\\\" limit)
               (when (and (< (point) limit) (not (TeX-in-comment)))
                 (setq char (char-after))
                 (forward-char)
                 (cond ((eq char ?\{)
                        (setq count (+ count TeX-brace-indent-level)))
                       ((eq char ?\})
                        (setq count (- count TeX-brace-indent-level)))
                       ((eq char ?\[)
                        (setq count (+ count TeX-brace-indent-level)))
                       ((eq char ?\])
                        (setq count (- count TeX-brace-indent-level)))
                       ((eq char ?\\)
                        (when (< (point) limit)
                          (forward-char)
                          t))))))
      count)))
