(add-to-list 'load-path "~/.emacs.d/lisp")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "99c86852decaeb0c6f51ce8bd46e4906a4f28ab4c5b201bdc3fdf85b24f88518" "527df6ab42b54d2e5f4eec8b091bd79b2fa9a1da38f5addd297d1c91aa19b616" default)))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Straight package manager
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

;; Nix/NixOS
(use-package nix-mode)

;; Haskell
(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

;; OCaml
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

;; Golang
(use-package go-mode
  :config
  (setenv "GOPATH" (concat (getenv "HOME") "/programming/go" ":" (getenv "HOME") "/programming/titan/gocode"))
  (add-to-list 'exec-path (concat (getenv "HOME") "/programming/go/bin") t)
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

;; Julia
(use-package julia-mode)

;; Latex
(use-package auctex
  :defer t
  :ensure t
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

;; LSP
(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c")
  :commands (lsp lsp-deferred)
  :hook
  (go-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setq lsp-gopls-env
        (list (cons "GOPATH" (getenv "GOPATH")) (cons "GO111MODULE" "on")))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection
                                     (lambda() (cons "/home/ross/go/bin/gopls"
                                                     lsp-gopls-server-args)))
                    :major-modes '(go-mode go-dot-mod-mode)
                    :priority 10
                    :remote? t
                    :server-id 'gopls-remote)))
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil))
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package direnv
  :config
  (direnv-mode))
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-copy-env "GOOGLE_APPLICATION_CREDENTIALS")
  (exec-path-from-shell-copy-env "GOPATH"))

(use-package vterm)
(use-package vterm-toggle
  :bind
  ("M-<return>" . vterm-toggle)
  ("S-M-<return>" . vterm-toggle-cd))

(use-package which-key :config (which-key-mode))

(use-package pdf-tools
  :config
  (pdf-tools-install))
(use-package transpose-frame)
(use-package smooth-scrolling
        :config
        (smooth-scrolling-mode t))
(use-package sudo-edit)
(use-package base16-theme
  :config
  ;; Try to load a theme from env variable
  (load-theme 'base16-solarized-light t))
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
   ;; ("<C-tab>" . counsel-company)
   ;; ("C-c k" . counsel-ag)
   ))
(use-package magit
  :bind
  (("C-x g" . magit-status)))
(use-package company ;; Autocomplete
  :config
  (global-company-mode t)
  (setq company-show-numbers 't)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  :bind
  ("<C-tab>" . company-complete))
(use-package ivy-prescient
  :config
  (ivy-prescient-mode t)
  (prescient-persist-mode t))
(use-package flycheck)
;; (use-package elpy
;;   :init
;;   (elpy-enable))

(use-package jupyter
  :bind
  (("C-c C-e" . jupyter-eval-line-or-region)
   ("C-c C-r" . jupyter-eval-region)
   ("C-c C-b" . jupyter-eval-buffer)
   ("C-c C-f" . jupyter-eval-defun))
  :config
  (setq jupyter-repl-echo-eval-p t))
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
(setq dired-listing-switches "-alh")
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(global-set-key (kbd "M-SPC") 'cycle-spacing)
(show-paren-mode t)
(electric-pair-mode t) ; auto close parens
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Tramp config
(setq tramp-auto-save-directory "~/.emacs.d/var/tramp/")
(setq tramp-chunksize 2000)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
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
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      '(("Home"
         ("Programming" (mode . go-mode))
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
                     (name . "\*info\*"))))))
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
