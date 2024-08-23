;;; init.el -*- lexical-binding: t; -*-

;; -----------------------------------------------------------------------------
;;; Intro
;; -----------------------------------------------------------------------------

;; A quick guide to use-package:
;; (use-package foo
;;   :bind (("M-s O" . foo-do-something)   ; Bind globally
;;
;;          ; Bind in mode map (can be another package's mode)
;;          :map foo-mode-map
;;
;;          ("M-o" . foo-do-smth-in-foo-mode)
;;          ("M-O" . foo-do-smth-else)
;;
;;          ; Remap the key binding of fill-paragraph to foo-fill
;;          ([remap fill-paragraph] . foo-fill))
;;
;;   :hook (python-base-mode . foo-mode)   ; add foo-mode to python-base-mode-hook
;;   :init                                 ; Executes before package is loaded
;;   (setq foo-init t)
;;   :config                               ; Executes after package is loaded
;;   (foo-mode 1))


;; Keywords which trigger deferred loading are: ':hook', ‘:commands’, ‘:bind’,
;; ‘:bind*’, ‘:bind-keymap’, ‘:bind-keymap*’, ‘:mode’, and ‘:interpreter’

;; -----------------------------------------------------------------------------
;;; GC Settings
;; -----------------------------------------------------------------------------

(setq garbage-collection-messages t)
(setq gc-cons-threshold (* 100 1024 1024))
;; NOTE: this seems to fire GC excessively when using TRAMP
;; Run GC whenever Emacs loses focus
;; https://news.ycombinator.com/item?id=39191012
;; (add-function :after
;;               after-focus-change-function
;;               (lambda () (unless (frame-focus-state) (garbage-collect))))


;; -----------------------------------------------------------------------------
;;; Elpaca package manager
;; -----------------------------------------------------------------------------

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
        ;; Enable Elpaca support for use-package's :ensure keyword.
        (elpaca-use-package-mode))

;; Necessary to use the Elpaca's `:ensure` support after this point
(elpaca-wait)

;; Add :ensure t by default. If using built-in packages, set :ensure nil
(use-package use-package
  :ensure nil
  :demand t
  :init (setq use-package-always-ensure t))


;; -----------------------------------------------------------------------------
;;; Built-in Packages
;; -----------------------------------------------------------------------------

(use-package emacs
  :ensure nil
  :demand t
  :config
  ;; Set font to use the system monospace, and make sure it's used for
  ;; mathematical symbols as well (necessary for BQN).
  (add-to-list 'default-frame-alist
               '(font . "monospace-12:weight=light"))
  (set-fontset-font "fontset-default" 'mathematical "monospace")

  (setq user-full-name "Ross Viljoen"
        user-mail-address "ross@viljoen.co.uk")

  (setq inhibit-splash-screen t)
  (setq inhibit-startup-echo-area-message t)
  (setq inhibit-startup-message t)
  (setq initial-scratch-message nil)

  (defalias 'yes-or-no-p 'y-or-n-p)

  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))

  (delete-selection-mode 1)
  (electric-pair-mode 1) ; auto close parens

  (setq native-comp-async-report-warnings-errors nil)

  ;; Better-defaults
  ;; Mostly copied from https://github.com/technomancy/better-defaults
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)

  ;; Fill column settings
  (setq-default fill-column 80)
  (global-display-fill-column-indicator-mode)
  (remove-hook 'text-mode-hook #'auto-fill-mode)
  (add-hook 'message-mode-hook #'word-wrap-mode)
  (add-hook 'text-mode-hook #'visual-line-mode)

  ;; https://www.emacswiki.org/emacs/SavePlace
  (save-place-mode 1)

  (show-paren-mode 1)

  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)

  (setq save-interprogram-paste-before-kill t
        apropos-do-all t
        mouse-yank-at-point t
        require-final-newline t
        visible-bell t
        load-prefer-newer t
        backup-by-copying t
        frame-inhibit-implied-resize t
        ediff-window-setup-function 'ediff-setup-windows-plain
        custom-file (expand-file-name "custom.el" user-emacs-directory))

  :bind
  ("M-SPC" . cycle-spacing)

  ("M-/" . hippie-expand)
  ("M-z" . zap-up-to-char)
  
  ("C-s" . isearch-forward-regexp)
  ("C-r" . isearch-backward-regexp)
  ("C-M-s" . isearch-forward)
  ("C-M-r" . 'isearch-backward))


(use-package uniquify
  ;; Disambiguate buffer names with "bar/mumble/name"; "quux/mumble/name"
  ;; instead of "name"; "name<2>".
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward))


(use-package recentf
  :ensure nil
  :config
  (recentf-mode 1)
  ; Periodically save recentf list (5 mins)
  (run-at-time nil (* 5 60) 'recentf-save-list)) 


(use-package dired
  :ensure nil
  :demand t
  :config
  (setq dired-listing-switches "-aBhl  --group-directories-first")
  (setq dired-dwim-target t)
  (setq dired-recursive-copies (quote always))
  (setq dired-recursive-deletes (quote top))
  (setq dired-isearch-filenames t)
  (use-package dired-x :ensure nil)
  :bind (:map dired-mode-map
              ("M-s f" . consult-recent-file)))


(use-package tramp
  :ensure nil
  :config
  (setq tramp-auto-save-directory
        (expand-file-name "var/tramp/" user-emacs-directory))
  (setq tramp-chunksize 2000)
  ;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; (add-to-list 'tramp-connection-properties
               ;; (list nil "direct-async-process" t))
  (setq tramp-ssh-controlmaster-options nil)
        ;; (concat
        ;;  "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
        ;;  "-o ControlMaster=auto -o ControlPersist=60m"))
  (connection-local-set-profile-variables
   'remote-bash
   '((shell-file-name . "/bin/bash")
     (shell-command-switch . "-c")
     (shell-interactive-switch . "-i")
     (shell-login-switch . "-l"))))


(use-package ibuffer
  :ensure nil
  :bind
  ("C-x C-b" . ibuffer)
  :hook
  (ibuffer-mode . (lambda ()
                    (ibuffer-auto-mode 1)
                    (ibuffer-switch-to-saved-filter-groups "Home")))
  :config
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-saved-filter-groups
        ;; TODO: group by project?
        '(("Home"
           ("Programming" (mode . prog-mode))
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
                       (name . "\*info\*")))))))


(use-package org
  :ensure nil
  :hook
  (org-mode . org-indent-mode)
  :config
  (setq org-directory "~/org/"))


(use-package display-line-numbers
  :ensure nil
  :hook
  (text-mode . display-line-numbers-mode)
  (prog-mode . display-line-numbers-mode)
  :config
  (setq display-line-numbers-type t))


(use-package proced
  :ensure nil
  :config
  (setq proced-auto-update-flag t)
  (setq proced-auto-update-interval 3)
  (setq proced-enable-color-flag t)
  (setq proced-show-remote-processes t))


(use-package re-builder
  :ensure nil
  :config
  (setq reb-re-syntax 'string))


(use-package eglot
  :ensure nil                           ; Use the builtin eglot
  ;; :hook
  ;; (python-base-mode . eglot-ensure)
  ;; (julia-mode . eglot-ensure)
  )


;; -----------------------------------------------------------------------------
;;; General Packages and Utilities
;; -----------------------------------------------------------------------------

;; TODO: terminal emacs setup
;; term-keys, mosh, wezterm

;; TODO: setup bindings
(use-package helpful)


(use-package ace-window
  :bind ("M-o" . ace-window)
  :config (ace-window-display-mode))


(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          eat-mode
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints


;; Initialise emacs PATH from shell PATH
(use-package exec-path-from-shell
  :demand t
  :config
  (exec-path-from-shell-initialize))


;; Needed for GPG passphrase entry when signing magit commits
(use-package pinentry
  :demand t
  :config
  (pinentry-start))


(use-package solarized-theme
  :demand t
  :config
  (load-theme 'solarized-light t))


(use-package smooth-scrolling
  :demand t
  :config
  (smooth-scrolling-mode 1))


(use-package sudo-edit)


(use-package pdf-tools
  :init (pdf-tools-install))


(use-package transient
  ;; Magit sometimes requires a more recent version of transient than the
  ;; built-in, so just install it anyway.
  ;; https://github.com/progfolio/elpaca/issues/302
  :ensure t)


(use-package magit
  :bind ("C-x g" . magit-status))


(use-package diff-hl
  :config
  (diff-hl-margin-mode)
  (global-diff-hl-mode))


(use-package eat
  :custom
  (eat-kill-buffer-on-exit t)
  :config
  (setq eat-shell "TERM=xterm-256color /bin/bash")
  (setq eat-tramp-shells '(("ssh" . "TERM=xterm-256color /bin/bash") ("docker" . "/bin/sh")))
  (delete [?\C-u] eat-semi-char-non-bound-keys) ; make C-u work in Eat terminals like in normal terminals
  (delete [?\C-g] eat-semi-char-non-bound-keys) ; ditto for C-g
  (add-to-list 'eat-semi-char-non-bound-keys [?\e ?\o])
  (add-to-list 'eat-semi-char-non-bound-keys [?\e ?\`])
  (eat-update-semi-char-mode-map)
  (eat-reload))


(use-package persistent-scratch
  :ensure t
  :commands persistent-scratch-setup-default
  :hook (after-init . persistent-scratch-setup-default))


(use-package jupyter
  :init (setq jupyter-repl-echo-eval-p t)
  :bind (:map jupyter-repl-interaction-mode-map
              (("C-c C-e" . jupyter-eval-line-or-region)
               ("C-c C-r" . jupyter-eval-region)
               ("C-c C-b" . jupyter-eval-buffer)
               ("C-c C-f" . jupyter-eval-defun))
              :map jupyter-repl-mode-map
              ;; This makes latex input work in the jupyter repl (should probably
              ;; make this Julia specific though)
              (("TAB" . julia-latexsub-or-indent)
               ;; ("C-c C-z" . jupyter-repl--switch-back)
               ))
  :config
  (setq org-babel-jupyter-resource-directory (concat user-emacs-directory "jupyter"))

  ;; Enable jumping back to the buffer from which jupyter-repl-pop-to-buffer was
  ;; called.
  ;; TODO: get this to work when starting a new REPL as well?
  (defvar-local jupyter-repl--script-buffer nil)
  (defun jupyter-repl-pop-to-buffer-with-save ()
    "Switch to the REPL buffer of the `jupyter-current-client' and
save the script buffer."
    (interactive)
    (if jupyter-current-client
        (let ((script-buffer (current-buffer)))
          (jupyter-with-repl-buffer jupyter-current-client
                                    (setq jupyter-repl--script-buffer script-buffer)
                                    (goto-char (point-max))
                                    (pop-to-buffer (current-buffer))))
      (error "Buffer not associated with a REPL, see `jupyter-repl-associate-buffer'")))

  (defun jupyter-repl--switch-back ()
    "Switch to the buffer that was active before last call to
`jupyter-repl-pop-to-buffer-with-save'."
    (interactive)
    (when (buffer-live-p jupyter-repl--script-buffer)
      (switch-to-buffer-other-window jupyter-repl--script-buffer)))

  (advice-add 'jupyter-repl-pop-to-buffer
              :override #'jupyter-repl-pop-to-buffer-with-save)

  ;; ;; This doesn't work, idk why
  ;; ;; MAYBE: this is probably a cleaner approach if I can get it to work?
  ;; (defun jupyter-save-source-buffer (orig-fun &rest args)
  ;;   (let ((script-buffer (current-buffer)))
  ;;     (apply orig-fun args)
  ;;     (setq jupyter-repl--script-buffer script-buffer)))
  ;; (advice-add 'jupyter-repl-pop-to-buffer :around #'jupyter-save-source-buffer)
  
  :hook
  (jupyter-repl-mode . (lambda ()
                         (define-key jupyter-repl-mode-map
                                     [remap jupyter-repl-pop-to-buffer]
                                     'jupyter-repl--switch-back))))


(use-package code-cells
  :ensure (:host github :repo "astoff/code-cells.el" :branch "master")
  :hook ((julia-mode python-base-mode) . code-cells-mode)
  :config
  (defun rv/julia-repl-send-region (start end)
    (julia-repl--send-string
     (buffer-substring-no-properties start end)))
  (let ((map code-cells-mode-map))
    (define-key map (kbd "M-p") 'code-cells-backward-cell)
    (define-key map (kbd "M-n") 'code-cells-forward-cell)
    (define-key map (kbd "C-c C-SPC") 'code-cells-mark-cell)
    (define-key map (kbd "C-c C-w") (code-cells-command 'kill-region :use-region))
    (define-key map (kbd "C-c M-w") (code-cells-command 'kill-ring-save :use-region))
    (define-key map [remap python-shell-send-region]
                (code-cells-command 'python-shell-send-region :use-region :pulse))
    (define-key map [remap jupyter-eval-line-or-region]
                (code-cells-command 'jupyter-eval-region :use-region :pulse))
    ;; (define-key map [remap julia-repl-send-region-or-line]
    ;; (code-cells-command 'julia-repl-send-region-or-line :use-region :pulse))
    (define-key map [remap julia-repl-send-region-or-line]
                (code-cells-command 'rv/julia-repl-send-region :use-region :pulse))))


(use-package epithet
  :ensure (:host github :repo "oantolin/epithet")
  ;; use 'epithet-rename-buffer' to rename a buffer
  ;; MAYBE: add buffer-file-name to epithet-suggesters?
  
  ;; Always auto-rename a few modes
  :hook
  ((Info-selection eww-after-render help-mode occur-mode shell-mode)
   . epithet-rename-buffer)
  ((compilation-start compilation-finish)
   . epithet-rename-buffer-ignoring-arguments))


(use-package breadcrumb :config (breadcrumb-mode))


(use-package hl-todo
  ;; NOTE: 'consult-todo' (see below) lists all TODOs in buffer
  :demand t
  ;; https://github.com/progfolio/elpaca/wiki/Warnings-and-Errors
  :ensure (:depth nil)                  ; Needed to get version info
  :config (global-hl-todo-mode 1))


(use-package which-key :config (which-key-mode 1))


(use-package wgrep)  ;; editable grep buffers


(use-package hide-mode-line
  ;; Hide the modeline for inferior python processes
  :hook (inferior-python-mode . hide-mode-line-mode))


;; Taken from: https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)


(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)

  (setq bqn-tsauto-config
      (make-treesit-auto-recipe
       :lang 'bqn
       :ts-mode 'bqn-ts-mode
       :remap '(bqn-mode)
       :url "https://github.com/shnarazk/tree-sitter-bqn"
       :revision "master"
       :source-dir "src"
       :ext "\\.bqn\\'"))
  (add-to-list 'treesit-auto-recipe-list bqn-tsauto-config)

  (delete 'yaml treesit-auto-langs)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode 1))


;; -----------------------------------------------------------------------------
;;; Minibuffer Completion
;; -----------------------------------------------------------------------------


(use-package vertico
  ;; Minimalistic completion UI
  :demand t
  :config
  (vertico-mode 1)

  ;; A few more useful configurations...
  ;; taken from https://github.com/minad/vertico
  (setq enable-recursive-minibuffers t) ; allow minibuffer commands while in minibuffer


  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  :bind (("C-<return>" . vertico-exit-input))) ; insert exact text without completion


(use-package orderless
  ;; Defines a completion style where space-separated components can
  ;; be matched in any order.
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init (marginalia-mode))


(use-package consult
  ;;     Search and navigation based on 'completing-read'.
  ;;     Essentially, allows other emacs commands to use the better
  ;;     minibuffer completion from above.
  ;;     NB: 'M-n' usually inserts the thing at point into the
  ;;     minibuffer
  ;;     Provides fast previews
  
  ;; Replace bindings. Lazily loaded due to `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s f" . consult-recent-file)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; consult--source-buffer :hidden t :default nil  ; uncomment if using perspective.el
   ;; consult--source-buffer :default nil
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; (add-to-list 'consult-buffer-sources persp-consult-source)  ; uncomment if using perspective.el

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )


(use-package consult-todo
  ;; Search TODOs detected by 'hl-todo' with consult
  :bind ("M-s t" . consult-todo))


(use-package embark
  ;; https://karthinks.com/software/fifteen-ways-to-use-embark/
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;;;; Corfu
;;   =====
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0)
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; CONFIGURE handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode 1))


;; -----------------------------------------------------------------------------
;;; Programming Language Modes
;; -----------------------------------------------------------------------------

;;;; OCaml
;;   =====

(let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin and Tuareg
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    (load (expand-file-name "emacs/site-lisp/tuareg-site-file" opam-share))
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; ;; Use opam switch to lookup ocamlmerlin binary
    ;; (setq merlin-command 'opam)
    (setq merlin-command "/home/ross/.opam/default/bin/ocamlmerlin") ; TODO: shouldn't be necessary?
    ;; To easily change opam switches within a given Emacs session, you can
    ;; install the minor mode https://github.com/ProofGeneral/opam-switch-mode
    ;; and use one of its "OPSW" menus.
    ))

(use-package dune)

(use-package merlin-eldoc)

(use-package utop
  :config
  (add-hook 'tuareg-mode-hook #'utop-minor-mode)
  (setq utop-command "opam exec -- dune utop . -- -emacs"))

;; https://opam.ocaml.org/doc/Tricks.html
(defun opam-env ()
  (interactive nil)
  (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var))))
(if (executable-find "opam") (opam-env))


;;;; Julia
;;   =====

(use-package julia-mode)

;; https://github.com/JuliaEditorSupport/julia-ts-mode/issues/21#issuecomment-2126885445
(use-package julia-ts-mode)

(use-package julia-repl
  :config
  (setq julia-repl-switches "--project=@.") ;; Activate first parent project found
  (julia-repl-set-terminal-backend 'eat)
  :bind (:map julia-repl-mode-map ("C-c C-e" . nil))
  :hook julia-mode)

(use-package eglot-jl
  ;; N.B. need to downgrade the installed LanguageServer version to 4.4 as 4.5
  ;; is broken for eglot. Located at eglot-jl-language-server
  :config (eglot-jl-init))

;; MAYBE: Julia snail

;;;; Python
;;   ======

(use-package python
  :ensure nil
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset t)
  :hook (python-base-mode . (lambda () (setq tab-width 4))))

(use-package pet
  ;; Finds the correct tool executables in the venv (pylsp etc.)
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(defun ross/eglot-workspace-config (server)
  (if (equal "pylsp" (plist-get (eglot--server-info server) :name))
      ;; Broken for python 3.12
      (list :pylsp (list :plugins (list :jedi_signature_help (list :enabled nil))))))
(setq-default eglot-workspace-configuration #'ross/eglot-workspace-config)

;; ;; <OPTIONAL> Numpy style docstring for Python.  See:
;; ;; https://github.com/douglasdavis/numpydoc.el.  There are other packages
;; ;; available for docstrings, see: https://github.com/naiquevin/sphinx-doc.el
(use-package numpydoc
  :config
  (setq numpydoc-insert-examples-block nil)
  (setq numpydoc-template-long nil)
  :bind (:map python-mode-map
              ("C-c C-n" . numpydoc-generate)))


;;;; Javascript/JSX/Typescript
;;   =========================
;; `npm install -g typescript-language-server'

(use-package js
  :ensure nil
  :bind (([remap js-find-symbol] . xref-find-definitions)))

(use-package flymake-eslint
  :config (setq flymake-eslint-prefer-json-diagnostics t)
  :hook (js-base-mode . flymake-eslint-enable))


;;;; Misc
;;   ====

(use-package bqn-mode
  :demand t
  :init
  (use-package bqn-keymap-mode :ensure nil)
  (use-package bqn-glyph-mode  :ensure nil)
  :config
  (defun run-or-switch-to-bqn ()
    "Run and switch to inferior BQN process if it does not
 already exist. Otherwise switch to the already existing process
 buffer."
    (interactive)
    (pop-to-buffer (bqn-comint-buffer)))

  :bind
  (:map bqn-mode-map
        ("C-c C-z" . run-or-switch-to-bqn)
        ("C-c C-c" . bqn-comint-send-buffer)
        ("C-c C-r" . bqn-comint-send-region)
        ("C-c C-e" . bqn-comint-send-dwim)))


(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc"))


(when (executable-find "agda-mode")
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate")))
  (setq auto-mode-alist
        (append
         '(("\\.agda\\'" . agda2-mode)
           ("\\.lagda.md\\'" . agda2-mode))
         auto-mode-alist)))


(use-package yaml-mode)

(use-package csv-mode)


;; -----------------------------------------------------------------------------
;;; Structural Editing
;; -----------------------------------------------------------------------------

(use-package expreg
  :bind
  (("C-=" . expreg-expand)
   ("C--" . expreg-contract)))


(use-package combobulate
  :ensure
  (:host github :repo "rossviljoen/combobulate" :branch "julia")
  :demand t
  :hook
  (
   (julia-ts-mode . combobulate-mode)
   ;; (python-ts-mode . combobulate-mode)
   ;; (js-ts-mode . combobulate-mode)
   ;; (css-ts-mode . combobulate-mode)
   ;; (yaml-ts-mode . combobulate-mode)
   ;; (json-ts-mode . combobulate-mode)
   ;; (typescript-ts-mode . combobulate-mode)
   ;; (tsx-ts-mode . combobulate-mode)
   ))

;; https://karthinks.com/software/a-consistent-structural-editing-interface/
;; https://karthinks.com/software/it-bears-repeating/
;; https://karthinks.com/software/persistent-prefix-keymaps-in-emacs/
;; https://github.com/mickeynp/combobulate
;; https://github.com/ethan-leba/tree-edit


;; -----------------------------------------------------------------------------
;;; Messaging
;; -----------------------------------------------------------------------------


(use-package elfeed
  ;; RSS feed reader
  :config (setq elfeed-feeds
              '(("https://codingquark.com/feed.xml" emacs)
                ("https://ag91.github.io/rss.xml" emacs)
                ("https://sqrtminusone.xyz/posts/index.xml" emacs)
                ("http://verisimilitudes.net/rss.xml" programming)
                ("http://ngnghm.github.io/feeds/all.rss.xml" programming)
                ("http://www.loper-os.org/?feed=rss" programming)
                ("https://mmapped.blog/feed.xml" programming)
                ("https://hbfs.wordpress.com/feed/" comp-sci)))
  (setq elfeed-db-directory (expand-file-name "elfeed/" user-emacs-directory)))


;; -----------------------------------------------------------------------------
;;; TODO
;; -----------------------------------------------------------------------------


;; dragstuff
;; better comment-lines that doesn't move cursor (save-excursion or smth?)

;; Workspaces?
;; (use-package perspective
;;   :bind (("C-x k" . persp-kill-buffer*))
;;   :custom
;;   (persp-mode-prefix-key (kbd "C-x x"))
;;   :init
;;   (persp-mode))

;; Something like khoj.el but less zogged

;; https://github.com/jart/emacs-copilot

;; Actually understand:
;;  - corfu
;;  - consult


;; https://github.com/karthink/popper


;; -----------------------------------------------------------------------------
;; Final Settings
;; -----------------------------------------------------------------------------

;; Saves minibuffer history, has to be loaded after some packages like
;; Combobulate
(savehist-mode 1)
(put 'downcase-region 'disabled nil)
