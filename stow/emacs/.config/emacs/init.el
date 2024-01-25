;;; init.el --- Description -*- lexical-binding: t; -*-

;; ---------------------------------------------------------------------------------------
;;; Straight.el package manager setup
;; ---------------------------------------------------------------------------------------

(setq straight-use-package-by-default t)
;; Bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)


;; if we're in guix, some packages need to be installed by guix, not straight
(defvar is-guix-system (and (eq system-type 'gnu/linux)
			                (file-exists-p "/etc/os-release")
                            (with-temp-buffer
                              (insert-file-contents "/etc/os-release")
                              (search-forward "ID=guix" nil t))
                            t))

;; TODO: not sure this should be necessary?
;; (if is-guix-system
    ;; (add-to-list 'load-path "/home/ross/.guix-profile/share/emacs/site-lisp"))
;; (load "subdirs")

;; (load-file (expand-file-name "exwm-config.el" user-emacs-directory))

;; ---------------------------------------------------------------------------------------
;;; Built-in Settings
;; ---------------------------------------------------------------------------------------

(setq gc-cons-threshold (* 100 1024 1024))

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

(remove-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'message-mode-hook #'word-wrap-mode)
(add-hook 'text-mode-hook #'visual-line-mode)

(global-set-key (kbd "M-SPC") 'cycle-spacing)

(delete-selection-mode +1)
(electric-pair-mode t) ; auto close parens

(setq native-comp-async-report-warnings-errors nil)

;;;; Better-defaults
;;   ===============
;;     Mostly copied from https://github.com/technomancy/better-defaults
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; https://www.emacswiki.org/emacs/SavePlace
(save-place-mode 1)

(recentf-mode 1)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(savehist-mode 1)
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

;;;; Dired
;;   =====
(setq dired-listing-switches "-aBhl  --group-directories-first"
      dired-dwim-target t
      dired-recursive-copies (quote always)
      dired-recursive-deletes (quote top))
(require 'dired-x)

;;;; Tramp
;;   =====
(setq tramp-auto-save-directory (expand-file-name "var/tramp/" user-emacs-directory))  ;; TODO: change to parameter
(setq tramp-chunksize 2000)
;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(connection-local-set-profile-variables
 'remote-bash
 '((shell-file-name . "/bin/bash")
   (shell-command-switch . "-c")
   (shell-interactive-switch . "-i")
   (shell-login-switch . "-l")))

;;;; Ibuffer
;;   =======
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


;;;; Org-mode
;;   =======
(setq org-directory "~/org/")
(add-hook 'org-mode-hook #'org-indent-mode)


;;;; Line numbers
;;   ============
(require 'display-line-numbers)

;; Exclude some modes from global line numbers
(defcustom display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode jupyter-repl-mode)
  "Major modes on which to disable line numbers."
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "Turn on line numbers except for certain major modes.
Exempt major modes are defined in `display-line-numbers-exempt-modes'."
  (unless (or (minibufferp)
              (member major-mode display-line-numbers-exempt-modes))
    (display-line-numbers-mode)))

(global-display-line-numbers-mode t)
(setq display-line-numbers-type t)

;; ;;;; Exec Path
;; ;;   =========
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))


;; ---------------------------------------------------------------------------------------
;;; General Packages
;; ---------------------------------------------------------------------------------------
(use-package solarized-theme
  :config
  (load-theme 'solarized-light t))

(use-package smooth-scrolling
        :config
        (smooth-scrolling-mode t))

(use-package sudo-edit)

;; ;; TODO: better way to integrate straight & guix?
(if is-guix-system
    (use-package pdf-tools
      :straight (pdf-tools :local-repo "/home/ross/.guix-profile/share/emacs/site-lisp/pdf-tools-1.1.0")
      :config
      (pdf-tools-install))
  (use-package pdf-tools
    :config
    (pdf-tools-install)))


(setq is-guix-system nil)
(use-package magit
  :bind
  (("C-x g" . magit-status)))

(use-package rainbow-mode)


;;;; Vterm
;;   =======
(use-package vterm)
(use-package vterm-toggle
  :bind
  ("M-<return>" . vterm-toggle)
  ("S-M-<return>" . vterm-toggle-cd))


;; ;;;; Perspective
;; ;;   ===========
;; (use-package perspective
;;   :bind (("C-x k" . persp-kill-buffer*))
;;   :custom
;;   (persp-mode-prefix-key (kbd "C-x x"))
;;   :init
;;   (persp-mode))


;;;; Eglot
;;   =======
(use-package eglot
  :hook (python-base-mode . eglot-ensure)
  :config
  ;; make sure eglot works for python-ts-mode
  ;; TODO: make this idempotent
  (setcar (assoc 'python-mode eglot-server-programs) 'python-base-mode))


;;;; Jupyter
;;   =======
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
  :hook
  (jupyter-repl-mode . (lambda () (define-key jupyter-repl-mode-map [remap jupyter-repl-pop-to-buffer] 'jupyter-repl--switch-back))))


;; TODO: get this to work when starting a new REPL as well?
(defvar-local jupyter-repl--script-buffer nil)
(defun jupyter-repl-pop-to-buffer-with-save ()
  "Switch to the REPL buffer of the `jupyter-current-client' and save the script buffer."
  (interactive)
  (if jupyter-current-client
      (let ((script-buffer (current-buffer)))
        (jupyter-with-repl-buffer jupyter-current-client
          (setq jupyter-repl--script-buffer script-buffer)
          (goto-char (point-max))
          (pop-to-buffer (current-buffer))))
    (error "Buffer not associated with a REPL, see `jupyter-repl-associate-buffer'")))

(defun jupyter-repl--switch-back ()
  "Switch to the buffer that was active before last call to `jupyter-repl-pop-to-buffer-with-save'."
  (interactive)
  (when (buffer-live-p jupyter-repl--script-buffer)
    (switch-to-buffer-other-window jupyter-repl--script-buffer)))

(advice-add 'jupyter-repl-pop-to-buffer :override #'jupyter-repl-pop-to-buffer-with-save)

;; ;; This doesn't work, idk why
;; ;; TODO: this is probably a cleaner approach if I can get it to work?
;; (defun jupyter-save-source-buffer (orig-fun &rest args)
;;   (let ((script-buffer (current-buffer)))
;;     (apply orig-fun args)
;;     (setq jupyter-repl--script-buffer script-buffer)))
;; (advice-add 'jupyter-repl-pop-to-buffer :around #'jupyter-save-source-buffer)


;;;; Code-Cells
;;   ==========
(use-package code-cells
  :straight (code-cells :host github :repo "astoff/code-cells.el" :branch "master")
  :hook ((julia-mode python-base-mode) . code-cells-mode)
  :config
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
    ))

;; (use-package epithet
;;   ;; Rename buffers with 'epithet-rename-buffer'
;;   ;; TODO: add buffer-file-name to epithet-suggesters?
  
;;   ;; Always auto-rename a few modes
;;   :hook
;;   ((Info-selection eww-after-render help-mode occur-mode shell-mode compilation-start compilation-finish)
;;    . epithet-rename-buffer))

(use-package hl-todo
  :init (global-hl-todo-mode))  ;; TODO: listing TODOs, jumping to next etc??

(use-package which-key
  :init (which-key-mode))

(use-package wgrep)  ;; editable grep buffers

;; (use-package mood-line
  ;; :init (mood-line-mode))

;; Visually wrap lines at fill-columnx
(use-package visual-fill-column
  :hook
  (visual-line-mode . visual-fill-column-mode))

;; Hide the modeline for inferior python processes.  This is not a necessary
;; package but it's helpful to make better use of the screen real-estate at our
;; disposal. See: https://github.com/hlissner/emacs-hide-mode-line.
(use-package hide-mode-line
  :hook (inferior-python-mode . hide-mode-line-mode))

;; TODO: move to separate file?
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

;;;; Tree-sitter
;;   ===========
;;     Configuration for tree-sitter and related packages
(use-package treesit-auto
  :init (setq treesit-auto-install 'prompt)
  :config (global-treesit-auto-mode))


;;;; Khoj
;;   ====
;;     Natural language search
;; (use-package khoj)


;; ---------------------------------------------------------------------------------------
;;; Completion
;; ---------------------------------------------------------------------------------------

;;;; Vertico
;;   =======
;;     Minimalistic completion UI

(use-package vertico
  :init
  (vertico-mode)
  
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  :bind (("C-<return>" . vertico-exit-input) ;; insert exact text without completion
         ))

;; A few more useful configurations...
;; taken from https://github.com/minad/vertico
(setq enable-recursive-minibuffers t) ;; allow minibuffer commands while in minibuffer

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


;;;; Orderless
;;   =========
;;     Defines a completion style where space-separated components can be matched in any order

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


;;;; Marginalia
;;   ==========
;;     Marginalia in the minibuffer

(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init (marginalia-mode))


;;;; Consult
;;   =======
;;     Search and navigation based on 'completing-read'.
;;     Essentially, allows other emacs commands to use the better
;;     minibuffer completion from above.
;;     NB: 'M-n' usually inserts the thing at point into the minibuffer
;;     Provides fast previews

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
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


;;;; Embark
;;   ======
;;     https://karthinks.com/software/fifteen-ways-to-use-embark/

(use-package embark
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
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
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
  (global-corfu-mode))


;; ---------------------------------------------------------------------------------------
;;; Programming Language Modes
;; ---------------------------------------------------------------------------------------

;;;; OCaml
;;   =====

(let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin and Tuareg
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    (load (expand-file-name "emacs/site-lisp/tuareg-site-file" opam-share))
    ;; (setq merlin-command "/home/ross/.opam/default/bin/ocamlmerlin") ; TODO: shouldn't be necessary?
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
(opam-env)

;;;; Julia
;;   =====
(use-package julia-mode)

(use-package julia-repl
  :config
  (setq julia-repl-switches "--project=@.") ;; Activate first parent project found
  (julia-repl-set-terminal-backend 'vterm)
  :bind (:map julia-repl-mode-map ("C-c C-e" . nil))
  :hook julia-mode)

;; TODO: add Julia snail?


;;;; Python
;;   ======
(use-package python
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset t)
  :hook (python-mode . (lambda () (setq tab-width 4))))

(use-package poetry
  :config
  ;; Checks for the correct virtualenv. Better strategy IMO because the default
  ;; one is quite slow.
  (setq poetry-tracking-strategy 'switch-buffer)
  :hook (python-base-mode . poetry-tracking-mode))

;; (defun my/eglot-workspace-config (server)
;;     (if-let ((venv (simple-venv-find))
;;         ((not (eq venv 'none)))
;;         (vp (file-name-directory venv))
;;         (vn (file-name-nondirectory venv)))
;;    (list (cons :python
;;           (list :venvPath vp :venv vn
;;            :pythonPath (simple-venv-interpreter venv))))))

(defun ross/eglot-workspace-config (server)
  (if (equal "pylsp" (plist-get (eglot--server-info server) :name))
      (list :pylsp (list :plugins
            (append (list :pycodestyle (list :enabled nil))
                    (if-let (((ignore-errors (poetry-ensure-in-project) t))  ;; check if we're in a poetry project
                             (venv (expand-file-name (poetry-get-virtualenv)))) ;; if we are - find the venv location
                        (list :jedi (list :environment venv))
                      nil))))))
(setq-default eglot-workspace-configuration #'ross/eglot-workspace-config)


;; ;; <OPTIONAL> Numpy style docstring for Python.  See:
;; ;; https://github.com/douglasdavis/numpydoc.el.  There are other packages
;; ;; available for docstrings, see: https://github.com/naiquevin/sphinx-doc.el
(use-package numpydoc
  :ensure t
  :defer t
  :custom
  (numpydoc-insert-examples-block nil)
  (numpydoc-template-long nil)
  :bind (:map python-mode-map
              ("C-c C-n" . numpydoc-generate)))

;; ;; <OPTIONAL> Buffer formatting on save using black.
;; ;; See: https://github.com/pythonic-emacs/blacken.
;; (use-package blacken
;;   :ensure t
;;   :defer t
;;   :custom
;;   (blacken-allow-py36 t)
;;   (blacken-skip-string-normalization t)
;;   :hook (python-mode-hook . blacken-mode))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))


(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

(setq auto-mode-alist
   (append
     '(("\\.agda\\'" . agda2-mode)
       ("\\.lagda.md\\'" . agda2-mode))
     auto-mode-alist))

;; ---------------------------------------------------------------------------------------
;;; Structural Editing
;; ---------------------------------------------------------------------------------------

;; https://karthinks.com/software/a-consistent-structural-editing-interface/
;; https://karthinks.com/software/it-bears-repeating/


;; ---------------------------------------------------------------------------------------
;;; Messaging
;; ---------------------------------------------------------------------------------------

;;;; Elfeed (RSS)
;;   ============

(use-package elfeed
  :init (setq elfeed-feeds
              '(("https://codingquark.com/feed.xml" emacs)
                ("https://ag91.github.io/rss.xml" emacs)
                ("https://sqrtminusone.xyz/posts/index.xml" emacs)
                ("http://verisimilitudes.net/rss.xml" programming)
                ("http://ngnghm.github.io/feeds/all.rss.xml" programming)
                ("http://www.loper-os.org/?feed=rss" programming)
                ("https://hbfs.wordpress.com/feed/" comp-sci))))

;; ---------------------------------------------------------------------------------------
;;; Window Manager
;; ---------------------------------------------------------------------------------------
;; (use-package perspective-exwm)


;; ---------------------------------------------------------------------------------------
;;; Guix
;; ---------------------------------------------------------------------------------------
(use-package guix)


;; ---------------------------------------------------------------------------------------
;;; General TODOs
;; ---------------------------------------------------------------------------------------

;; dragstuff
;; better comment-lines that doesn't move cursor (save-excursion or smth?)

;; make code-cells and ipynb work with treesitter (& emacs-jupyter...)
