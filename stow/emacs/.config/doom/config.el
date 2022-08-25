;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Ross Viljoen"
      user-mail-address "ross@viljoen.co.uk"
      doom-scratch-intial-major-mode 'lisp-interaction-mode
      doom-theme 'doom-solarized-light
      display-line-numbers-type t
      load-prefer-newer t
      company-idle-delay nil)

(setq org-directory "~/org/")

(setq +default-want-RET-continue-comments nil)

(after! dired
  (setq dired-listing-switches "-aBhl  --group-directories-first"
        dired-dwim-target t
        dired-recursive-copies (quote always)
        dired-recursive-deletes (quote top)))

(map! "C-x C-j" #'dired-jump)

(remove-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'message-mode-hook #'word-wrap-mode)

(after! julia-repl
  (define-key! julia-repl-mode-map "C-c C-e" nil)
  (setq julia-repl-switches "--project=@.") ;; Activate first parent project found
  (julia-repl-set-terminal-backend 'vterm))

(setq lsp-enable-folding t)
(setq lsp-julia-package-dir nil)
(use-package! lsp-julia
  ;; N.B. Pkg.add LanguageServer and SymbolServer in Julia repl
  :config
  (setq lsp-julia-default-environment (concat (getenv "JULIA_DEPOT_PATH") "/environments/v1.7"))
  (setq-hook! 'julia-mode-hook +format-with-lsp nil))

;; Doesn't work because of errors in formatter_service.jl?
;; (use-package! julia-formatter
;;   :config
;;   ;; No idea what I was trying to do here??
;;   ;; (set-formatter! 'julia-formatter
;;   ;;   (lambda () (julia-formatter-format-region
;;   ;;          (point-min)
;;   ;;          (point-max)))
;;   ;;   :modes 'julia-mode))
;;   (add-hook 'julia-mode-hook
;;     (lambda ()
;;       (add-hook 'before-save-hook
;;                 (lambda ()
;;                   (julia-formatter-format-region
;;                    (point-min)
;;                    (point-max)))
;;                 nil
;;                 t))))
;; (require 'julia-formatter)

(after! latex
  (map! :map LaTeX-mode-map
        "C-c C-b" #'TeX-font))

(set-popup-rule! "^\\*jupyter-repl*" :ignore t)
(use-package! jupyter
  ;; Using :map in :bind means the keys only get bound in the specified local
  ;; map. Using :bind without :map will bind keys globally.
  :bind (:map jupyter-repl-interaction-mode-map
         (("C-c C-e" . jupyter-eval-line-or-region)
          ("C-c C-r" . jupyter-eval-region)
          ("C-c C-b" . jupyter-eval-buffer)
          ("C-c C-f" . jupyter-eval-defun))
         :map jupyter-repl-mode-map
         ;; This makes latex input work in the jupyter repl (should probably
         ;; make this Julia specific though)
         (("TAB" . julia-latexsub-or-indent))))

;; Hack to fix Jupyter REPL freezing:
;; https://github.com/nnicandro/emacs-jupyter/issues/219
(setq jupyter-repl-echo-eval-p t)
(defun jupyter-repl-font-lock-override (_ignore beg end &optional verbose)
  `(jit-lock-bounds ,beg . ,end))
(advice-add #'jupyter-repl-font-lock-fontify-region
            :override #'jupyter-repl-font-lock-override)
;; Needed for LaTeX and some other rich output
(setq org-babel-jupyter-resource-directory (concat user-emacs-directory "jupyter"))

(use-package! code-cells
  :hook ((julia-mode python-mode) . code-cells-mode)
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

  ;; :bind (:map code-cells-mode-map
  ;;        (("M-p" . code-cells-backward-cell)
  ;;         ("M-n" . code-cells-forward-cell)
  ;;         ("C-c C-SPC" . code-cells-mark-cell)
  ;;         ;; These need to be lambdas because :bind can only bind functions with
  ;;         ;; no args. Also, need (interactive) to tell emacs that this function
  ;;         ;; is a *command* (as you can only bind commands to keys, not plain
  ;;         ;; functions)
  ;;         :config
  ;;         ("C-c C-w" . (lambda () (interactive)
  ;;                        (code-cells-command 'kill-region :use-region)))
  ;;         ("C-c M-w" . (lambda () (interactive)
  ;;                        (code-cells-command 'kill-ring-save :use-region)))
  ;;         ([remap python-shell-send-region] .
  ;;          (lambda () (interactive)
  ;;            (code-cells-command 'python-shell-send-region :use-region :pulse)))
  ;;         ([remap jupyter-eval-line-or-region] .
  ;;          (lambda () (interactive)
  ;;            (code-cells-command 'jupyter-eval-region :use-region :pulse))))))
