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

(after! julia-repl
  (setq julia-repl-switches "--project=@.")
  (julia-repl-set-terminal-backend 'vterm))

(after! latex
  (map! :map LaTeX-mode-map
        "C-c C-b" #'TeX-font))

(use-package! jupyter
  ;; Using :map in :bind means the keys only get bound in the specified local
  ;; map. Using :bind without :map will bind keys globally.
  :bind (:map jupyter-repl-interaction-mode-map
         (("C-c C-e" . jupyter-eval-line-or-region)
          ("C-c C-r" . jupyter-eval-region)
          ("C-c C-b" . jupyter-eval-buffer)
          ("C-c C-f" . jupyter-eval-defun))))

;; Hack to fix Jupyter REPL freezing:
;; https://github.com/nnicandro/emacs-jupyter/issues/219
(setq jupyter-repl-echo-eval-p t)
(defun jupyter-repl-font-lock-override (_ignore beg end &optional verbose)
  `(jit-lock-bounds ,beg . ,end))
(advice-add #'jupyter-repl-font-lock-fontify-region
            :override #'jupyter-repl-font-lock-override)

(use-package! code-cells
  :bind (:map code-cells-mode-map
         (("M-p" . cells-backward-cell)
          ("M-n" . cells-forward-cell)
          ("C-c C-SPC" . cells-mark-cell)
          ;; These need to be lambdas because :bind can only bind functions with
          ;; no args. Also, need (interactive) to tell emacs that this function
          ;; is a *command* (as you can only bind commands to keys, not plain
          ;; functions)
          ("C-c C-w" . (lambda () (interactive)
                         (code-cells-command 'kill-region :use-region)))
          ("C-c M-w" . (lambda () (interactive)
                         (code-cells-command 'kill-ring-save :use-region)))
          ([remap python-shell-send-region] .
           (lambda () (interactive)
             (code-cells-command 'python-shell-send-region :use-region :pulse)))
          ([remap jupyter-eval-line-or-region] .
           (lambda () (interactive)
             (code-cells-command 'jupyter-eval-region :use-region :pulse))))))
