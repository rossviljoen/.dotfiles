;; early-init.el -*- lexical-binding: t; -*-

(setq default-frame-alist '(
                            ;; Setting the face in here prevents flashes of
                            ;; color as the theme gets activated
                            (background-color . "#000000")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))

(setq package-enable-at-startup nil)

