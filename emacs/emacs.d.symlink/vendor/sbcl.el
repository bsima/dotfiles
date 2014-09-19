;(load "~/quicklisp/setup.lisp")
;(load (expand-file-name "~/quicklisp/slime-helper.el"))

(setq inferior-lisp-program (executable-find "sbcl"))

(add-to-list 'auto-mode-alist '("\\.lisp$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.asd$" . lisp-mode))

(setq slime-enable-evaluate-in-emacs t)

;; Prevent SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a `(`
;;(defun override-slime-repl-bindings-with-paredit ()
;;(define-key slime-repl-mode-map
;;  (read-kbd-macro paredit-backward-delete-key) nil
))
;;(add-hook 'slime-repl-mode-hook 'overrideslime-repl-bindings-with-paredit)


;; Org-mode settings
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-font-lock-mode 1)
