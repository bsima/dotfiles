;; This is where your customizations should live

;; env PATH
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; Uncomment the lines below by removing semicolons and play with the
;; values in order to set the width (in characters wide) and height
;; (in lines high) Emacs will have whenever you start it

;; (setq initial-frame-alist '((top . 0) (left . 0) (width . 20) (height . 80)))


;; Place downloaded elisp files in this directory. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")

;; PHP packages
;(require 'php-mode)
;(require 'flymake-php)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; === File extensions =====================================

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.hl\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))


;; === Themes ==============================================

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

;; Uncomment this to increase font size
;; (set-face-attribute 'default nil :height 140)
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     ;; (color-theme-initialize)
     (color-theme-sanityinc-tomorrow-night)
     ;; (color-theme-sanityinc-tomorrow-bright)
     ;; (color-theme-sanityinc-tomorrow-blue)
     ;; (color-theme-sanityinc-tomorrow-eighties)
     ;; (color-theme-moe-dark) ;; don't know if this one works
     ))

;;(load-theme 'moe-dark t)

;; == Customizations =======================================

;; Turn on line numbers
(global-linum-mode 1)

;; Column marker at 80 characters
(require 'column-marker)
(add-hook
 'clojure-mode-hook
  (lambda () (interactive) (column-marker-1 80)))

;; Flyspell often slows down editing so it's turned off
(remove-hook 'text-mode-hook 'turn-on-flyspell)

(load "~/.emacs.d/vendor/clojure")

;; hippie expand - don't try to complete with file names
(setq hippie-expand-try-functions-list (delete 'try-complete-file-name hippie-expand-try-functions-list))
(setq hippie-expand-try-functions-list (delete 'try-complete-file-name-partially hippie-expand-try-functions-list))

(setq ido-use-filename-at-point nil)

;; Save here instead of littering current directory with emacs backup files
(setq backup-directory-alist `(("." . "~/.saves")))

;; Vim-mode (Evil-mode)
;; (require 'evil)
;; (require 'evil-paredit)
;; (evil-mode 1)
;; (setq evil-default-cursor t)

;; Textmate mode
(require 'textmate)
(textmate-mode)
