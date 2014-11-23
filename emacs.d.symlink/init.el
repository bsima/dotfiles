;;; When this file gets to be over ~200 lines, I'll break up the
;;; sections into their own files

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages

(require 'package)

;; Load packages
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environment

(require 'cl)

(defvar emacs-root (if (or (eq system-type 'cygwin)
                           (eq system-type 'gnu/linux)
                           (eq system-type 'linux))
                        "/home/bsima/"
                        "c:/home/bsima")
  "My home directory - root of my personal emacs-load-path")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Languages


;; Lisp

;; SBCL
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

;; Clojure

;; Python
; ... To be continued

;; OCaml
; ... To be continued

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keyboard shortcuts

;; alias C-; to M (meta, alt)
(global-set-key (kbd "C-;") 'execute-extended-command)
;; alias C-x/c C-m to M-x (tho I don't use it too often)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; alias C-' to shell-command (usually M-!)
(global-set-key (kbd "C-'") 'shell-command)

;; make C-w into backspace, C-x/c C-k into "cut" (which is what C-w was previously)
;; this one is really handy...
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; M-x qrr = find and replace
(defalias 'qrr 'query-replace-regexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display

;; get rid of chrome
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(let ((fantasque-font
      "-apple-Fantasque_Sans_Mono-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1"))
  (set-frame-font fantasque-font nil t))

(load-theme 'tangotango t)

(load-library "~/.emacs-local")

(shell)

;;; end .emacs
