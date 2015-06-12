;;; init.el - My emacs stuff
;;
;; Author:   Ben Sima <bensima@gmail.com>
;; URL:      http://github.com/bsima/dotfiles
;; Version:  42
;; Keywords: mine

;;; Commentary:

;; Some of this is taken from Steve Yegge:
;;   https://sites.google.com/site/steveyegge2/my-dot-emacs-file
;;   https://sites.google.com/site/steveyegge2/my-dot-emacs-file
;;
;; When I borrowed stuff, I tried to include inline-references to
;; the original code.
;;
;; When this file gets to be over ~200 lines, I'll break up the
;; sections into their own files

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages

(require 'package)

;; Load packages
;; FIXME: these should be https, but there is a bug, see:
;; https://github.com/nicferrier/elmarmalade/issues/55
(add-to-list 'package-archives '("melpa"     . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(setq package-enable-at-startup nil); Don't initialize later as well
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Languages

;; General
(setq-default indent-tabs-mode nil)

;; Lisp general / elis
(rainbow-delimiters-mode)
(require-package 'paredit)
(show-paren-mode)
(require 'hl-sexp)
(add-hook 'emacs-lisp-mode-hook #'hl-sexp-mode)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)

;; SBCL
;(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
(add-hook 'lisp-mode-hook #'hl-sexp-mode)
(add-hook 'lisp-mode-hook #'paredit-mode)

;; Clojure
(add-to-list 'auto-mode-alist '("\\.boot" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.hl" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs" . clojure-mode))
(require-package 'cider)
(require-package 'company)

(setq cider-repl-history-file "~/.emacs.d/cider-history")
(setq cider-repl-use-pretty-printing t)
(setq cider-repl-use-clojure-font-lock t)
(setq cider-repl-result-prefix ";; => ")
(setq cider-repl-wrap-history t)
(setq cider-repl-history-size 3000)
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq cider-show-error-buffer nil)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'clojure-mode-hook #'hl-sexp-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)


;; Ruby
(add-to-list 'auto-mode-alist '("\\Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Guardfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.haml" . haml-mode))

(add-hook 'ruby-mode-hook 'robe-mode)
(require 'rvm)
;(rvm-use-default)

; Get rvm.el to auto start before using robe
(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

;; SASS
(add-to-list 'auto-mode-alist '("\\.scss" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.sass" . sass-mode))

;; CoffeeScript
(coffee-tab-width 2)


;; Python
;;(require 'virtualenvwrapper)
;;(venv-initialize-eshell)

;; OCaml
;; ... To be continued


;; JavaScript
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environment

(require 'cl-lib) ;; use common lisp everywhere

;; My emacs home directory
(defvar emacs-root (if (or (eq system-type 'gnu/linux)
			   (eq system-type 'linux))
                        "/home/bsima/.dotfiles/emacs.d.symlink/"
                        "/Users/bsima/.dotfiles/emacs.d.symlink/"))

;; Configure backups and autosaves
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(defun edit-init ()
  "Edits my configuration file"
  (interactive)
  (find-file (concat emacs-root "init.el")))

(defun load-init ()
  "Loads my init file"
  (interactive)
  (load-file (concat emacs-root "init.el")))

(defun aris () (interactive) (find-file "/home/bsima/projects/aristotl/TODO.org"))

;; I want yasnippet everywhere
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-snippet-dirs
      '("~/me/system/snippets" ;; personal snippets in my dotme repo
        "~/me/system/snippets/yasnippet-snippets"))


;; Shell emulator stuff

(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

; http://paralambda.org/2012/07/02/using-gnu-emacs-as-a-terminal-emulator/
(when (require 'multi-term nil t)
  (global-set-key (kbd "<f5>") 'multi-term)
  (global-set-key (kbd "<C-next>") 'multi-term-next)
  (global-set-key (kbd "<C-prior>") 'multi-term-prev)
  (setq multi-term-buffer-name "term"
        multi-term-program "/bin/zsh"))

(when (require 'term nil t) ; only if term can be loaded..
  (setq term-bind-key-alist
        (list (cons "C-c C-c"   'term-interrupt-subjob)
              (cons "C-p"       'previous-line)
              (cons "C-n"       'next-line)
              (cons "M-f"       'term-send-forward-word)
              (cons "M-b"       'term-send-backward-word)
              (cons "C-c C-j"   'term-line-mode)
              (cons "C-c C-k"   'term-char-mode)
              (cons "M-DEL"     'term-send-backward-kill-word)
              (cons "M-d"       'term-send-forward-kill-word)
              (cons "<C-left>"  'term-send-backward-word)
              (cons "<C-right>" 'term-send-forward-word)
              (cons "C-r"       'term-send-reverse-search-history)
              (cons "M-p"       'term-send-raw-meta)
              (cons "M-y"       'term-send-raw-meta)
              (cons "C-y"       'term-send-raw))))

(when (require 'term nil t)
  (defun term-handle-ansi-terminal-messages (message)
    (while (string-match "\eAnSiT.+\n" message)
      ;; Extract the command code and the argument.
      (let* ((start (match-beginning 0))
             (command-code (aref message (+ start 6)))
             (argument
              (save-match-data
                (substring message
                           (+ start 8)
                           (string-match "\r?\n" message
                                         (+ start 8))))))
        ;; Delete this command from MESSAGE.
        (setq message (replace-match "" t t message))
        
        (cond ((= command-code ?c)
               (setq term-ansi-at-dir argument))
              ((= command-code ?h)
               (setq term-ansi-at-host argument))
              ((= command-code ?u)
               (setq term-ansi-at-user argument))
              ((= command-code ?e)
               (save-excursion
                 (find-file-other-window argument)))
              ((= command-code ?x)
               (save-excursion
                 (find-file argument))))))
    
    (when (and term-ansi-at-host term-ansi-at-dir term-ansi-at-user)
      (setq buffer-file-name
            (format "%s@%s:%s" term-ansi-at-user term-ansi-at-host term-ansi-at-dir))
      (set-buffer-modified-p nil)
      (setq default-directory (if (string= term-ansi-at-host (system-name))
                                  (concatenate 'string term-ansi-at-dir "/")
                                (format "/%s@%s:%s/" term-ansi-at-user term-ansi-at-host term-ansi-at-dir))))
    message))


;; Make EWW use Chromium
(setq shr-external-browser "chromium-browser")

(defun open-github (&optional path)
  "Opens GitHub.com. Prompts for an optional argument of a path to GitHub.

For example: `explore` would resolve to https://github.com/explore, and
`bsima/bsima` would resolve to https://github.com/bsima/bsima. If no path is
provided, it defaults to https://github.com"
  (interactive "sGitHub Path: ")
  (browse-url
   (if (null path)
      "https://github.com"
      (concat "https://github.com/" path))))

(defun search-ddg (query)
  "Seraches DuckDuckGo for query."
  (interactive "sDDG: ")
  (let ((q (replace-regexp-in-string "\s" "+" query t t)))
    (browse-url (concat "https://duckduckgo.com?q=" q))))

;;this not workie
(defun jabber ()
  "Starts up jabber and switches to the buffer."
  (interactive)
  (jabber-connect jabber-username jabber-server jabber-network-server nil jabber-password)
  (switch-to-buffer "*-jabber-*"))

(defun connect-remote (user ip)
  "Opens an SSH tunnel to remote host with full emacs functionality."
  (interactive "sUser: \nsIP: ")
  (dired (format "/%s@%s:/" user ip)))

(defun markdown-to-html ()
  (interactive)
  (let* ((basename (file-name-sans-extension (buffer-file-name)))
         (html-filename (format "%s.html" basename)))
    (shell-command (format "pandoc -o %s %s"
                           html-filename (buffer-file-name)))
    (find-file-other-window html-filename)))

;;;;;;;;;
;; src => http://howardism.org/Technical/Emacs/eshell-fun.html
(defun eshell-here ()
  "Opens a new eshell in current directory.

Opens up a new shell in the directory instantiated with the
current buffer's file. The eshell is renamed to match said
directory for easier identification if useing multiple eshells."
  (interactive)
  (let* ((parent (if (buffer-file-name)
		     (file-name-directory (buffer-file-name))
		   default-directory))
	 (height (/ (window-total-height) 3))
	 (name (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))

(defun eshell/x ()
  "From the eshell, `x` exits the shell and closes the window."
  (insert "exit")
  (eshell-send-input)
  (delete-window))

;; also useful: http://www.masteringemacs.org/article/complete-guide-mastering-eshell

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org mode stuff

;;; blah blah blah

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keyboard shortcuts
;;;
;;;    C-:         new eshell in lower third (eshell-here)
;;;    C-"         shell-command (usually M-!)
;;;    C-;         same as M-x
;;;    C-x C-m     same as M-x
;;;    C-c C-m     same as M-x
;;;    C-w         backspace
;;;    C-z         kill selected region (like cut)
;;;    C-x/c C-k   cut selected text
;;;    M-Shift-L   toggle line numbers
;;;    C-q         Goto line
;;;    C-c C-s     Search DuckDuckGo
;;;    C-`         Insert character literal (`quoted-insert`)
;;;
;;;  Multiple cursors
;;;    C-S-s C-S-s Add cursor to each line in a region
;;;    C->         Mark the next similar (not-continuous)
;;;    C->         Mark the previous similiar (not-continuous)
;;;    C-c C-<     Mark all similar (not-continuous)
;;;

;; Don't disable the upcase-region function, I seem to use it alot
(put 'upcase-region 'disabled nil)

(global-set-key (kbd "C-:") 'eshell-here)
(global-set-key (kbd "C-;") 'execute-extended-command)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key (kbd "C-\"") 'shell-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key (kbd "C-z") 'kill-region)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key (kbd "M-L") 'linum-mode)
(global-set-key (kbd "C-q") 'goto-line)
(global-set-key (kbd "C-c C-s") 'search-ddg)
(global-set-key (kbd "C-`") 'quoted-insert)
(global-set-key (kbd "C-.") 'hs-toggle-hiding)

;; M-x qrr == find and replace
(defalias 'qrr 'query-replace-regexp)

;; M-x mx  == magit-status
(defalias 'ms 'magit-status)

;; M-x mt == multi-term
(defalias 'mt 'multi-term)

;;; Multiple cursors
;;; http://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;; Default window movement - use Shift+arrow-key to move between windows
(windmove-default-keybindings)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display

;; get rid of chrome
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;; Fonts
(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (find-if (lambda (f) (find-font (font-spec :name f))) fonts))
         
(set-face-attribute 'default nil :font (font-candidate '"Fantasque Sans Mono-11:weight=normal"))

;; tangotango is a good all-around theme
(load-theme 'tangotango t)

;; change theme based on day/night
(setq calendar-location-name "Home") 
(setq calendar-latitude 43.16)
(setq calendar-longitude -77.61)

;(require 'theme-changer)
;(change-theme 'solarized-light 'solarized-dark)

(load-library "~/.emacs-local")

;;; end .emacs
