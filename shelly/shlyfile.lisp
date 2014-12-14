;; -*- mode: common-lisp -*-

;; This is the main config file for Shelly <http://shlyfile.org>. It's loaded every
;; time Shelly is run.

;; ...which means that I can basically use this as a portable system environment.
;; Imagine that, all I have to do to setup my ideal environment, is `git clone` my
;; dotfiles repo, run the setup, and install the SBCL with Shelly. This file would
;; be a part of my dotfiles, so I can store everything I need in here. Or, I can make
;; separate files and packages that this file can import. Thus, I'll be basically
;; creating a personalized layer on top of whatever system I'm running. I can do away
;; with all the zsh stuff and write everything in Lisp. I can also make a `.sbcl` file
;; that's loaded whenever I startup the SBCL REPL, which also loads the same files as
;; this one. Over time, I'll have a completely portable system.
