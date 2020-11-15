;;; package ---- my own config
;;; private/iwaka/config.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(load! "+vars")
(load! "+fonts")
(load! "+org")
(load! "+ui-ux")
(load! "functions")
(load! "+keybindings")
(load! "+referencing")
(if (string= (system-name) "iwaka-thinkpad")
    (load! "+mail"))

;;; config.el ends here
