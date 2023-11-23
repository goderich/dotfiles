;;; package ---- my own config
;;; private/iwaka/config.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(load! "vars")
(load! "functions")
(load! "fonts")
(after! org
  (load! "org")
  (load! "links")
  (load! "refs")
  (load! "orca")
  (after! transient
    (load! "pandoc")))
(load! "ui-ux")
(load! "keybindings")
(load! "cite")
(after! mu4e
  (load! "mail"))

;;; config.el ends here
