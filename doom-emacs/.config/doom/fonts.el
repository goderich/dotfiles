;;; ~/dotfiles/doom-emacs/.config/doom/+fonts.el -*- lexical-binding: t; -*-

;; When :size is a float value, it's in points (pixels if int)
(setq doom-font
      (font-spec
       :family "Source Code Pro"
       :size 20.0))

(setq doom-unicode-font
      (font-spec
       :family "Noto Sans Mono CJK TC"
       :lang "zh"
       :size 22.0))

(setq doom-variable-pitch-font
      (font-spec
       :family "Linux Libertine O"))

(after! mixed-pitch
  (setq mixed-pitch-set-height t)
  (set-face-attribute 'variable-pitch nil :height 1.2))
