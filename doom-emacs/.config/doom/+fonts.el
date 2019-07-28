;;; ~/dotfiles/doom-emacs/.config/doom/+fonts.el -*- lexical-binding: t; -*-

;; When :size is a float value, it's in points (pixels if int)
(setq doom-font
      (font-spec
       :family "Source Code Pro"
       :size 16.0))

(setq doom-unicode-font
      (font-spec
       :family "Noto Sans Mono CJK TC"
       :lang "zh"))

;; Setup Chinese fonts to look all nice and shit
(add-hook 'after-make-frame-functions 'cnfonts-set-font-with-saved-step)
(add-hook 'window-setup-hook 'cnfonts-set-font-with-saved-step)