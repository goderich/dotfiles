;; -*- no-byte-compile: t; -*-
;;; private/iwaka/packages.el

;; Temporary workaround for autoload errors
(package! evil-matchit :recipe (:fetcher github :repo "redguardtoo/evil-matchit" :commit "7d65b4167b1f0086c2b42b3aec805e47a0d355c4"))

;; Chinese fonts
(package! cnfonts)

;; No-distraction writing
(package! writeroom-mode)

;; Citations
(package! org-ref)
