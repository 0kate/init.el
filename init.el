;; Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)
(package-refresh-contents)

(require 'use-package)

(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

(use-package neotree
  :ensure t
  :config
  (setq neo-theme 'nerd
        neo-window-fixed-size nil
        neo-smart-open t
        neo-autorefresh t)
  :bind
  (("C-t" . neotree-toggle)))

;; whitespace
(use-package whitespace
  :ensure t
  :config
  (global-whitespace-mode 1)
  (setq whitespace-display-mappings '((space-mark   ?\x3000 [?\▫])
                                    (tab-mark     ?\t     [?\xBB ?\t])
                                    (newline-mark ?\n     [?¬ ?\n]))))

;; magit
(use-package magit
  :ensure t)

(use-package company
  :ensure
  :config
  (global-company-mode t)
  (setq company-backends '((company-capf company-dabbrev-code))
        company-minimum-prefix-length 1
        company-idle-delay 0.0))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((ruby-mode . lsp))
  ;; (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

;; Hooks
(add-hook 'window-setup-hook '(lambda ()
                                (set-face-background 'default "undefined")))
(add-hook 'after-init-hook 'global-company-mode)

;; Basic options
(setq make-backup-files nil)
(setq maximum-scroll-margin 0.5
      scroll-margin 99999
      scroll-step 1)
(setq-default indent-tabs-mode nil)
(setq make-backup-files nil)
(setq auto-save-default nil)

(menu-bar-mode -1)
(tool-bar-mode -1)

(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;; Basic keybinds
;; (global-set-key (kbd "C-i") 'next-buffer)
(global-set-key (kbd "M-]") 'enlarge-window-horizontally)
(global-set-key (kbd "M-[") 'shrink-window-horizontally)

;; Custsom commands
(defun split ()
  (interactive)
  (split-window-horizontally))

(defun vsplit ()
  (interactive)
  (split-window-vertically))

;; asdf enable
(let ((path (substitute-env-vars (concat "$HOME/.asdf" "/shims:" "$HOME/.asdf" "/bin:$PATH"))))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   (vector "#1d1f21" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#c5c8c6"))
 '(beacon-color "#cc6666")
 '(compilation-message-face 'default)
 '(custom-enabled-themes '(sanityinc-tomorrow-bright))
 '(custom-safe-themes
   '("78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))
 '(fci-rule-color "#373b41")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   '(("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100)))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages '(magit use-package neotree monokai-theme))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
