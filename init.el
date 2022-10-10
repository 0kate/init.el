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

;; Hooks
(setq window-setup-hook '(lambda ()
                           (set-background-color "ARGBBB000000")))

;; Basic options
(setq make-backup-files nil)
(setq maximum-scroll-margin 0.5
      scroll-margin 99999
      scroll-step 1)
(setq-default indent-tabs-mode nil)

(menu-bar-mode -1)

(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;; Basic keybinds
(global-set-key (kbd "C-i") 'next-buffer)
(global-set-key (kbd "M-]") 'enlarge-window-horizontally)
(global-set-key (kbd "M-[") 'shrink-window-horizontally)

;; Custsom commands
(defun split ()
  (interactive)
  (split-window-horizontally))

(defun vsplit ()
  (interactive)
  (split-window-vertically))
