(add-to-list 'load-path "~/.config/emacs/plugins/neotree/")
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")

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

(load-theme 'monokai t)

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

;; neotree
(require 'neotree)
(setq neo-theme 'nerd
      neo-window-fixed-size nil
      neo-smart-open t
      neo-autorefresh t)
(global-set-key (kbd "C-t") 'neotree-toggle)

;; whitespace
(require 'whitespace)
(global-whitespace-mode 1)
(setq whitespace-display-mappings '((space-mark   ?\x3000 [?\▫])
                                    (tab-mark     ?\t     [?\xBB ?\t])
                                    (newline-mark ?\n     [?¬ ?\n])))
