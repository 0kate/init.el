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

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

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

(use-package rust-mode
  :ensure t)

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((ruby-mode . lsp)
   (rust-mode . lsp))
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

(setq clipboard-cli "xclip"
      cut-command clipboard-cli
      paste-command (concat clipboard-cli " -o"))

(defun cut-with-window-system (text &optional rest)
  (let ((process-connection-type nil))
    (let ((proc (start-process cut-command "*Messages*" cut-command)))
      (process-send-string proc text)
      (process-send-eof proc))))

(defun paste-with-window-system ()
  (shell-command-to-string paste-command))

(when (and (not window-system)
           (executable-find clipboard-cli))
  (setq interprogram-cut-function 'cut-with-window-system)
  (setq interprogram-paste-function 'paste-with-window-system))

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
 '(package-selected-packages
   '(use-package neotree mozc monokai-theme magit lsp-mode company color-theme-sanityinc-tomorrow)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
