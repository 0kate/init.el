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
  (exec-path-from-shell-copy-envs '("ASDF_DIR" "ASDF_DATA_DIR"))
  (exec-path-from-shell-initialize))

(use-package neotree
  :ensure t
  :config
  (setq neo-theme 'nerd
        neo-window-fixed-size nil
        neo-autorefresh t)
  :bind
  (("C-t" . neotree-toggle)))

(use-package whitespace
  :ensure t
  :config
  (global-whitespace-mode 1)
  (setq whitespace-line nil)
  (setq whitespace-display-mappings '((space-mark   ?\x3000 [?\▫])
                                      (tab-mark     ?\t     [?\xBB ?\t])
                                      (newline-mark ?\n     [?¬ ?\n]))))

(use-package highlight-indent-guides
  :ensure t
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-character ?┊)
  :hook
  ((prog-mode . highlight-indent-guides-mode)))

(use-package rainbow-delimiters
  :ensure t
  :hook
  ((prog-mode . rainbow-delimiters-mode)))

(use-package magit
  :ensure t)

(use-package git-gutter
  :ensure t
  :hook
  ((prog-mode . git-gutter-mode))
  :custom
  (git-gutter:modified-sign "~"))

(use-package company
  :ensure
  :config
  (global-company-mode t)
  (setq company-backends '((company-capf company-dabbrev-code))
        company-minimum-prefix-length 1
        company-idle-delay 0.0))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package flycheck-rust
  :ensure t
  :hook
  ((rust-mode . flycheck-rust-setup)))

(use-package yaml-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package zig-mode
  :ensure t)

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((c-mode . lsp)
   (ruby-mode . lsp)
   (rust-mode . lsp)
   (zig-mode . lsp))
  ;; (lsp-mode . lsp-enable-which-key-integration)
  :config
  ;; for clangd
  (setq lsp-clangd-binary-path (executable-find "clangd"))
  ;; for rust-analyzer
  ;; (setq lsp-rust-analyzer-display-parameter-hints t
  ;;       lsp-rust-analyzer-binding-mode-hints t
  ;;       lsp-rust-analyzer-inlay-hints-mode t
  ;;       lsp-rust-analyzer-display-lifetime-elision-hints-enable t
  ;;       lsp-rust-analyzer-server-display-inlay-hints t)
  ;; for zls
  (setq lsp-zig-zls-executable (executable-find "zls"))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions t))

;; Hooks
(add-hook 'window-setup-hook '(lambda ()
                                (set-face-background 'default "undefined")))
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'eshell-mode-hook '(lambda ()
                               (display-line-numbers-mode -1)
                               (local-set-key (kbd "C-l") (lambda ()
                                                            (with-current-buffer "*eshell*"
                                                              (end-of-buffer)
                                                              (eshell-kill-input)
                                                              (insert "clear 1")
                                                              (eshell-send-input)
                                                              (eshell-bol)
                                                              (yank))))
                               (setq-local maximum-scroll-margin 1.0
                                           scroll-margin 0)))
(add-hook 'c-mode-hook '(lambda ()
                          (setq c-basic-offset 8)))

;; Basic options
(setq maximum-scroll-margin 0.5
      scroll-margin 99999
      scroll-step 1)
(setq-default indent-tabs-mode nil)
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)
(electric-pair-mode 1)

(menu-bar-mode -1)
(tool-bar-mode -1)

(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;; Basic keybinds
;; (global-set-key (kbd "C-i") 'next-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<up>") 'enlarge-window)
(global-set-key (kbd "C-<down>") 'shrink-window)

;; Custsom commands
(defun split ()
  (interactive)
  (split-window-horizontally))

(defun vsplit ()
  (interactive)
  (split-window-vertically))

(setq copy-command '("xclip")
      paste_command "xclip -o | tr -d \r"
      copy-process nil)

(defun copy-to-clipboard (text)
  (setq copy-process (make-process :name "copy-proc"
                                   :buffer nil
                                   :command copy-command
                                   :connection-type 'pipe))
  (process-send-string copy-process text)
  (process-send-eof copy-process))

(defun paste-from-clipboard ()
  (if (and copy-process (process-live-p copy-process))
      nil
    (shell-command-to-string paste_command)))

(setq interprogram-cut-function 'copy-to-clipboard)
(setq interprogram-paste-function 'paste-from-clipboard)

;; asdf enable
(let ((path (substitute-env-vars (concat (concat (if (getenv "ASDF_DATA_DIR") "$ASDF_DATA_DIR" "$HOME") "/.asdf/shims")
                                         ":$HOME/.asdf/bin:$PATH"))))
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
