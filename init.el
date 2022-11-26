;; Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)
(package-refresh-contents)

(defun package-ensure-package (package)
  (unless (package-installed-p package)
    (package-install package)))

;; monokai-theme
(package-ensure-package 'monokai-theme)
(load-theme 'monokai t)

;; exec-path-from-shell
(package-ensure-package 'exec-path-from-shell)
(exec-path-from-shell-copy-envs '("ASDF_DIR" "ASDF_DATA_DIR"))
(exec-path-from-shell-initialize)

;; neotree
(package-ensure-package 'neotree)
(setq neo-theme 'nerd
      neo-window-fixed-size nil
      neo-autorefresh t)
(global-set-key (kbd "C-t") 'neotree-toggle)

;; whitespace
(package-ensure-package 'whitespace)
(global-whitespace-mode 1)
(setq whitespace-line nil
      whitespace-display-mappings '((space-mark   ?\x3000 [?\▫])
                                    (tab-mark     ?\t     [?\xBB ?\t])
                                    (newline-mark ?\n     [?¬ ?\n])))

;; highlight-indent-guides
(package-ensure-package 'highlight-indent-guides)
(setq highlight-indent-guides-method 'character
      highlight-indent-guides-character ?┊)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(add-hook 'text-mode-hook 'highlight-indent-guides-mode)

;; rainbow-delimiters
(package-ensure-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; magit
(package-ensure-package 'magit)

;; git-gutter
(package-ensure-package 'git-gutter)
(setq git-gutter:modified-sign "~")
(global-git-gutter-mode t)

;; company
(package-ensure-package 'company)
(global-company-mode t)
(setq company-backends '((company-capf company-dabbrev-code))
      company-minimum-prefix-length 1
      company-idle-delay 0)

;; flycheck
(package-ensure-package 'flycheck)
(global-flycheck-mode)

;; flycheck-rust
(package-ensure-package 'flycheck-rust)
(add-hook 'rust-mode-hook 'flycheck-rust-setup)

;; ivy
(package-ensure-package 'ivy)
(ivy-mode t)

;; yaml-mode
(package-ensure-package 'yaml-mode)

;; rust-mode
(package-ensure-package 'rust-mode)

;; docker
(package-ensure-package 'docker)
(package-ensure-package 'dockerfile-mode)

;; web-mode
(package-ensure-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(setq web-mode-attr-indent-offset nil
      web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2
      web-mode-enable-current-element-highlight t)
(setq web-mode-enable-auto-closing t
      web-mode-enable-auto-pairing t
      web-mode-auto-close-style 2
      web-mode-tag-auto-close-style 2)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-html-tag-bracket-face ((t (:foreground "#909090")))))

(package-ensure-package 'typescript-mode)

(package-ensure-package 'tide)
(setq tide-completion-ignore-case t
      tide-completion-show-source t
      tide-completion-fuzzy t
      tide-completion-detailed t)
(add-hook 'web-mode-hook (lambda ()
                      (tide-setup)))

;; rainbow-mode
(package-ensure-package 'rainbow-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)
(add-hook 'text-mode-hook 'rainbow-mode)

;; lsp-mode
(package-ensure-package 'lsp-mode)
(setq lsp-keymap-prefix "C-c l")
;; for debug
;; (setq lsp-print-io t)
;; for clangd
(setq lsp-clangd-binary-path (executable-find "clangd"))
;; for rust-analyzer
(setq lsp-rust-analyzer-display-parameter-hints t
      lsp-rust-analyzer-binding-mode-hints t
      lsp-rust-analyzer-inlay-hints-mode t
      lsp-rust-analyzer-display-lifetime-elision-hints-enable t
      lsp-rust-analyzer-server-display-inlay-hints t)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'ruby-mode-hook 'lsp)
(add-hook 'rust-mode-hook 'lsp)
(add-hook 'yaml-mode 'lsp)
(add-hook 'dockerfile-mode 'lsp)

;; lsp-ui
(package-ensure-package 'lsp-ui)
(setq lsp-ui-sideline-show-diagnostics t
      lsp-ui-sideline-show-code-actions t)

;; powerline
(package-ensure-package 'powerline)
(powerline-center-theme)

;; restclient
(package-ensure-package 'restclient)
(package-ensure-package 'company-restclient)
(add-to-list 'company-backends 'company-restclient)

;; Hooks
(defun disable-scroll-margin ()
  (setq-local maximum-scroll-margin 0.5
              scroll-margin 99999
              scroll-step 1))

(add-hook 'window-setup-hook '(lambda ()
                                (set-face-background 'default "undefined")))
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'eshell-mode-hook '(lambda ()
                               (display-line-numbers-mode -1)))
(add-hook 'eshell-mode-hook '(lambda ()
                               (display-line-numbers-mode -1)
                               (eshell-disable-buffer-control)))
(add-hook 'c-mode-hook '(lambda ()
                          (setq c-basic-offset 8)))
(add-hook 'prog-mode-hook '(lambda () (disable-scroll-margin)))
(add-hook 'text-mode-hook '(lambda () (disable-scroll-margin)))
(add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . ruby-mode))

;; Basic options
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
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Custsom commands
(defun enable-window-sizing ()
  (interactive)
  (global-set-key (kbd "C-n") 'enlarge-window)
  (global-set-key (kbd "C-p") 'shrink-window)
  (global-set-key (kbd "C-f") 'enlarge-window-horizontally)
  (global-set-key (kbd "C-b") 'shrink-window-horizontally))

(defun disable-window-sizing ()
  (interactive)
  (global-set-key (kbd "C-n") 'next-line)
  (global-set-key (kbd "C-p") 'previous-line)
  (global-set-key (kbd "C-f") 'forward-char)
  (global-set-key (kbd "C-b") 'backward-char))

(defun eshell-enable-buffer-control ()
  (interactive)
  (when (eq major-mode 'eshell-mode)
    (local-set-key (kbd "C-p") 'previous-line)
    (local-set-key (kbd "C-n") 'next-line)
    (message "[eshell] Buffer control is enabled")))

(defun eshell-disable-buffer-control()
  (interactive)
  (when (eq major-mode 'eshell-mode)
    (local-set-key (kbd "C-p") 'eshell-previous-input)
    (local-set-key (kbd "C-n") 'eshell-next-input)
    (message "[eshell] Buffer control is disabled")))

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
   '(restclient use-package monokai-theme magit lsp-mode company)))
