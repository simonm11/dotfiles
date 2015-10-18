;;; package --- summary:

;;; Commentary:

;;; code:

(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")
		  ("melpa" . "http://melpa.milkbox.net/packages/")
                  ))
  (add-to-list 'package-archives source t))

(package-initialize)

(load-theme 'zenburn t)

(require 'powerline)
(powerline-default-theme)

(set-face-attribute 'mode-line nil
                    :foreground "#e0e0e0"
		    :background "#CC3300"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :box nil)


(add-hook 'after-init-hook 'global-flycheck-mode)


(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

(require 'egg)

(defun sudired ()
  "Documentation."
  (interactive)
  (dired "/sudo::/"))

(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;; ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; enhanced buffer manager C-x/C-b
(require 'ibuffer)
(defalias 'list-buffers 'ibuffer)

;; display line numbers
(global-linum-mode 1)

;; normal copy/past mode
(cua-mode t)

;; fix to make autopair work with cua mode
(delete-selection-mode 1)

(put 'autopair-insert-opening 'delete-selection t)
(put 'autopair-skip-close-maybe 'delete-selection t)
(put 'autopair-insert-or-skip-quote 'delete-selection t)
(put 'autopair-extra-insert-opening 'delete-selection t)
(put 'autopair-extra-skip-close-maybe 'delete-selection t)
(put 'autopair-backspace 'delete-selection 'supersede)
(put 'autopair-newline 'delete-selection t)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;;
(global-auto-revert-mode 1)

;; use trump to open file as root when using ido-find-file
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; No tool bar
(tool-bar-mode -1)

;; Cursor as a line
(setq-default cursor-type 'bar)


(setq default-frame-alist
      '((top . 20) (left . 100)
        (width . 170) (height . 60)
        ))

;; offset in c
;(setq c-basic-offset 4)

;; ?? don't remember
(setq indent-tabs-mode nil)

;; save all emacs backup files (~ files) in .emacs.d/backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backupsM-x package-install ternM-x package-install tern
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )


;; Add .vertex and .frag as c++ mode
(add-to-list 'auto-mode-alist '("\\.frag\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.vertex\\'" . c++-mode))


;; JS
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern)
     (require 'tern-auto-complete)
     (tern-ac-setup)))


(add-text-properties (point-min) (point-max)
                     '(line-spacing 0.25 line-height 1.25))

(setq-default line-spacing 0.25)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" default)))
 '(initial-buffer-choice t)
 '(show-paren-mode t)
 '(standard-indent 4)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil :family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 98 :width normal))))
 '(highlight-indentation-face ((t nil))))
