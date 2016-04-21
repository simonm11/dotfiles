;;; package --- summary:

;;; Commentary:

;;; code:

(require 'package)
(dolist (source '(("melpa" . "http://melpa.milkbox.net/packages/")))  
  (add-to-list 'package-archives source t))

(package-initialize)

(setenv "PATH" (concat (getenv "PATH") ":~/.node_modules/bin"))
(setq exec-path (append exec-path '("~/.node_modules/bin")))

;; (load-theme 'hc-zenburn t)
(require 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-night t)

;; highlight current line
;; (global-hl-line-mode 1)

(require 'revive)
;;Keyboard shortcuts
(define-key ctl-x-map "S" 'save-current-configuration)
(define-key ctl-x-map "F" 'resume)
(define-key ctl-x-map "K" 'wipe)

(require 'powerline)
(powerline-default-theme)

(set-face-attribute 'mode-line nil
                    :foreground "#e0e0e0"
		    :background "#CC3300"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :box nil)

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; PROJECTILE
;; @TODO some hotkeys
(projectile-global-mode)

;; BOOKMARK+
; (require 'bookmark+)

;;(require 'indent-guide)
;;(add-hook 'prog-mode-hook 'indent-guide-mode)

;; FLYCHECK

(add-hook 'after-init-hook 'global-flycheck-mode)

(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;(add-hook 'prog-mode-hook (lambda() (set-fringe-mode 0)))
;(toggle-scroll-bar -1)

; hideshow minor mode
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key (kbd "C-c c") 'hs-toggle-hiding)

;(require 'egg)

(defun sudired ()
  "Documentation."
  (interactive)
  (dired "/sudo::/"))

(setq tramp-default-method "ssh")
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

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

(require 'ace-jump-mode)
(define-key global-map (kbd "C-f") 'ace-jump-mode)

;; setup size of initial frame

;;(setq default-frame-alist
;;      '((top . 10) (left . 10)
;;        (width . 135) (height . 40)
;;        ))

;; offset in c
;(setq c-basic-offset 4)

;; save all emacs backup files (~ files) in .emacs.d/backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

; disable lock files (.#files)
(setq create-lockfiles nil)

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


(set-face-attribute 'default nil :family "DejaVu Sans Mono" :foundry "PfEd" :height 100)

;(setq-default line-spacing 10)

; (setq-default indent-tabs-mode nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "/home/simon/.emacs.d/bookmarks")
 '(cua-mode t nil (cua-base))
 '(initial-buffer-choice t)
 '(line-spacing 0.5)
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(standard-indent 4)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
