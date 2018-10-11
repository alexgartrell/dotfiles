;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(column-number-mode t)

(setq vc-handled-backends nil)

(setq package-archives '())

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(c-add-style "fbcode"
	     '("stroustrup"
	       (indent-tabs-mode . nil)
	       (c-basic-offset . 2)
	       (c-indent-level . 2)
	       (c-offsets-alist . ((innamespace . [0])
				   (member-init-intro . ++)
				   (member-init-cont . 0)
				   (arglist-intro . ++)
				   (arglist-cont . 0)
				   (defun-close . [0])
				   (access-label . /)))))

;;; Mohan & Kumar's unique style
(c-add-style "mohan"
	     '("linux"
	       (indent-tabs-mode 't)
	       (c-basic-offset . 8)
	       (tab-width . 8)))


(defun maybe-fbcode-style ()
  (when (and buffer-file-name
	     (string-match "fbcode" buffer-file-name))
    (c-set-style "fbcode")))

; (setq-default c-default-style "mohan")
(setq-default c-default-style "fbcode")

(add-hook 'c-mode-hook 'fbcode-style)

(add-hook 'c++-mode-hook
	  (lambda ()
	    (c-set-style "fbcode")))

;; No backup files
(setq make-backup-files nil)

(setq-default fill-column 75)

;; My keys
(global-set-key (kbd "C-x m") 'compile)
(global-set-key (kbd "C-x p") 'fill-paragraph)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x C-b") 'delete-trailing-whitespace)
(global-set-key (kbd "C-x r") 'replace-string)

;; Do paren matching
(show-paren-mode 't)

;; Never insert tabs, which are terrible
(setq indent-tabs-mode nil)


;; Hide menu bar
(menu-bar-mode 0)

;; Don't show startup message
(setq inhibit-startup-message t)

;; Enable highlighting of selected regions
(transient-mark-mode 1)

; For visual selection
(setq transient-mark-mode 1)

(add-to-list 'load-path "~/.emacs.d/lisp")

;; Color themes
(add-to-list 'load-path "~/.emacs.d/lisp/color-theme")
(add-to-list 'load-path "~/.emacs.d/lisp/color-theme/solarized")
(require 'color-theme-solarized)

;; LustyExplorer - for find-file, etc.
; We use this instead of ido-mode, because it's better
(require 'lusty-explorer)
(global-set-key (kbd "C-x C-f") 'lusty-file-explorer)
(global-set-key (kbd "C-x b") 'lusty-buffer-explorer)


;; Add configerator mode aliases
(setq auto-mode-alist (cons '("TARGETS$" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("^BUCK$" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.cinc$" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.cconf$" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.tw$" . python-mode) auto-mode-alist))

;; Add markdown mode aliases
(setq auto-mode-alist (cons '("\.md$" . markdown-mode) auto-mode-alist))

;; Have highlighting all the time
(global-font-lock-mode 1)

;; Thrift mode for .thrift files
(autoload 'thrift-mode "thrift-mode" nil t nil)
(setq auto-mode-alist (append '(("\\.thrift$" . thrift-mode))
                              auto-mode-alist))

;; show trailing whitespace ...
(set-face-background 'trailing-whitespace "#900000")
(setq-default show-trailing-whitespace t)

(defadvice python-calculate-indentation (around outdent-closing-brackets)
  "Handle lines beginning with a closing bracket and indent them so that
they line up with the line containing the corresponding opening bracket."
  (save-excursion
    (beginning-of-line)
    (let ((syntax (syntax-ppss)))
      (if (and (not (eq 'string (syntax-ppss-context syntax)))
               (python-continuation-line-p)
               (cadr syntax)
               (skip-syntax-forward "-")
               (looking-at "\\s)"))
          (progn
            (forward-char 1)
            (ignore-errors (backward-sexp))
            (setq ad-return-value (current-indentation)))
        ad-do-it))))

(ad-activate 'python-calculate-indentation)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; For elpy
(setq elpy-rpc-python-command "python3")
;; For interactive shell
(setq python-shell-interpreter "python3")

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (rust-mode ein jedi elpy hack-time-mode esup)))
 '(safe-local-variable-values
   (quote
    ((eval c-set-offset
	   (quote arglist-close)
	   0)
     (eval c-set-offset
	   (quote arglist-intro)
	   (quote ++))
     (eval c-set-offset
	   (quote case-label)
	   0)
     (eval c-set-offset
	   (quote statement-case-open)
	   0)
     (eval c-set-offset
	   (quote substatement-open)
	   0)))))
