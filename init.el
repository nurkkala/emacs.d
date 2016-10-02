;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tom's Emacs Configuration of Doom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set the location of the customization settings.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; ELPA - Set up packages early so that they can be used from within this file.
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)
;; After ELPA package initialization, can use locally installed libraries.

(require 's)							;String library

;; Key bindings. From emacs manual: Sequences consisting of C-c and a letter (either upper
;; or lower case) are reserved for users.
(global-set-key (kbd "C-c a") 'align-current)
(global-set-key (kbd "C-c d") 'dash-at-point)
(global-set-key (kbd "C-c e") 'emmet-expand-line)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c k e") 'external-keyboard)
(global-set-key (kbd "C-c k i") 'internal-keyboard)
(global-set-key (kbd "C-c m") 'compile)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o b") 'org-iswitchb)
(global-set-key (kbd "C-c o c") 'org-capture)
(global-set-key (kbd "C-c o l") 'org-store-link)
(global-set-key (kbd "C-c s") 'sort-lines)
(global-set-key (kbd "C-c t") 'sr-speedbar-toggle)
(global-set-key (kbd "C-c u") 'untabify)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c y") 'bury-buffer)
;(global-set-key (kbd "C-c i") 'mu4e)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; Disable ns-win binding of ns-print-buffer, which causes no end of grief.
(global-unset-key (kbd "s-p"))

;; Scroll modestly without moving point.
(setq nurk/modest-scroll-lines 2)
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up nurk/modest-scroll-lines)))
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down nurk/modest-scroll-lines)))

;; Modifier bindings
(defun internal-keyboard ()
  "Define modifers to suit internal keyboard."
  (interactive)
  (setq mac-command-modifier 'meta)		; Command => Meta
  (setq mac-option-modifier 'super)		; Option => Super
  (setq mac-control-modifier 'control)	; Control => Control
  (setq ns-function-modifier 'hyper)	; Fn => Hyper
  (message "Configured for internal keyboard"))

(defun external-keyboard ()
  "Define modifers to suit external keyboard."
  (interactive)
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)
  (setq mac-control-modifier 'control)
  (setq ns-function-modifier 'none)
  (message "Configured for external keyboard"))

;;;; Initialize paths - https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Frame format (emacs-fu.blogspot.com)
(setq frame-title-format
      '("" invocation-name ": "(:eval (if (buffer-file-name)
										  (abbreviate-file-name (buffer-file-name)) "%b"))))

;; Start up the emacs server process.
;(require 'server)
;(unless (server-running-p)
;  (server-start))

;; When visiting a log file, enable tailing.
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))

;; Allow narrowing to proceed without warning.
(put 'narrow-to-region 'disabled nil)

;; Spaceline - https://github.com/TheBB/spaceline
(require 'spaceline-config)
(spaceline-emacs-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode-specific configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Magit - http://magit.vc/
(setq magit-last-seen-setup-instructions "1.4.0")

;;;; Magit Gitflow - https://github.com/jtatarik/magit-gitflow
(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

;;;; Emmet - https://github.com/smihica/emmet-mode
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(eval-after-load "emmet-mode"		;Don't rebind C-j
  '(define-key emmet-mode-keymap (kbd "C-j") nil))

;;;; Web-Mode - http://web-mode.org/
(require 'web-mode)
(setq web-mode-enable-engine-detection t)
(setq web-mode-engines-alist
	  '(("ctemplate" . "\\.html\\'")
		("django" .	"\\.jinja2\\'")
		("django" . "\\.email\\'")))
(add-to-list 'auto-mode-alist '("\\.email\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jinja2\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(defun nurk/web-mode-hook ()
  (setq web-mode-markup-indent-offset 2))
(add-hook 'web-mode-hook 'nurk/web-mode-hook)

;;;; Helm - https://emacs-helm.github.io/helm/
(require 'helm-config)
(helm-mode 1)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

;;;; Projectile - http://batsov.com/projectile/
;; (projectile-global-mode)
;; (setq projectile-completion-system 'helm)
;; (helm-projectile-on)
;; (setq projectile-switch-project-action 'helm-projectile)

;;;; Javacsript JS2 - https://github.com/mooz/js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook (lambda () (setq comment-start "// ")))
(add-hook 'js2-mode-hook #'js2-refactor-mode) ;What is #' ??
(js2r-add-keybindings-with-prefix "C-c r")

;;;; AUCTeX - https://www.gnu.org/software/auctex/
(require 'tex-site)
(setq TeX-auto-save t)					; Enable parse on save.
(setq TeX-parse-self t)					; Enable parse on load.
(setq-default TeX-master nil)			; Query for master file.
(setq TeX-PDF-mode t)					; Default to PDF
(add-hook 'TeX-mode-hook 'flyspell-mode)

;;;; RefTeX - http://www.gnu.org/software/auctex/reftex.html
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)	; For AUCTeX
(setq reftex-plug-into-AUCTeX t)

;;;; Make AUCTeX aware of latexmk
(require 'auctex-latexmk)
(auctex-latexmk-setup)

;;;; Org Mode - http://orgmode.org/
;; (require 'org-mac-link)
;; (add-hook 'org-mode-hook 'flyspell-mode)
;; (add-hook 'org-mode-hook (lambda () (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

;;;; Markdown Mode - http://jblevins.org/projects/markdown-mode/
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;; Elpy - https://github.com/jorgenschaefer/elpy
;; (elpy-enable)
;; (elpy-use-ipython)
;; (pyvenv-workon "brook")

;;;; YASnippet - http://capitaomorte.github.io/yasnippet/
;; (require 'yasnippet)
;; (yas-global-mode 1)

;;;; Miscellaneous

;; Open dash documentation for item at point.
(require 'dash-at-point)

;; Use semantic mode for the speedbar.
(require 'semantic/sb)

;; Use the GIT version of ox-reveal
(add-to-list 'load-path "~/src/org-reveal")
(require 'ox-reveal)

;; Work around emacs 24.3 ispell bug.
(add-hook 'TeX-mode-hook (lambda () (setq-local comment-padding " ")))
(setq ispell-program-name "aspell")

;; Turn on fill-column indicator
(fci-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-theme 'zenburn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)

(add-to-list 'mu4e-view-actions
			 '("ViewInBrowser" . mu4e-action-view-in-browser) t)

(setq nurk/mu4e-account-list
      '(("campus"
		 (user-mail-address "tnurkkala@cse.taylor.edu")
		 (message-signature-file "cse.txt"))
		("cse"
		 (user-mail-address "tnurkkala@cse.taylor.edu")
		 (message-signature-file "cse.txt"))
		("gmail"
		 (user-mail-address "tom.nurkkala@gmail.com")
		 (message-signature-file "gmail.txt"))
		("nurknet"
		 (user-mail-address "tom@nurknet.com")
		 (message-signature-file "nurknet.txt"))
		))

(defun nurk/mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
			  ;; Set account based on parent message (if there is one).
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
			;; Set account by asking user.
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                nurk/mu4e-account-list "/"))
                             (mapcar #'(lambda (var) (car var)) nurk/mu4e-account-list)
                             nil t nil nil (caar nurk/mu4e-account-list))))
		 ;; Look up variables associated with account.
         (account-vars (cdr (assoc account nurk/mu4e-account-list))))
    (if account-vars
		;; Found the account; set up variables.
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      ;; Doh: no such account!
      (error "No email account found"))))

(add-hook 'mu4e-compose-pre-hook 'nurk/mu4e-set-account)

(defun nurk/mu4e-compose-stuff ()
  "My settings for message composition."
  (set-fill-column 72)
  (flyspell-mode))

(add-hook 'mu4e-compose-mode-hook 'nurk/mu4e-compose-stuff)

(defun nurk/mark-spam (msg ignore)
  "Mark messages flagged as spam."
  (with-temp-buffer
    (insert-file-contents (mu4e-message-field msg :path))
    (goto-char (point-min))
    (if (re-search-forward "^X-Spam-Flag: \\(.*\\)" nil t 1)
		(string= (downcase (match-string 1)) "yes")
      nil)))

(add-to-list 'mu4e-headers-custom-markers
			 '("Spam" nurk/mark-spam))
