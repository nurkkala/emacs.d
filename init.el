;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tom's Emacs Configuration of Doom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set the location of the customization settings.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; ELPA - Set up packages early so that they can be used from within this file.
(require 'package)
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)
;; After ELPA package initialization, can use locally installed libraries.

;; Paradox -- https://github.com/Malabarba/paradox
(require 'paradox)
(paradox-enable)

(require 's)							;String library

;; Key bindings. From emacs manual: Sequences consisting of C-c and a letter (either upper
;; or lower case) are reserved for users.
(global-set-key (kbd "C-c a") 'align-current)
(global-set-key (kbd "C-c c") 'compile)
;; (global-set-key (kbd "C-c d") 'dash-at-point)
(global-set-key (kbd "C-c e") 'emmet-expand-line)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c i") 'ispell-buffer)
(global-set-key (kbd "C-c k e") 'external-keyboard)
(global-set-key (kbd "C-c k i") 'internal-keyboard)
(global-set-key (kbd "C-c m") 'mu4e)
(global-set-key (kbd "C-c s") 'sort-lines)
(global-set-key (kbd "C-c t") 'sr-speedbar-toggle)
(global-set-key (kbd "C-c u") 'untabify)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c y") 'bury-buffer)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Disable ns-win binding of ns-print-buffer, which causes no end of grief.
(global-unset-key (kbd "s-p"))

;; Scroll modestly without moving point.
(setq nurk/modest-scroll-lines 2)
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up nurk/modest-scroll-lines)))
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down nurk/modest-scroll-lines)))

;; AVY -- https://github.com/abo-abo/avy
(require 'avy)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g e") 'avy-goto-word-0)
(avy-setup-default)						;For isearch

;; ;; Bindings that stick around - https://github.com/abo-abo/hydra/wiki
;; ;; Some bindings are from the hydra examples.
;; (require 'hydra)
;; (require 'org-agenda)					;Access to org-agenda-mode-map

;; (defhydra hydra-org-globals ()
;;   "org"
;;   ("a" org-agenda "agenda")
;;   ("b" org-iswitchb "switch buffer")
;;   ("c" org-capture "capture")
;;   ("l" org-store-link "store link"))
;; (global-set-key (kbd "<f5>") 'hydra-org-globals/body)
;; ;; Was

;; ;; Example 1: text scale
;; (defhydra hydra-zoom ()
;;   "zoom"
;;   ("g" text-scale-increase "in")
;;   ("l" text-scale-decrease "out"))
;; (global-set-key (kbd "<f6>") 'hydra-zoom/body)

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

;; ;; Spaceline - https://github.com/TheBB/spaceline
;; (require 'spaceline-config)
;; (spaceline-emacs-theme)

;; Smart Mode Line - https://github.com/Malabarba/smart-mode-line
(sml/setup)
(load-theme 'smart-mode-line-powerline)

;; Non-ASCII (https://www.emacswiki.org/emacs/FindingNonAsciiCharacters)
(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode-specific configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Magit - http://magit.vc/
(setq magit-last-seen-setup-instructions "1.4.0")

;;;; Magit Gitflow - https://github.com/jtatarik/magit-gitflow
;; (require 'magit-gitflow)
;; (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

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
(add-to-list 'auto-mode-alist '("\\.svg\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jinja2\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(defun nurk/web-mode-hook ()
  (setq web-mode-markup-indent-offset 2))
(add-hook 'web-mode-hook 'nurk/web-mode-hook)

;;;; Helm - https://emacs-helm.github.io/helm/
(defun enable-helm ()
  (require 'helm)

  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)
  (helm-mode 1))

;; (enable-helm)

;;  Ivy -- http://oremacs.com/swiper/
(defun enable-ivy ()
  (require 'ivy)
  (counsel-mode 1)
  (ivy-mode 1)

  (all-the-icons-ivy-setup)

  (require 'ivy-rich)
  (ivy-rich-mode 1))

(enable-ivy)

;; Flyspell
(add-hook 'TeX-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

(require 'flyspell-correct-ivy)
(setq flyspell-correct-interface 'flyspell-correct-ivy)
(define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-word-generic)

;;;; JavaScript JS2 - https://github.com/mooz/js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook (lambda () (setq comment-start "// ")))
;(add-hook 'js2-mode-hook #'js2-refactor-mode) ;What is #' ??
;(js2r-add-keybindings-with-prefix "C-c r")

;;;; AUCTeX - https://www.gnu.org/software/auctex/
(require 'tex-site)
(setq TeX-auto-save t)			; Enable parse on save.
(setq TeX-parse-self t)			; Enable parse on load.
(setq-default TeX-master nil)		; Query for master file.
(setq TeX-PDF-mode t)			; Default to PDF

;;;; RefTeX - http://www.gnu.org/software/auctex/reftex.html
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)	; For AUCTeX
(setq reftex-plug-into-AUCTeX t)

;;;; Make AUCTeX aware of latexmk
(require 'auctex-latexmk)
(auctex-latexmk-setup)

;; Multiple-cursors - https://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;;; Org Mode - http://orgmode.org/
(add-to-list 'load-path "~/Org/elisp")
(require 'org-slides-notes)
(require 'org-schedule-course)

(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o b") 'org-switchb)
(global-set-key (kbd "C-c o c") 'org-capture)
(global-set-key (kbd "C-c o l") 'org-store-link)

;; (require 'org-mac-link)
;; (add-hook 'org-mode-hook (lambda () (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))
(require 'ox)

(require 'ob-mongo "~/.emacs.d/src/ob-mongo/ob-mongo.el")
(add-to-list 'org-babel-load-languages '(mongo . t))

(defun nurk/org-export-filter-src-block (text backend info)
  "Compress multiple blank lines in source block to single line.
This makes source blocks look better on export when they contain
noweb references that are stripped using strip-export"
  (replace-regexp-in-string "^\n+" "\n" text))

(add-to-list 'org-export-filter-src-block-functions 'nurk/org-export-filter-src-block)

(defun nurk/maybe-insert-file (backend)
  "Maybe insert a file at the beginning of an Org file.
Checks the value of buffer-local variable
org-export-top-level-file; if it contains the path to a readable
file and the BACKEND is derived from LaTeX, insert the file at
the beginning of the Org buffer."
  (if (and (local-variable-p 'org-export-top-level-file)
		   (org-export-derived-backend-p backend 'latex))
	  (if (file-readable-p org-export-top-level-file)
		  (progn
			(goto-char (point-min))
			(insert-file-contents org-export-top-level-file))
		(warn "Can't locate top-level file '%s'" org-export-top-level-file))))

(add-hook 'org-export-before-processing-hook 'nurk/maybe-insert-file)

(defun nurk/org-latex-format-headline-function (todo _todo-type priority text tags _info)
  "Format headlines nicely for LaTeX export."
  (concat
   (and todo (format "{\\color{orange}\\footnotesize\\sffamily %s} " todo))
   (and priority (format "\\framebox{\\#%c} " priority))
   text
   (and tags
		(format "\\hfill{}\\textsc{%s}"
				(mapconcat #'org-latex--protect-text tags ":")))))

;;;; Markdown Mode - http://jblevins.org/projects/markdown-mode/
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;; Elpy - https://github.com/jorgenschaefer/elpy
;; (elpy-enable)
;; (elpy-use-ipython)
;; (pyvenv-workon "brook")

;;;; YASnippet - https://github.com/joaotavora/yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;;;; Miscellaneous

;; Open dash documentation for item at point.
;; (require 'dash-at-point)

;; Use semantic mode for the speedbar.
(require 'semantic/sb)

;; Work around emacs 24.3 ispell bug.
(add-hook 'TeX-mode-hook (lambda () (setq-local comment-padding " ")))
(setq ispell-program-name "aspell")

;; Turn on fill-column indicator
(require 'fill-column-indicator)
(fci-mode 1)

;; GIFT mode
(add-to-list 'load-path "~/.emacs.d/gift-mode")
(require 'gift-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (load-theme 'zenburn)
;; (load-theme 'oceanic)
;; (load-theme 'sanityinc-tomorrow-night)
;; (load-theme 'spacemacs-dark)
;; (load-theme 'spacemacs-light)

