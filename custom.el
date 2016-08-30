(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-complete-expert-commands t)
 '(TeX-view-program-list (quote (("Skim" "open -a Skim %o"))))
 '(TeX-view-program-selection
   (quote
	((output-pdf "Skim")
	 ((output-dvi style-pstricks)
	  "dvips and gv")
	 (output-dvi "xdvi")
	 (output-pdf "Evince")
	 (output-html "xdg-open"))))
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(bibtex-dialect (quote biblatex))
 '(coffee-tab-width 4)
 '(column-number-mode t)
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
	("dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "cdbd0a803de328a4986659d799659939d13ec01da1f482d838b68038c1bb35e8" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "708df3cbb25425ccbf077a6e6f014dc3588faba968c90b74097d11177b711ad1" "68d36308fc6e7395f7e6355f92c1dd9029c7a672cbecf8048e2933a053cf27e6" "0ad5a61e6ee6d2e7f884c0da7a6f437a4c84547514b509bdffd06757a8fc751f" "df3e05e16180d77732ceab47a43f2fcdb099714c1c47e91e8089d2fcf5882ea3" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "11636897679ca534f0dec6f5e3cb12f28bf217a527755f6b9e744bd240ed47e1" "19352d62ea0395879be564fc36bc0b4780d9768a964d26dfae8aad218062858d" "a444b2e10bedc64e4c7f312a737271f9a2f2542c67caa13b04d525196562bf38" "2e5705ad7ee6cfd6ab5ce81e711c526ac22abed90b852ffaf0b316aa7864b11f" "b06aaf5cefc4043ba018ca497a9414141341cb5a2152db84a9a80020d35644d1" "3dafeadb813a33031848dfebfa0928e37e7a3c18efefa10f3e9f48d1993598d3" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "6a9606327ecca6e772fba6ef46137d129e6d1888dcfc65d0b9b27a7a00a4af20" default)))
 '(desktop-save-mode t)
 '(display-time-mode nil)
 '(elpy-mode-hook (quote (hl-line-mode)))
 '(emmet-indentation 2)
 '(f90-auto-keyword-case (quote upcase-word))
 '(fci-rule-color "#383838")
 '(fill-column 79)
 '(gnutls-min-prime-bits 1024)
 '(helm-man-or-woman-function (quote woman))
 '(helm-reuse-last-window-split-state t)
 '(ibuffer-formats
   (quote
	((mark modified read-only " "
		   (name 18 18 :left :elide)
		   " "
		   (size 9 -1 :right)
		   " "
		   (mode 16 16 :left :elide)
		   " " filename-and-process)
	 (mark modified read-only " "
		   (name 25 25 :left :elide)
		   " "
		   (size 8 -1 :right)
		   " "
		   (mode 10 10)
		   " " filename-and-process))))
 '(ibuffer-saved-filter-groups
   (quote
	(("Tom's Filter Group"
	  ("Python"
	   (mode . python-mode))
	  ("JavaScript"
	   (mode . js2-mode))
	  ("HTML/Web"
	   (or
		(used-mode . html-mode)
		(used-mode . css-mode)
		(used-mode . web-mode)))))))
 '(ibuffer-saved-filters
   (quote
	(("gnus"
	  ((or
		(mode . message-mode)
		(mode . mail-mode)
		(mode . gnus-group-mode)
		(mode . gnus-summary-mode)
		(mode . gnus-article-mode))))
	 ("programming"
	  ((or
		(mode . emacs-lisp-mode)
		(mode . cperl-mode)
		(mode . c-mode)
		(mode . java-mode)
		(mode . idl-mode)
		(mode . lisp-mode)))))))
 '(ido-enable-tramp-completion nil)
 '(indicate-buffer-boundaries (quote left))
 '(indicate-empty-lines t)
 '(jedi:complete-on-dot t)
 '(jedi:setup-keys t)
 '(jinja2-user-keywords (quote ("topic" "section" "subsection")))
 '(js2-basic-offset 2)
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate")))
 '(markdown-command
   "/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin/markdown_py")
 '(message-citation-line-function (quote message-insert-formatted-citation-line))
 '(message-signature-directory "~/.signatures")
 '(message-signature-file "nurknet.txt")
 '(minimap-update-delay 0.4)
 '(minimap-window-location (quote right))
 '(mu4e-bookmarks
   (quote
	(("date:24h..now and flag:unread and not flag:trashed" "Last 24 Hours (Unread)" 50)
	 ("date:48h..now and flag:unread and not flag:trashed" "Last 48 Hours (Unread)" 52)
	 ("date:today..now and not flag:trashed" "Today (All)" 116)
	 ("date:7d..now and flag:unread" "Last 7 Days (Unread)" 119)
	 ("date:30d..now and flag:unread" "Last 30 Days (Unread)" 109)
	 ("flag:unread AND NOT flag:trashed" "Unread (All)" 117))))
 '(mu4e-compose-dont-reply-to-self t)
 '(mu4e-compose-signature t)
 '(mu4e-compose-signature-auto-include nil)
 '(mu4e-get-mail-command "/opt/local/bin/offlineimap")
 '(mu4e-headers-leave-behavior (quote apply))
 '(mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")
 '(mu4e-maildir "/Users/tom/Mail")
 '(mu4e-mu-binary "/usr/local/bin/mu")
 '(mu4e-update-interval 300)
 '(mu4e-user-mail-address-list
   (quote
	("tom.nurkkala@gmail.com" "thnurkkala@taylor.edu" "tnurkkala@cse.taylor.edu" "tom@nurknet.com" "thnurkkala@tayloru.edu")))
 '(mu4e-view-show-images t)
 '(newsticker-url-list
   (quote
	(("Slashdot" "http://rss.slashdot.org/Slashdot/slashdot" nil nil nil))))
 '(ns-tool-bar-display-mode (quote both) t)
 '(ns-tool-bar-size-mode (quote regular) t)
 '(org-agenda-files
   (quote
	("~/Taylor/Classes/2013-2014/Spring/SYS 394 - ISD/schedule.org" "~/Org/todo.org")))
 '(org-agenda-start-on-weekday 0)
 '(org-babel-load-languages
   (quote
	((latex . t)
	 (python . t)
	 (sh . t)
	 (emacs-lisp . t))))
 '(org-checkbox-hierarchical-statistics nil)
 '(org-clock-continuously nil)
 '(org-clock-into-drawer t)
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-confirm-babel-evaluate nil)
 '(org-default-notes-file "~/Org/refile.org")
 '(org-export-with-drawers t)
 '(org-hierarchical-checkbox-statistics nil)
 '(org-latex-classes
   (quote
	(("article" "\\documentclass[11pt]{article}"
	  ("\\section{%s}" . "\\section*{%s}")
	  ("\\subsection{%s}" . "\\subsection*{%s}")
	  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	  ("\\paragraph{%s}" . "\\paragraph*{%s}")
	  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
	 ("report" "\\documentclass[11pt]{report}"
	  ("\\part{%s}" . "\\part*{%s}")
	  ("\\chapter{%s}" . "\\chapter*{%s}")
	  ("\\section{%s}" . "\\section*{%s}")
	  ("\\subsection{%s}" . "\\subsection*{%s}")
	  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
	 ("book" "\\documentclass[11pt]{book}"
	  ("\\part{%s}" . "\\part*{%s}")
	  ("\\chapter{%s}" . "\\chapter*{%s}")
	  ("\\section{%s}" . "\\section*{%s}")
	  ("\\subsection{%s}" . "\\subsection*{%s}")
	  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
	 ("syllabus" "\\documentclass{article}"
	  ("\\section{%s}" . "\\section*{%s}")
	  ("\\subsection{%s}" . "\\subsection*{%s}")))))
 '(org-log-done (quote note))
 '(org-log-into-drawer t)
 '(org-log-note-clock-out t)
 '(org-mobile-files (quote (org-agenda-files "tenure-app.org")))
 '(org-reveal-center nil)
 '(org-reveal-history t)
 '(org-reveal-margin "0.1")
 '(org-reveal-mathjax t)
 '(org-reveal-root "http://cdn.jsdelivr.net/reveal.js/2.5.0/")
 '(org-reveal-theme "beige")
 '(org-src-fontify-natively t)
 '(org-src-preserve-indentation t)
 '(org-startup-folded (quote content))
 '(org-startup-indented t)
 '(recentf-mode t)
 '(reftex-plug-into-AUCTeX t)
 '(reftex-ref-style-default-list (quote ("Default" "Hyperref")))
 '(rw-hunspell-dicpath-list (quote ("/opt/local/share/hunspell")))
 '(safe-local-variable-values
   (quote
	((TeX-master . qrs-tee-erd-hw)
	 (engine . django)
	 (pyvenv-workon . brook)
	 (TeX-engine . "lualatex")
	 (eval when
		   (fboundp
			(quote rainbow-mode))
		   (rainbow-mode 1)))))
 '(save-place t nil (saveplace))
 '(send-mail-function (quote smtpmail-send-it))
 '(shell-escape-mode "-shell-escape")
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(smtpmail-queue-dir "~/Mail/queue/cur")
 '(smtpmail-queue-mail nil)
 '(smtpmail-smtp-server "smtp.webfaction.com")
 '(smtpmail-smtp-service 587)
 '(speedbar-default-position (quote right))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(user-mail-address "tom@nurknet.com")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
	((20 . "#BC8383")
	 (40 . "#CC9393")
	 (60 . "#DFAF8F")
	 (80 . "#D0BF8F")
	 (100 . "#E0CF9F")
	 (120 . "#F0DFAF")
	 (140 . "#5F7F5F")
	 (160 . "#7F9F7F")
	 (180 . "#8FB28F")
	 (200 . "#9FC59F")
	 (220 . "#AFD8AF")
	 (240 . "#BFEBBF")
	 (260 . "#93E0E3")
	 (280 . "#6CA0A3")
	 (300 . "#7CB8BB")
	 (320 . "#8CD0D3")
	 (340 . "#94BFF3")
	 (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(visual-line-mode nil t)
 '(web-mode-markup-indent-offset 2)
 '(which-function-mode nil)
 '(woman-fill-frame t)
 '(woman-manpath
   (quote
	("/opt/local/share/man" "/usr/share/man" "/usr/local/share/man" "/usr/X11/man"
	 ("/bin" . "/usr/share/man")
	 ("/sbin" . "/usr/share/man")
	 ("/usr/bin" . "/usr/share/man")
	 ("/usr/sbin" . "/usr/share/man")
	 ("/usr/local/bin" . "/usr/local/share/man")
	 ("/usr/local/sbin" . "/usr/local/share/man")
	 ("/usr/X11/bin" . "/usr/X11/man")
	 ("/usr/bin/X11" . "/usr/X11/man")
	 ("/usr/bin/mh" . "/usr/share/man")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3F3F3F" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Menlo"))))
 '(html-mode-default ((t (:inherit sgml-mode-default :height 140 :foundry "Monaco" :family "apple"))) t)
 '(minimap-active-region-background ((t (:background "#555"))))
 '(minimap-font-face ((t (:height 0.3 :family "DejaVu Sans Mono")))))
