;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Try out WanderLust
; (require 'wl)

;; Using this strategy to install mu/mu4e:
;;   http://blog.danielgempesaw.com/post/43467552978/installing-mu-and-mu4e-with-homebrew-with-emacs
;; See also
;;   http://pragmaticemacs.com/emacs/master-your-inbox-with-mu4e-and-org-mode/
;;   http://www.macs.hw.ac.uk/~rs46/posts/2014-01-13-mu4e-email-client.html

(add-to-list 'load-path "/usr/local/Cellar/mu/1.0/share/emacs/site-lisp/mu/mu4e")
(require 'mu4e)
(require 'org-mu4e)

;; Some settings that I've turned off for one reason or another.
;; (setq mu4e-headers-leave-behavior 'apply)
;; (add-hook 'mu4e-compose-mode-hook 'org~mu4e-mime-switch-headers-or-body)
;; (setq org-mu4e-convert-to-html t)

(setq org-mu4e-link-query-in-headers-mode nil)

(add-to-list 'mu4e-view-actions
			 '("ViewInBrowser" . mu4e-action-view-in-browser) t)

(setq nurk/fancy-sig (concat "Tom Nurkkala, PhD\n"
							 "Associate Professor, Computer Science and Engineering\n"
							 "Director, Center for Missions Computing\n"
							 "Taylor University\n"))
(setq nurk/simple-sig "Tom Nurkkala\n")

(setq mu4e-contexts
	  `( ,(make-mu4e-context
		   :name "Nurk Net"
		   :match-func (lambda (msg)
						 (when msg
						   (mu4e-message-contact-field-matches msg :to "@nurknet.com")))
		   :vars `((user-mail-address . "tom@nurknet.com")
				   (mu4e-sent-folder . "/nurknet/Sent")
				   (mu4e-trash-folder . "/nurknet/Trash")
				   (mu4e-refile-folder . "/nurknet/Archive")
				   (mu4e-compose-signature . ,nurk/simple-sig)))
		 ,(make-mu4e-context
		   :name "Department"
		   :match-func (lambda (msg)
						 (when msg
						   (mu4e-message-contact-field-matches msg :to "@cse.taylor.edu")))
		   :vars `((user-mail-address . "tnurkkala@cse.taylor.edu")
				   (mu4e-sent-folder . "/cse/Sent")
				   (mu4e-trash-folder . "/cse/Trash")
				   (mu4e-refile-folder . "/cse/Archive")
				   (mu4e-compose-signature . ,nurk/fancy-sig)))
		 ,(make-mu4e-context
		   :name "Campus"
		   :match-func (lambda (msg)
						 (when msg
						   (mu4e-message-contact-field-matches msg :to "@taylor.edu")))
		   :vars `((user-mail-address . "thnurkkala@taylor.edu")
				   (mu4e-sent-folder . "/campus/Sent")
				   (mu4e-trash-folder . "/campus/Trash")
				   (mu4e-refile-folder . "/campus/Archive")
				   (mu4e-compose-signature . ,nurk/fancy-sig)))
		 ,(make-mu4e-context
		   :name "Gmail"
		   :match-func (lambda (msg)
						 (when msg
						   (mu4e-message-contact-field-matches msg :to "@gmail.com")))
		   :vars `((user-mail-address . "tom.nurkkala@gmail.com")
				   (mu4e-sent-folder . "/gmail/[Gmail].Sent Mail")
				   (mu4e-trash-folder . "/gmail/[Gmail].Trash")
				   (mu4e-refile-folder . "/gmail/[Gmail].Archive")
				   (mu4e-compose-signature . ,nurk/simple-sig)))
		 ))

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

;; Stuff from before mu4e added contexts

;; (setq nurk/mu4e-account-list
;;       '(("campus"
;; 		 (user-mail-address "tnurkkala@cse.taylor.edu")
;; 		 (message-signature-file "cse.txt"))
;; 		("cse"
;; 		 (user-mail-address "tnurkkala@cse.taylor.edu")
;; 		 (message-signature-file "cse.txt"))
;; 		("gmail"
;; 		 (user-mail-address "tom.nurkkala@gmail.com")
;; 		 (message-signature-file "gmail.txt"))
;; 		("nurknet"
;; 		 (user-mail-address "tom@nurknet.com")
;; 		 (message-signature-file "nurknet.txt"))
;; 		))

;; (defun nurk/mu4e-set-account ()
;;   "Set the account for composing a message."
;;   (let* ((account
;;           (if mu4e-compose-parent-message
;; 			  ;; Set account based on parent message (if there is one).
;;               (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
;;                 (string-match "/\\(.*?\\)/" maildir)
;;                 (match-string 1 maildir))
;; 			;; Set account by asking user.
;;             (completing-read (format "Compose with account: (%s) "
;;                                      (mapconcat #'(lambda (var) (car var))
;;                                                 nurk/mu4e-account-list "/"))
;;                              (mapcar #'(lambda (var) (car var)) nurk/mu4e-account-list)
;;                              nil t nil nil (caar nurk/mu4e-account-list))))
;; 		 ;; Look up variables associated with account.
;;          (account-vars (cdr (assoc account nurk/mu4e-account-list))))
;;     (if account-vars
;; 		;; Found the account; set up variables.
;;         (mapc #'(lambda (var)
;;                   (set (car var) (cadr var)))
;;               account-vars)
;;       ;; Doh: no such account!
;;       (error "No email account found"))))

;; (add-hook 'mu4e-compose-pre-hook 'nurk/mu4e-set-account)
