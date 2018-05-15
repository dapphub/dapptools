;;
;; hevm.el
;;

(defvar hevm-executable-path
  "/Users/mbrock/src/hevm/dist/build/hevm/hevm")

(defvar hevm-root nil)
(defvar hevm-buffer nil)

(defun hevm ()
  (interactive)
  (kill-buffer "*hevm*")
  (let ((buffer (get-buffer-create "*hevm*")))
    (make-comint-in-buffer
     "Hevm" buffer hevm-executable-path nil
     "emacs")
    (switch-to-buffer-other-window buffer)
    (with-current-buffer buffer
      (hevm-mode))
    (setq hevm-buffer buffer)
    (message "Hevm started.")))
  
(define-derived-mode hevm-mode comint-mode "Hevm"
  "Major mode for `hevm'."
  nil "Hevm"
  (setq comint-prompt-regexp "^> ")
  (setq comint-prompt-read-only t)
  (add-hook 'comint-output-filter-functions 'hevm-output-filter))

(defun hevm-initialize ()
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(add-hook 'hevm-mode-hook 'hevm-initialize)

(defvar hevm-newest-output nil)
(defvar hevm-dapp-info nil)
(defvar hevm-unit-tests nil)

(defun hevm-output-filter (string)
  (when (string-match comint-prompt-regexp string)
    (save-excursion
      (with-current-buffer hevm-buffer
	(comint-previous-prompt 1)
	(beginning-of-line)
	(when (looking-at comint-prompt-regexp)
	  (forward-line 1)
	  (setq hevm-newest-output (read hevm-buffer))
	  (hevm-handle-output hevm-newest-output))))))

(defvar hevm-source-map-overlay nil)

(defun hevm-highlight-source-region (offset length)
  (let* ((start (+ 1 offset))
	 (end (+ length start)))
    (if hevm-source-map-overlay
	(move-overlay hevm-source-map-overlay start end (current-buffer))
      (setq hevm-source-map-overlay
	  (make-overlay start end (current-buffer)))
    (overlay-put hevm-source-map-overlay
		 'face
		 '(background-color . "pink")))))

(define-minor-mode hevm-debug-mode
  "Hevm debug mode."
  nil
  " Hevm"
  '(("n" . hevm-do-step-once))
  :group 'hevm)

(defun hevm-do-step-once ()
  (interactive)
  (hevm-send '(step-once)))

(defun hevm-handle-output (thing)
  (pcase thing
    (`(dapp-info (root ,root) (unit-tests . ,tests))
     (setq hevm-root (file-name-as-directory root))
     (setq hevm-unit-tests tests))
    (`(step (pc ,pc) (file ,file) (srcmap ,offset ,length ,jump-type))
     (find-file-read-only (concat hevm-root file))
     (hevm-debug-mode)
     (goto-char (+ 1 offset))
     (hevm-highlight-source-region offset length)
     (recenter))
    ('(ok)
     (message "Hevm is being lazy."))
    ('(unrecognized-command)
     (error "Unrecognized Hevm input command."))
    (_
     (message "Unknown Hevm output: %S" thing))))

(defun hevm-send (input)
  (with-current-buffer hevm-buffer
    (goto-char (point-max))
    (insert (prin1-to-string input))
    (comint-send-input nil t)))

(defun hevm-send-dummy ()
  (interactive)
  (hevm-send '(dummy)))

(defun hevm-load (root json-file)
  (interactive "DDapp root: \nfJSON file: ")
  (hevm-send `(load-dapp ,root ,json-file)))

(defun hevm-test ()
  (interactive)
  (hevm-load "/Users/mbrock/src/ds-token"
	     "/Users/mbrock/src/ds-token/out/token.t.sol.json"))
