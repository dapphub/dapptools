;;
;; hevm.el
;;

(defvar hevm-executable-path
  "/Users/mbrock/src/hevm/dist/build/hevm/hevm")

(defvar hevm-root nil)
(defvar hevm-buffer nil)
(defvar hevm-plan '())
(defvar hevm-should-setup nil)

(defun hevm (root json-file)
  (interactive "DDapp root: \nfJSON file: ")
  (when (get-buffer "*hevm*")
    (kill-buffer "*hevm*"))
  (let ((buffer (get-buffer-create "*hevm*")))
    (setq hevm-plan `((load-dapp ,root ,json-file)))
    (make-comint-in-buffer
     "Hevm" buffer hevm-executable-path nil
     "emacs")
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
(defvar hevm-source-map-overlay nil)
(defvar hevm-vm nil)

(defun hevm-output-filter (string)
  (when (string-match comint-prompt-regexp string)
    (save-excursion
      (with-current-buffer hevm-buffer
	(comint-previous-prompt 1)
	(beginning-of-line)
	(when (looking-at comint-prompt-regexp)
	  (forward-line 1)
	  (setq hevm-newest-output (read hevm-buffer))
	  (hevm-handle-output hevm-newest-output))))
    (when hevm-plan
      (hevm-send (car hevm-plan))
      (setq hevm-plan (cdr hevm-plan)))))

(defun hevm-highlight-source-region (offset length jump-type)
  (let* ((start (+ 1 offset))
	 (end (+ length start))
	 (color (pcase jump-type
		  ('JumpInto "lightgreen")
		  ('JumpRegular nil)
		  ('JumpFrom "lightblue"))))
    (message color)
    (if hevm-source-map-overlay
	(move-overlay hevm-source-map-overlay start end (current-buffer))
      (setq hevm-source-map-overlay
	    (make-overlay start end (current-buffer))))
    (overlay-put hevm-source-map-overlay
		 'face
		 `((weight . "bold")
		   (background-color . ,color)))))

(define-minor-mode hevm-debug-mode
  "Hevm debug mode."
  nil
  " Hevm"
  '(("n" . hevm-do-step-once)
    ("N" . hevm-do-step-to-next-source-location))
  :group 'hevm)

(defun hevm-do-step-once ()
  (interactive)
  (hevm-send '(step "once")))

(defun hevm-do-step-to-next-source-location ()
  (interactive)
  (hevm-send '(step "source-location")))

(defun hevm-run-test ()
  (interactive)
  (let* ((contract-name
	  (completing-read
	   "Contract: "
	   (mapcar #'car hevm-unit-tests)))
	 (test
	  (completing-read
	   "Unit test: "
	   (cadr (assoc contract-name hevm-unit-tests)))))
    (setq hevm-should-setup t)
    (hevm-send `(run-test ,contract-name ,test))))

(defvar hevm-stack-buffer)

(defun hevm-update (vm)
  (setq hevm-vm vm)
  (setq hevm-stack-buffer (get-buffer-create "*hevm stack*"))
  (let* ((state (cadr (assoc 'state hevm-vm)))
	 (stack (cadr (assoc 'stack state))))
    (with-current-buffer hevm-stack-buffer
      (delete-region (point-min) (point-max))
      (let ((i 1))
	(dolist (word stack)
	  (insert (format "(%S) %S\n" i word))
	  (setf i (+ i 1))))
      (fit-window-to-buffer
       (get-buffer-window hevm-stack-buffer)
       16 6))))

(defun hevm-handle-output (thing)
  (pcase thing
    (`(dapp-info (root ,root) (unit-tests . ,tests))
     (setq hevm-root (file-name-as-directory root))
     (setq hevm-unit-tests tests)
     (run-with-idle-timer 0 nil #'hevm-run-test))
    (`(step (vm ,vm) (file ,file) (srcmap ,offset ,length ,jump-type))
     (hevm-update vm)
     (find-file-read-only (concat hevm-root file))
     (hevm-debug-mode)
     (goto-char (+ 1 offset))
     (hevm-highlight-source-region offset length jump-type)
     (recenter)
     (when hevm-should-setup
       (setq hevm-should-setup nil)
       (delete-other-windows)
       (split-window)
       (switch-to-buffer hevm-stack-buffer)
       (fit-window-to-buffer)
       (other-window 1)))
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
