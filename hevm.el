;;
;; hevm.el
;;

(defvar hevm-executable-path
  "/Users/mbrock/src/hevm/dist/build/hevm/hevm")

(defvar hevm-root nil
  "Root path of the currently debugged dapp.")

(defvar hevm-buffer nil
  "Process buffer used to communicate with hevm.")

(defvar hevm-stack-buffer nil
  "Buffer that displays the current VM's word stack.")

(defvar hevm-plan '()
  "When the next prompt is ready, we pop the head of this list
and send it as input.")

(defvar hevm-should-setup nil
  "Are we just about to set up the Hevm debugging layout?")

(defvar hevm-dapp-info nil
  "Serialized DappInfo structure for the current dapp.")

(defvar hevm-unit-tests nil
  "List of unit tests for the current dapp.")

(defvar hevm-vm nil
  "The latest VM structure during debugging.")

(defvar hevm-source-map-overlay nil
  "Overlay that moves around in the debugged source code buffers.")

(define-derived-mode hevm-mode comint-mode "Hevm"
  "Major mode for `hevm'."
  nil "Hevm"
  (setq comint-prompt-regexp "^> ")
  (setq comint-prompt-read-only t)
  (add-hook 'comint-output-filter-functions 'hevm-output-filter))

(defun hevm (root json-file)
  "Start a Hevm debugging session for a given dapp."
  (interactive "DDapp root: \nfJSON file: ")
  (when (get-buffer "*hevm*")
    (kill-buffer "*hevm*"))
  (let ((buffer (get-buffer-create "*hevm*")))
    (setq hevm-plan `((load-dapp ,root ,json-file)))
    (make-comint-in-buffer "Hevm" buffer hevm-executable-path nil "emacs")
    (with-current-buffer buffer (hevm-mode))
    (setq hevm-buffer buffer)
    (message "Hevm started.")))

(add-hook 'hevm-mode-hook 'hevm-initialize)
(defun hevm-initialize ()
  "Do Hevm comint minor mode initialization stuff."
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(defun hevm-send (input)
  "Send a command to the Hevm process."
  (with-current-buffer hevm-buffer
    (goto-char (point-max))
    (insert (prin1-to-string input))
    (comint-send-input nil t)))

(defmacro hevm-define-command (name help command)
  "Define a simple Hevm command-sending function."
  (declare (indent defun))
  `(defun ,command ()
     ,help
     (interactive)
     (hevm-send ,command)))

(hevm-define-command hevm-do-step-once
  "Step forward by one opcode."
  (step "once"))

(hevm-define-command hevm-do-step-to-next-source-location
  "Step forward until the source location changes."
  (step "source-location"))

(define-minor-mode hevm-debug-mode
  "Hevm debug minor mode."
  nil
  " Hevm"
  '(("n" . hevm-do-step-once)
    ("N" . hevm-do-step-to-next-source-location)
    ("c" . hevm-browse-contracts)
    ("q" . quit-window))
  :group 'hevm)

(define-minor-mode hevm-browse-contracts-mode
  "Hevm contract browser minor mode."
  nil
  " Hevm-Contracts"
  '(("q" . quit-window))
  :group 'hevm)

(defun hevm-output-filter (string)
  "Hook for the Hevm process output."
  ;; Does the readline prompt occur in the output?
  ;; If so, we should process all the output before it.
  (when (string-match comint-prompt-regexp string)
    (save-excursion
      (with-current-buffer hevm-buffer
	(comint-previous-prompt 1)
	(beginning-of-line)
	(let ((have-something-to-do (looking-at comint-prompt-regexp)))
	  (when have-something-to-do
	    (forward-line 1)
	    (hevm-handle-output (read hevm-buffer))))))
    (hevm-follow-plan)
    (comint-next-prompt 1)))

(defun hevm-handle-output (thing)
  "React to a Hevm process output s-expression."
  (pcase thing

    ;; Incoming dapp load confirmation with unit test list.
    (`(dapp-info (root ,root) (unit-tests . ,tests))
     (setq hevm-root (file-name-as-directory root))
     (setq hevm-unit-tests tests)
     (run-with-idle-timer 0 nil #'hevm-run-test))

    ;; Incoming new VM step information.
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

    ;; We sent some command that Hevm didn't understand.
    ('(unrecognized-command)
     (error "Unrecognized Hevm input command."))

    ;; We got some weird stuff from Hevm.
    (_
     (message "Unknown Hevm output: %S" thing))))

(defun hevm-follow-plan ()
  "Pop and send a command from the Hevm plan queue, if possible."
  (when hevm-plan
    (hevm-send (car hevm-plan))
    (setq hevm-plan (cdr hevm-plan))))

(defun hevm-run-test ()
  "Pick a dapp unit test and load it into Hevm."
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

(defun hevm-highlight-source-region (offset length jump-type)
  "Move the Hevm source map overlay to some range in the current buffer."
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

(defun hevm-update (vm)
  "Use a new EVM state and update live buffers like the stack viewer."
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

(defun hevm-browse-contracts ()
  "Open a buffer that lists all the contracts in the current EVM.
This buffer is non-live; you have to refresh it to see new state."
  (interactive)
  (let ((buffer (get-buffer-create "*hevm contracts*")))
    (switch-to-buffer buffer)
    (read-only-mode 1)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max))
      (org-mode)
      (hevm-browse-contracts-mode)
      (dolist (contract (cadr (assoc 'contracts hevm-vm)))
	(let* ((address (car contract))
	       (fields (cadr contract))
	       (storage (cadr (assoc 'storage fields)))
	       (balance (cadr (assoc 'balance fields)))
	       (nonce (cadr (assoc 'nonce fields)))
	       (codehash (cadr (assoc 'codehash fields))))
	  (insert "* " address "\n")
	  (insert "  Balance:  " balance "\n")
	  (insert "  Nonce:    " nonce "\n")
	  (insert "  Codehash: " codehash "\n")
	  (when storage
	    (insert "\n** Storage\n")
	    (dolist (x storage)
	      (pcase (car x)
		(`(hash ,k ,bs)
		 (insert "   - "
			 (format "%S" k) " = " (cadr x) "\n")
		 ;; Insert the hash preimage.
		 ;; This isn't so useful right now.
		 ;; Hevm needs better preimage tracking.
		 (insert "     [" bs "]"))
		(_
		 (insert "   - " (car x) " = " (cadr x) "\n")))))
	  (insert "\n"))))
    (goto-char (point-min))))
