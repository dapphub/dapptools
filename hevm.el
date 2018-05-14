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

(defvar hevm-dapp-info nil)

(defun hevm-output-filter (string)
  t)

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

