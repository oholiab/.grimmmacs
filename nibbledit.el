(defvar nibble-hilights nil "nibble mode syntax highlighting")
(setq nibble-raw-hex-start 11)
(setq nibble-raw-hex-end 18)
(setq nibble-raw-hex-padded-end 16)

(setq nibble-highlights
			'(("^[[:xdigit:]]\\{8\\}:" . font-lock-function-name-face)
				("\\<[[:xdigit:]]\\{5\\}\\>" . font-lock-warning-face)
				("\\<[[:xdigit:]]\\{3\\}\\>" . font-lock-warning-face)
				("  .+$" . font-lock-keyword-face)
				))

(defun nibble-mode ()
	(interactive)
	(kill-all-local-variables)
	(setq-local major-mode 'nibble-mode)
	(setq-local mode-name "nibble-mode")
	;; (use-local-map)
	;; (setq nibble-mode-map)
	(setq font-lock-defaults '(nibble-highlights))
	)

(defun nibble-hexload (filename &optional buffer)
  "Load a binary file as a hexdump into specified buffer (current buffer by default)

  Creates a new buffer if it does not already exist"
  (interactive "fLoad binary file: ")
  (if (not buffer)
      (nibble-hexload filename (current-buffer))
    (with-current-buffer (get-buffer-create buffer)
      (insert (shell-command-to-string (format "xxd %s" filename))))
    (switch-to-buffer buffer)))

(defun nibble-hexreload (filename)
  (interactive "fLoad binary file: ")
  (erase-buffer)
  (nibble-hexload filename (current-buffer)))

(defun nibble-hexsave (filename)
  (interactive "sFilename: ")
  (shell-command-on-region (point-min) (point-max) (format "xxd -r > %s" filename) standard-output))

(defun nibble-delete-trailing-newlines ()
  "Deletes all blank lines at the end of the file, even the last one"
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines)
      (let ((trailnewlines (abs (skip-chars-backward "\n\t"))))
        (if (> trailnewlines 0)
            (progn
              (delete-char trailnewlines)))))))


(defun nibble-select-raw-hex ()
  "Select just the raw hex section of the dump"
  (interactive)
  (setq deactivate-mark nil)
  (nibble-delete-trailing-newlines)
  (let ((raw-hex-start nibble-raw-hex-start)
        (raw-hex-end (- (point-max) nibble-raw-hex-padded-end)))
    (set-mark raw-hex-start)
    (goto-char raw-hex-end))
  (activate-mark))

(defun nibble-copy-raw-hex ()
  "Put raw hex in the rectangle copy buffer to be pasted with `yank-rectangle`"
  (interactive)
  (nibble-select-raw-hex)
  (copy-rectangle-as-kill (mark) (point)))

(defun nibble-kill-all-whitespace ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "[ \t\r\n]+" nil t)
    (replace-match "" nil nil)))

(defun nibble-even-nibbles ()
  "Dump an extra zeroed nibble at the end if there are an odd number"
  (interactive)
  (if (not (zerop (% (point-max) 2)))
      (progn
        (message (format "Odd number of nibbles, appending 0: %d (was %d)" (+ (point-max) 1) (point-max)))
        (goto-char (point-max))
        (insert "0"))
    (message (format "Even number of nibbles, doing nothing: %d" (point-max)))))

(defun nibble-xxd-p-format-buffer ()
  (interactive)
  (nibble-kill-all-whitespace)
  (nibble-even-nibbles)
  (goto-char (point-min))
  (while (< (+ (point) 60) (point-max))
    (goto-char (+ (point) 60))
    (insert "\n")))

(defun nibble-copy-raw-hex-to-processing-buffer ()
  (interactive)
     (nibble-copy-raw-hex)
     (switch-to-buffer "hexproc")
     (yank-rectangle))

(defun nibble-hexsave-raw (filename)
  (interactive "sFilename: ")
  (nibble-copy-raw-hex-to-processing-buffer)
  (nibble-xxd-p-format-buffer)
  (shell-command-on-region (point-min) (point-max) (format "xxd -p -r > %s" filename) standard-output))

(defun nibble-hex-save-and-reload (filename)
  (interactive "sFilename: ")
  (let ((currbuff (current-buffer)))
    (nibble-hexsave-raw filename)
    (switch-to-buffer currbuff))
  (kill-buffer "hexproc")
  (nibble-hexreload filename))
