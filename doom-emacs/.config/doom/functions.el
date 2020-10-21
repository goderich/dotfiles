;; Various functions for my personal use

(defun prepend-numbers (numlines)
  "Prepend numbers to the beginning of lines.
   Numbers start with 1 and increase by one
   each time. The argument specifies the total
   amount of lines to prepend the numerals to.
   The numbers are followed by a dot and whitespace."
  (interactive "sPrepend how many times? ")
  (beginning-of-line)
  (dotimes (i (string-to-number numlines))
    (insert (format "%d. " (1+ i)))
    (forward-line)
    (beginning-of-line)))

(defun insert-numbers (numlines)
  "Insert new lines with incrementing numbers.
   Numbers start with 1 and increase by one
   each time. The argument specifies the total
   amount of lines to create."
  (interactive "sInsert how many lines? ")
  (beginning-of-line)
  (dotimes (i (string-to-number numlines))
    (insert (format "%d\n" (1+ i)))))

(defun md-table->latex ()
  "Convert a Markdown table preceding the point into LaTeX."
  (interactive)
  (defun convert-row ()
    (let ((md-asterisk (rx "\\" "\*"))
          (md-bold (rx (repeat 2 "\*")
                       word-start
                       (group (minimal-match (zero-or-more (not (any "\&")))))
                       word-end
                       (repeat 2 "\*")))
          (md-opening-quote (rx " '" word-start)))
      (delete-char 2)
      (while (re-search-forward "|" (1- (line-end-position)) t)
        (replace-match "&"))
      (re-search-forward "|")
      (replace-match "\\\\ \\addlinespace" t t)
      (beginning-of-line)
      (while (re-search-forward md-asterisk (1- (line-end-position)) t)
        (replace-match "\\\\text{\*}"))
      (beginning-of-line)
      (while (re-search-forward md-bold (1- (line-end-position)) t)
        (replace-match "\\\\textbf{\\1}"))
      (beginning-of-line)
      (while (re-search-forward md-opening-quote (1- (line-end-position)) t)
        (replace-match " \`"))
      (forward-line)))
  (progn
    (re-search-backward "^:")
    (skip-chars-forward "[: ]")
    (setq table-caption
          (replace-regexp-in-string
           "\\\\\\*"  "\\\\text{\*}"
           (delete-and-extract-region
            (point)
            (- (search-forward " {#") 3))))
    (setq table-label
          (delete-and-extract-region
           (point)
           (1- (line-end-position))))
    (beginning-of-line)
    (delete-region (point) (progn (forward-line 1) (point)))
    (insert
     "\\begin{table}[hbt!]\n"
     "\\centering\n"
     "\\caption{" table-caption "}\n"
     "\\label{" table-label "}\n")
    (setq table-top (point))
    (forward-line)
    (setq table-columns
          (1- (cl-count ?| (thing-at-point 'line))))
    (convert-row)
    (re-search-backward "addlinespace")
    (replace-match "midrule \\\\addlinespace")
    (forward-line)
    (delete-region (point) (progn (forward-line 1) (point)))
    (while (< 0 (cl-count ?| (thing-at-point 'line)))
      (convert-row))
    (setq table-bottom (point))
    (re-search-backward "addlinespace")
    (replace-match "bottomrule")
    (forward-line)
    (insert
     "\\end{tabular}\n"
     "\\end{table}\n")
    (goto-char table-top)
    (insert
     "\\begin{tabular}{ "
     (apply 'concat (make-list table-columns "l "))
     "} \\toprule")
    (re-search-backward "]")))

;; This function is from https://github.com/kawabata/ox-pandoc/issues/51
(defun tb/ox-pandoc-fix-export-blocks (backend)
  "Wrap all export blocks with =BEGIN_EXPORT org=, so ox-pandoc does not remove them"
  (when (or (eq backend 'pandoc) (eq backend 'org))
    (let ((blocks
           (org-element-map
               (org-element-parse-buffer)
               'export-block
             'identity)))
      (seq-map
       (lambda (b)
         (unless (string= (org-element-property :type b) "ORG")
           (goto-char (org-element-property :end b))
           (insert "#+END_EXPORT\n")
           (goto-char (org-element-property :begin b))
           (insert "#+BEGIN_EXPORT org\n,")))
       (reverse blocks)))))

(defun gd/send-confirm-has-recipient ()
  "Confirm that the addressee field is not empty before sending."
  (interactive)
  (if (not (message-field-value "To"))
      (message "Empty addressee field!")
    (org-msg-ctrl-c-ctrl-c)))
