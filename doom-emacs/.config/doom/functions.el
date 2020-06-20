;; Various functions for my personal use

(defun prepend-numbers (numlines)
  "Prepend numbers to the beginning of lines.
   Numbers start with 1 and increase by one
   each time. The argument specifies the total
   amount of lines to prepend the numerals to.
   The numbers are followed by a dot and whitespace."
  (interactive "sPrepend how many times? ")
  ;; (beginning-of-line)
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
  ;; (beginning-of-line)
  (dotimes (i (string-to-number numlines))
    (insert (format "%2d\n" (1+ i)))))
