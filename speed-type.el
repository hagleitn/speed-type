;;; speed-type.el --- Practice touch and speed typing

;; Copyright 2015 Gunther Hagleitner

;; Author: Gunther Hagleitner
;; Keywords: TODO
;; URL: TODO
;; Package-Requires: ()

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This allows you to practice your touch typing skills on any buffer
;; or region. You can also type random samples from
;; ebooks. speed-type also keeps track of your stats (WPM, CPM, etc)

;;; Code:

(require 'url)

(defun speed-type--seconds-to-minutes (seconds)
  (/ seconds 60.0))

(defun speed-type--gross-wpm (num-entries seconds)
  (round (/ (/ num-entries 5.0)
            (speed-type--seconds-to-minutes seconds))))

(defun speed-type--gross-cpm (num-entries seconds)
  (round (/ num-entries (speed-type--seconds-to-minutes seconds))))

(defun speed-type--net-wpm (num-entries uncorrected-errors seconds)
  (round (- (speed-type--gross-wpm num-entries seconds)
            (/ uncorrected-errors
               (speed-type--seconds-to-minutes seconds)))))

(defun speed-type--net-cpm (num-entries uncorrected-errors seconds)
  (round (- (speed-type--gross-cpm num-entries seconds)
            (/ uncorrected-errors
               (speed-type--seconds-to-minutes seconds)))))

(defun speed-type--accuracy (total-entries correct-entries corrections)
  (let* ((correct-entries (- correct-entries corrections))
         (correct-entries (if (> correct-entries 0) correct-entries 0)))
    (* (round (* (/ correct-entries (float total-entries)) 100.0) 0.01) 0.01)))

(defun speed-type--skill (wpm)
  (cond ((< wpm 25) "Beginner")
        ((< wpm 30) "Intermediate")
        ((< wpm 40) "Average")
        ((< wpm 55) "Pro")
        ((< wpm 80) "Master")
        (t          "Racer")))

(defvar speed-type-explaining-message "\n
Gross wpm/cpm ignore uncorrected errors and indicate raw speed.
Net wpm/cmp take uncorrected errors into account and are a measure
of effective or net speed.")

(defvar speed-type-stats-format "\n
Skill:\t\t%s
Gross WPM:\t%d
Net WPM:\t%d
Gross CPM:\t%d
Net CPM:\t%d
Accuracy:\t%.2f
%s")

(defun speed-type--generate-stats (num-entries num-errors num-corrections seconds)
  (format speed-type-stats-format
          (speed-type--skill (speed-type--net-wpm num-entries num-errors seconds))
          (speed-type--gross-wpm num-entries seconds)
          (speed-type--net-wpm num-entries num-errors seconds)
          (speed-type--gross-cpm num-entries seconds)
          (speed-type--net-cpm num-entries num-errors seconds)
          (speed-type--accuracy num-entries (- num-entries num-errors) num-corrections)
          speed-type-explaining-message))

(defvar speed-type--gb-url-format
  "https://www.gutenberg.org/cache/epub/%d/pg%d.txt")

(defvar speed-type--gb-book-list '(1790 1332 1140 868 698 658 645 629 616 613
					597 549 537 527 523 517 486 486 486 464 457
					454 447 431))

(defun speed-type--gb-url (book-num)
  (format speed-type--gb-url-format book-num book-num))

(defun speed-type--gb-retrieve (book-num)
  (let* ((dr (locate-user-emacs-file (format "speed-type")))
	 (fn (locate-user-emacs-file (format "speed-type/%d.txt" book-num)))
	 (url-request-method "GET"))
    (if (file-readable-p fn)
	(find-file-noselect fn t)
      (let* ((buf (url-retrieve-synchronously (speed-type--gb-url book-num))))
	(with-current-buffer buf
	  (delete-trailing-whitespace)
	  (when (not (file-exists-p dr))
	    (make-directory dr))
	  (write-file fn)
	  buf)))))

(defvar speed-type--start-time nil)
(make-variable-buffer-local 'speed-type--start-time)

(defvar speed-type--orig-text nil)
(make-variable-buffer-local 'speed-type--orig-text)

(defvar speed-type--num-entries 0)
(make-variable-buffer-local 'speed-type--num-entries)

(defvar speed-type--num-errors 0)
(make-variable-buffer-local 'speed-type--num-errors)

(defvar speed-type--num-remaining 0)
(make-variable-buffer-local 'speed-type--num-remaining)

(defvar speed-type--mod-str nil)
(make-variable-buffer-local 'speed-type--mod-str)

(defvar speed-type--num-corrections 0)
(make-variable-buffer-local 'speed-type--num-corrections)

(defun speed-type--elapsed-time ()
  "Speed-type--lapsed-time returns a float with the total time since start."
  (let* ((end-time (float-time)))
    (if (not speed-type--start-time)
        0 (- end-time speed-type--start-time))))

(defun speed-type--check-same (pos a b)
  "Speed-type--check-same returns true iff either both characters are
white space or if the are the same."
  (let* ((q (aref a pos))
         (p (aref b pos)))
    (or (and (= (char-syntax p) ?\s)
             (= (char-syntax q) ?\s))
        (= p q))))

(defun speed-type--handle-del (start end)
  "Speed-type--handle-del keeps track of the statistics when a deletion
occurs in the buffer."
  (delete-region start end)
  (dotimes (i (- end start) nil)
    (let* ((pos (+ (1- start) i))
           (q (aref speed-type--mod-str pos)))
      (cond ((= q 0) ())
            ((= q 1) (progn (decf speed-type--num-entries)
                            (incf speed-type--num-remaining)))
            ((= q 2) (progn (decf speed-type--num-entries)
                            (incf speed-type--num-remaining)
                            (decf speed-type--num-errors)
                            (incf speed-type--num-corrections))))
      (store-substring speed-type--mod-str pos 0))))

(defun speed-type--handle-complete ()
  "Speed-type--handle-complete removes typing hooks from the buffer
and prints statistics"
  (remove-hook 'after-change-functions 'change)
  (remove-hook 'first-change-hook 'first-change)
  (goto-char (point-max))
  (insert (speed-type--generate-stats
           speed-type--num-entries
           speed-type--num-errors
           speed-type--num-corrections
           (speed-type--elapsed-time))))

(defun speed-type--diff (orig new start end)
  "Updates stats and buffer contents with result of changes in text."
  (let* ((start0 (1- start))
         (end0 (1- end))
         (color nil))
    (dotimes (i (- end start) nil)
      (let* ((pos0 (+ start0 i))
             (pos (+ start i)))
        (if (speed-type--check-same i orig new)
            (progn (setq color "green")
                   (store-substring speed-type--mod-str pos0 1))
          (progn (incf speed-type--num-errors)
                 (setq color "red")
                 (store-substring speed-type--mod-str pos0 2)))
        (incf speed-type--num-entries)
        (decf speed-type--num-remaining)
        (add-face-text-property pos (1+ pos) `(:foreground ,color))))))

(defun speed-type--change (start end length)
  "Speed-Type--change handles buffer changes. It makes sure that the
contents don't actually change, but rather the contents are color
coded and stats are gathered about the typing performance."
  (let* ((start0 (1- start))
         (end0 (1- end))
         (new-text (buffer-substring start end))
         (old-text (substring speed-type--orig-text
			      start0 (+ start0 length)))
         (orig (substring speed-type--orig-text start0 end0)))
    (speed-type--handle-del start end)
    (insert old-text)
    (speed-type--diff orig new-text start end)
    (goto-char end)
    (when (= speed-type--num-remaining 0)
      (speed-type--handle-complete))))

(defun speed-type--first-change ()
  "Speed-type--first-change starts the timer."
  (when (not speed-type--start-time)
    (setq speed-type--start-time (float-time))))

(defun speed-type--chomp (str)
  "Speed-type--chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(defun speed-type--setup (text)
  "Speed-type--setup set's up a new buffer in which the typing excercise
takes place. TEXT is copied into that new buffer."
  (let* ((speed-type--buffer (generate-new-buffer "speed-type"))
         (text (speed-type--chomp text))
	 (len (length text)))
    (set-buffer speed-type--buffer)
    (setq speed-type--orig-text text)
    (setq speed-type--mod-str (make-string len 0))
    (setq speed-type--num-remaining (length text))
    (erase-buffer)
    (insert speed-type--orig-text)
    (not-modified)
    (switch-to-buffer speed-type--buffer)
    (goto-char 0)
    (make-local-variable 'after-change-functions)
    (make-local-variable 'first-change-hook)
    (add-hook 'after-change-functions 'speed-type--change)
    (add-hook 'first-change-hook 'speed-type--first-change)
    (message "Timer will start when you type the first character.")))

(defun speed-type-region (start end)
  "Open copy of region in a new buffer to speed type the text"
  (interactive "r")
  (speed-type--setup (buffer-substring start end)))

(defun speed-type-buffer ()
  "Open copy of buffer contents in a new buffer to speed type the text"
  (interactive)
  (speed-type--setup (buffer-substring (point-min) (point-max))))

(defun speed-type-paragraph ()
  (interactive)
  (let* ((rand-num (random (length speed-type--gb-book-list)))
	 (book-num (nth rand-num speed-type--gb-book-list))
	 (paragraph-num (+ 20 (random 200))))
    (with-current-buffer (speed-type--gb-retrieve book-num)
      (goto-char 0)
      (dotimes (i paragraph-num nil)
	(forward-paragraph))
      (mark-paragraph)
      (speed-type-region (region-beginning) (region-end)))))

;;; tests

(eval-when-compile
  (defun speed-type--stats-tests ()
    (assert (= 3 (speed-type--seconds-to-minutes 180)))
    (assert (= 30 (speed-type--gross-wpm 450 180)))
    (assert (= 15 (speed-type--net-wpm 450 45 180)))
    (assert (= 85 (speed-type--accuracy 100 90 5)))
    (assert (string= "Beginner" (speed-type--skill 10)))
    (assert (string= "Pro" (speed-type--skill 45)))
    (assert (string= "Racer" (speed-type--skill 400))))
  (speed-type--stats-tests)

  (defun speed-type--url-tests ()
    (assert (string= "https://www.gutenberg.org/cache/epub/1/pg1.txt"
                     (speed-type--gb-url 1))))
  (speed-type--url-tests)

  (defun speed-type--charfun-tests ()
    (assert (speed-type--check-same 0 "\nfoo\n" "\nfoo\n"))
    (assert (speed-type--check-same 1 "\nfoo\s" "\nfoo\n"))
    (assert (speed-type--check-same 4 "\nfoo\s" "\nfoo\t"))
    (assert (not (speed-type--check-same 2 "\nfoo\s" "\nfxo\n"))))
  (speed-type--charfun-tests)

  (defun speed-type--chomp-tests ()
    (assert (string= "foo\n\t\sbar"
		     (speed-type--chomp "\s\n\tfoo\n\t\sbar\n\t\s"))))
  (speed-type--chomp-tests))

(provide 'speed-type)

;;; speed-type.el ends here
