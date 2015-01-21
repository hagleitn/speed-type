;;; speed-type.el --- Practice touch and speed typing

;; Copyright (C) 2015 Gunther Hagleitner

;; Author: Gunther Hagleitner
;; Version: 0.1
;; Keywords: games
;; URL: https://github.com/hagleitn/speed-type
;; Package-Requires: ((cl-lib "0.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Speed-type allows you to practice your touch typing skills. You can
;; test yourself by typing snippets from online books or use any piece
;; of text or code you have in emacs. Speed-type keeps track of your
;; stats (WPM, CPM, accuracy) while you are typing.

;;; Code:

(require 'url)
(require 'cl-lib)

(defun speed-type--seconds-to-minutes (seconds)
  "Return minutes in float for SECONDS."
  (/ seconds 60.0))

(defun speed-type--gross-wpm (entries seconds)
  "Return gross words-per-minute.

Computes words-per-minute as (ENTRIES/5) / (SECONDS/60)."
  (round (/ (/ entries 5.0)
            (speed-type--seconds-to-minutes seconds))))

(defun speed-type--gross-cpm (entries seconds)
  "Return gross characters-per-minute.

Computes characters-per-minute as ENTRIES / (SECONDS/60)."
  (round (/ entries (speed-type--seconds-to-minutes seconds))))

(defun speed-type--net-wpm (entries uncorrected-errors seconds)
  "Return net words-per-minute.

Computes net words-per-minute as:
  ((ENTRIES/5) - UNCORRECTED-ERRORS) / (SECONDS/60)."
  (let ((net-wpm (round (- (speed-type--gross-wpm entries seconds)
                           (/ uncorrected-errors
                              (speed-type--seconds-to-minutes seconds))))))
    (if (> 0 net-wpm) 0 net-wpm)))

(defun speed-type--net-cpm (entries uncorrected-errors seconds)
  "Return net characters-per-minute.

Computes net characters-per-minute as:
  (ENTRIES - UNCORRECTED-ERRORS) / (SECONDS/60)."
  (let ((net-cpm (round (- (speed-type--gross-cpm entries seconds)
                           (/ uncorrected-errors
                              (speed-type--seconds-to-minutes seconds))))))
    (if (> 0 net-cpm) 0 net-cpm)))

(defun speed-type--accuracy (total-entries correct-entries corrections)
  "Return accuracy.

Accuracy is computed as (CORRECT-ENTRIES - CORRECTIONS) / TOTAL-ENTRIES."
  (let* ((correct-entries (- correct-entries corrections))
         (correct-entries (if (> correct-entries 0) correct-entries 0)))
    (* (round (* (/ correct-entries (float total-entries)) 100.0) 0.01) 0.01)))

(defun speed-type--skill (wpm)
  "Return skill for WPM."
  (cond ((< wpm 25) "Beginner")
        ((< wpm 30) "Intermediate")
        ((< wpm 40) "Average")
        ((< wpm 55) "Pro")
        ((< wpm 80) "Master")
        (t          "Racer")))

(defvar speed-type-explaining-message "\n
Gross wpm/cpm ignore uncorrected errors and indicate raw speed.
Net wpm/cpm take uncorrected errors into account and are a measure
of effective or net speed.")

(defvar speed-type-stats-format "\n
Skill:\t\t%s
Net WPM:\t%d
Net CPM:\t%d
Gross WPM:\t%d
Gross CPM:\t%d
Accuracy:\t%.2f%%
Total time:\t%s
Total chars:\t%d
Corrections:\t%d
Total errors:\t%d
%s")

(defun speed-type--generate-stats (entries errors corrections seconds)
  "Return string of statistics."
  (format speed-type-stats-format
          (speed-type--skill (speed-type--net-wpm entries errors seconds))
          (speed-type--net-wpm entries errors seconds)
          (speed-type--net-cpm entries errors seconds)
          (speed-type--gross-wpm entries seconds)
          (speed-type--gross-cpm entries seconds)
          (speed-type--accuracy entries (- entries errors) corrections)
          (format-seconds "%M %z%S" seconds)
          entries
          corrections
          (+ errors corrections)
          speed-type-explaining-message))
  

(defvar speed-type--gb-url-format
  "https://www.gutenberg.org/cache/epub/%d/pg%d.txt")

(defvar speed-type--gb-book-list
  '(1342 11 1952 1661 74 1232 23 135 5200 2591 844 84 98 2701 1400 16328 174
         46 4300 345 1080 2500 829 1260 6130 1184 768 32032 521 1399 55))

(defun speed-type--gb-url (book-num)
  "Return url for BOOK-NUM."
  (format speed-type--gb-url-format book-num book-num))

(defun speed-type--gb-retrieve (book-num)
  "Return buffer with book number BOOK-NUM in it."
  (let ((dr (locate-user-emacs-file (format "speed-type")))
        (fn (locate-user-emacs-file (format "speed-type/%d.txt" book-num)))
        (url-request-method "GET"))
    (if (file-readable-p fn)
        (find-file-noselect fn t)
      (let ((buf (url-retrieve-synchronously (speed-type--gb-url book-num))))
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

(defvar speed-type--entries 0)
(make-variable-buffer-local 'speed-type--entries)

(defvar speed-type--errors 0)
(make-variable-buffer-local 'speed-type--errors)

(defvar speed-type--remaining 0)
(make-variable-buffer-local 'speed-type--remaining)

(defvar speed-type--mod-str nil)
(make-variable-buffer-local 'speed-type--mod-str)

(defvar speed-type--corrections 0)
(make-variable-buffer-local 'speed-type--corrections)

(defvar speed-type--title nil)
(make-variable-buffer-local 'speed-type--title)

(defvar speed-type--author nil)
(make-variable-buffer-local 'speed-type--author)

(defun speed-type--elapsed-time ()
  "Return float with the total time since start."
  (let ((end-time (float-time)))
    (if (not speed-type--start-time)
        0 (- end-time speed-type--start-time))))

(defun speed-type--check-same (pos a b)
  "Return true iff both A[POS] B[POS] are white space or if they are the same."
  (let ((q (aref a pos))
        (p (aref b pos)))
    (or (and (= (char-syntax p) ?\s)
             (= (char-syntax q) ?\s))
        (= p q))))

(defun speed-type--handle-del (start end)
  "Keep track of the statistics when a deletion occurs between START and END."
  (delete-region start end)
  (dotimes (i (- end start) nil)
    (let* ((pos (+ (1- start) i))
           (q (aref speed-type--mod-str pos)))
      (cond ((= q 0) ())
            ((= q 1) (progn (cl-decf speed-type--entries)
                            (cl-incf speed-type--remaining)))
            ((= q 2) (progn (cl-decf speed-type--entries)
                            (cl-incf speed-type--remaining)
                            (cl-decf speed-type--errors)
                            (cl-incf speed-type--corrections))))
      (store-substring speed-type--mod-str pos 0))))

(defun speed-type--handle-complete ()
  "Remove typing hooks from the buffer and print statistics."
  (remove-hook 'after-change-functions 'speed-type--change)
  (remove-hook 'first-change-hook 'speed-type--first-change)
  (goto-char (point-max))
  (when (and speed-type--title speed-type--author)
    (insert "\n\n")
    (insert (propertize
             (format "%s, by %s" speed-type--title speed-type--author)
             'face 'italic)))
  (insert (speed-type--generate-stats
           speed-type--entries
           speed-type--errors
           speed-type--corrections
           (speed-type--elapsed-time)))
  (read-only-mode))

(defun speed-type--diff (orig new start end)
  "Update stats and buffer contents with result of changes in text."
  (let ((start0 (1- start))
        (end0 (1- end))
        (color nil))
    (dotimes (i (- end start) nil)
      (let ((pos0 (+ start0 i))
            (pos (+ start i)))
        (if (speed-type--check-same i orig new)
            (progn (setq color "green")
                   (store-substring speed-type--mod-str pos0 1))
          (progn (cl-incf speed-type--errors)
                 (setq color "red")
                 (store-substring speed-type--mod-str pos0 2)))
        (cl-incf speed-type--entries)
        (cl-decf speed-type--remaining)
	(if (fboundp 'add-face-text-property)
	    (add-face-text-property pos (1+ pos) `(:foreground ,color))
	  (add-text-properties pos (1+ pos) `(face (:foreground ,color))))))))

(defun speed-type--change (start end length)
  "Handle buffer changes.

Make sure that the contents don't actually change, but rather the contents
are color coded and stats are gathered about the typing performance."
  (let ((len (length speed-type--orig-text)))
    (when (<= start len)
      (let* ((end (if (> end (1+ len)) len end))
             (length (if (> (+ start length) len) (1+ (- len start)) length))
             (start0 (1- start))
             (end0 (1- end))
             (new-text (buffer-substring start end))
             (old-text (substring speed-type--orig-text
                                  start0 (+ start0 length)))
             (orig (substring speed-type--orig-text start0 end0)))
        (speed-type--handle-del start end)
        (insert old-text)
        (speed-type--diff orig new-text start end)
        (goto-char end)
        (when (= speed-type--remaining 0)
          (speed-type--handle-complete))))))

(defun speed-type--first-change ()
  "Start the timer."
  (when (not speed-type--start-time)
    (setq speed-type--start-time (float-time))))

(defun speed-type--trim (str)
  "Trim leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any "\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(defun speed-type--setup (text &optional author title)
  "Set up a new buffer for the typing exercise on TEXT."
  (with-temp-buffer
    (insert text)
    (delete-trailing-whitespace)
    (setq text (buffer-string)))
  (let* ((buf (generate-new-buffer "speed-type"))
         (text (speed-type--trim text))
         (len (length text)))
    (set-buffer buf)
    (setq speed-type--orig-text text)
    (setq speed-type--mod-str (make-string len 0))
    (setq speed-type--remaining (length text))
    (erase-buffer)
    (insert speed-type--orig-text)
    (not-modified)
    (switch-to-buffer buf)
    (goto-char 0)
    (setq speed-type--author author)
    (setq speed-type--title title)
    (make-local-variable 'after-change-functions)
    (make-local-variable 'first-change-hook)
    (add-hook 'after-change-functions 'speed-type--change)
    (add-hook 'first-change-hook 'speed-type--first-change)
    (message "Timer will start when you type the first character.")))

;;;###autoload
(defun speed-type-region (start end)
  "Open copy of [START,END] in a new buffer to speed type the text."
  (interactive "r")
  (speed-type--setup (buffer-substring start end)))

;;;###autoload
(defun speed-type-buffer ()
  "Open copy of buffer contents in a new buffer to speed type the text."
  (interactive)
  (speed-type--setup (buffer-substring (point-min) (point-max))))

(defvar speed-type--min-chars 200)
(defvar speed-type--max-chars 450)
(defvar speed-type--skip-paragraphs 30)
(defvar speed-type--max-paragraphs 200)

;;;###autoload
(defun speed-type-text ()
  "Setup a new text sample to practice touch or speed typing."
  (interactive)
  (let* ((rand-num (random (length speed-type--gb-book-list)))
         (book-num (nth rand-num speed-type--gb-book-list))
         (paragraph-num (+ speed-type--skip-paragraphs
                           (random speed-type--max-paragraphs)))
         (fwd t)
         (p (point))
         (tries 20)
	 (author nil)
	 (title nil))
    (with-current-buffer (speed-type--gb-retrieve book-num)
      (goto-char 0)
      (when (re-search-forward "^Title: " nil t)
        (setq title (buffer-substring (point) (line-end-position))))
      (when (re-search-forward "^Author: " nil t)
        (setq author (buffer-substring (point) (line-end-position))))
      (dotimes (i paragraph-num nil)
        (setq p (point))
        (if fwd (forward-paragraph)
          (backward-paragraph))
        (when (= p (point))
          (setq fwd (not fwd))))
      (mark-paragraph)
      (exchange-point-and-mark)
      (setq fwd nil)
      (while (> tries 0)
        (let ((size (- (point) (mark))))
          (cond ((< size speed-type--min-chars)
                 (progn (forward-paragraph)
                        (when fwd
                          (forward-paragraph)
                          (mark-paragraph)
                          (exchange-point-and-mark))
                        (setq fwd nil)))
                ((> size speed-type--max-chars)
                 (progn (search-backward "." (mark) t)
                        (setq fwd t)))
                (t (setq tries 1))))
        (cl-decf tries))
      (when fwd (forward-char))
      (speed-type--setup
       (buffer-substring (region-beginning) (region-end))
       author title))))

(provide 'speed-type)

;;; speed-type.el ends here
