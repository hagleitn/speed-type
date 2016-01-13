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
(require 'request) ;; To use GitHub's REST API


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

(defconst speed-type--english-label "English" "Identifies English texts")
(defconst speed-type--code-label "Code" "Identifies code samples")
(defconst speed-type--java-label "Java" "Identifies Java samples")

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

(defun speed-type--github-account-from-url (url)
  "Retrieves the github account from given url"
  (if (string-match "https?://\\(www\\.\\)?github.com/\\([^/]+\\)/.*" url)
      (match-string 2 url)))

(defun speed-type--github-repo-from-url (url)
  "Retrieves the github repository from given url"
  (if (string-match (concat "https?://\\(www\\.\\)?github.com/[^/]+/\\([^/]+\\)/.*") url)
      (match-string 2 url)))

(defun speed-type--github-raw-url-from-blob-url (url)
  "Converts given github url to raw"
  (let* ((account (speed-type--github-account-from-url url))
         (repo (speed-type--github-repo-from-url url))
         (blob-url (concat "https?://\\(www\\.\\)?github.com/" account "/" repo "/blob/\\(.*\\)")))
    (if (string-match blob-url url)
        (concat "https://github.com/" account "/" repo "/raw/" (match-string 2 url))
      url)))

(defun speed-type--github-api-get (url success-fn)
  "Calls GitHub API and calls given function back upon success"
  (let ((result))
    (request
     url
     :parser 'json-read
     :sync t
     :success (function* (lambda (&key data &allow-other-keys)
                           (setq result (funcall success-fn data)))))
    result))

(defun speed-type--github-account-info (account keyword)
  "Retrieves information from a github account"
  (speed-type--github-api-get
   (concat "https://api.github.com/users/" account)
   (lambda (data)
     (assoc-default keyword data))))

(defun speed-type--github-repo-info (account repo keyword)
  "Retrieves information about a github repository"
  (speed-type--github-api-get
   (concat "https://api.github.com/repos/" account "/" repo)
   (lambda (data)
     (assoc-default keyword data))))

(defcustom speed-type--code-samples
  '(
    "https://github.com/appium/sample-code/blob/master/sample-code/examples/java/generic/src/main/java/Main.java"
    "https://github.com/youtube/api-samples/blob/master/java/src/main/java/com/google/api/services/samples/youtube/cmdline/Auth.java"
    "https://github.com/youtube/api-samples/blob/master/java/src/main/java/com/google/api/services/samples/youtube/cmdline/analytics/YouTubeAnalyticsReports.java"
    "https://github.com/youtube/api-samples/blob/master/java/src/main/java/com/google/api/services/samples/youtube/cmdline/live/ListBroadcasts.java"
    )
  "Code samples"
  :group 'speed-type
  :type '(repeat string))

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

(defun speed-type--remove-response-headers-from-buffer ()
  "Removes the response headers from the current buffer"
  (beginning-of-buffer)
  (let ((header-region-end (re-search-forward "^\n")))
    (beginning-of-buffer)
    (delete-region (point-min) header-region-end)))

(defun speed-type--narrow-java-buffer ()
  "Narrows the Java buffer to 100 words after the class declaration"
  (speed-type--narrow-to-n-words-after-separator-from-buffer "{" 100))

(defun speed-type--narrow-to-n-words-after-separator-from-buffer (separator n)
  "Narrows the buffer to n words after the line containing the first occurrence of given separator"
  (save-excursion
    (let ((index 0)
          (start (point-min)))
      (beginning-of-buffer)
      (re-search-forward separator)
      (beginning-of-line)
      (delete-region start (point))
      (while (> n (count-words start (point)))
        (forward-line 1))
      (delete-region (point) (point-max))
      (beginning-of-buffer)
      (replace-regexp "\\([ \t]+\\)" " "))))

(defun speed-type--n-spaces (n)
  "Retrieves a string with given number of spaces"
  (make-string n (string-to-char " ")))

(defun speed-type--indentation-offset-in-buffer ()
  "Retrieves the indentation offset in the current buffer"
  (save-excursion
    (end-of-buffer)
    (beginning-of-line)
    (if (not (= 0 (count-matches "^$")))
        (insert "\n"))
    (let ((number-of-spaces 1)
          (line-count (count-lines 1 (point-max)))
          (empty-lines (count-matches "^\[ \]*\n" (point-min))))
      (while (= line-count
                (+ empty-lines
                   (count-matches (format "^[ ]\\{%d\\}[ ]*\\S-+" number-of-spaces) (point-min))))
        (cl-incf number-of-spaces))
      (- number-of-spaces 1))))

(defun speed-type--normalize-indentation ()
  "Removes excessive indentation in the current buffer"
  (save-excursion
    (beginning-of-buffer)
    (if (re-search-forward "^Author: " nil t)
        (forward-line))
      (save-restriction
      (narrow-to-region (point) (point-max))
      (let ((indentation (speed-type--indentation-offset-in-buffer)))
        (replace-regexp (format "^[ ]\\{%d\\}" indentation) "")))))

(defun speed-type--remove-java-comments-in-buffer ()
  "Removes Java comments in buffer"
  (save-excursion
    (beginning-of-buffer)
    (let ((start (re-search-forward "/\\*\\*?" nil :no-error))
          (end))
      (while (not (null start))
        (setq end (re-search-forward "\\*/" nil :no-error))
        (if (not (null end))
          (delete-region (- start (length "/**")) end))
        (setq start (re-search-forward "/\\*\\*?" nil :no-error)))
      (beginning-of-buffer)
      (replace-regexp "^[ \t]*//.*$" "")
      (beginning-of-buffer)
      (replace-regexp "\n\n*" "\n")
      )))

(defun speed-type--process-java-buffer ()
  "Processes the Java buffer"
  (speed-type--remove-java-comments-in-buffer)
  (speed-type--normalize-indentation))

(defun speed-type--code-sample-is (type sample)
  "Checks if the given url represents code of a certain type"
  (string-suffix-p (concat "." (downcase type)) sample))

(defun speed-type--code-sample-is-java (sample)
  "Checks if the given url belongs to a Java file"
  (speed-type--code-sample-is speed-type--java-label sample))

(defun speed-type--sample-is-code (sample)
  "Checks if the given url represents code snippets"
  ;; TODO: support more languages
  (speed-type--code-sample-is-java sample))

(defun speed-type--insert-code-metadata (sample)
  "Appends the code metadata of given sample to the current buffer"
  (let* ((account (speed-type--github-account-from-url sample))
         (repo (speed-type--github-repo-from-url sample))
         (name (speed-type--github-account-info account 'name))
         (email (speed-type--github-account-info account 'email))
         (title (speed-type--github-repo-info account repo 'description))
         (author (concat
                  name
                  (if email (concat " <" email ">") ""))))
    (beginning-of-buffer)
    (re-search-forward "^$")
    (insert (concat "\nURL: " sample "\n"
                    "\nTitle: " title "\n"
                    "\nAuthor: " author "\n"
                    " \n"))))

(defun speed-type--conditionally-insert-metadata (url)
  "Inserts the metadata if the buffer does not include it, and only if it's available"
  (if (speed-type--sample-is-code url)
      (speed-type--insert-code-metadata url)))

(defun speed-type--process-buffer (&optional sample)
  "Process the buffer for a code sample"
  (cond ((and sample
              (speed-type--code-sample-is-java sample) (speed-type--process-java-buffer)))
        (t t)))

(defun speed-type--code-sample-url (sample-num)
  "Returns the sample at SAMPLE-NUM index"
  (let ((n (min sample-num (- (length speed-type--code-samples) 1))))
    (nth n speed-type--code-samples)))

(defun speed-type--sample-num-to-url (sample-num)
  "Retrieves the url for given sample index"
  (cond ((string= speed-type-practice-content speed-type--english-label) (speed-type--gb-url sample-num))
        (t (speed-type--code-sample-url book-num))))

(defun speed-type--code-retrieve (sample-num)
  "Returns a buffer with the sample SAMPLE-NUM in it."
  (let ((dr (locate-user-emacs-file (format "speed-type")))
        (fn (locate-user-emacs-file (format "speed-type/code-%d.txt" sample-num)))
        (url-request-method "GET")
        (sample (speed-type--code-sample sample-num)))
    (if (file-readable-p fn)
        (find-file-noselect fn t)
      (let ((buf (url-retrieve-synchronously (speed-type--github-raw-url-from-blob-url sample))))
        (with-current-buffer buf
          (delete-trailing-whitespace)
          (speed-type--conditionally-insert-metadata sample)
          (when (not (file-exists-p dr))
            (make-directory dr))
          (write-file fn)
          buf)))))

(defun speed-type--retrieve (index)
  "Retrieves the practice content, at given index"
  (cond ((string= speed-type-practice-content speed-type--english-label) (speed-type--gb-retrieve index))
         (t (speed-type--code-retrieve index))))

(defun speed-type--content-list ()
  "Retrieves the list with the available content for practicing touch typing"
  (cond ((string= speed-type-practice-content speed-type--code-label) (number-sequence 0 (length speed-type--code-samples)))
        (t speed-type--gb-book-list)))

(defgroup speed-type nil
  "Configuration settings for Speed Type mode"
  :group 'extensions)

(defcustom speed-type-practice-content "English"
  "The type of content used to practice speed/touch typing"
  :group 'speed-type
  :type '(choice
          (const :tag "English" "English")
          (const :tag "Code" "Code")))

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

(defun speed-type--setup (text &optional url author title)
  "Set up a new buffer for the typing exercise on TEXT."
  (with-temp-buffer
    (insert text)
    (delete-trailing-whitespace)
    (speed-type--normalize-indentation)
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
(defvar speed-type--min-chars-for-code-samples 100)
(defvar speed-type--max-chars-for-code-samples 200)
(defvar speed-type--skip-paragraphs-for-code-samples 5)
(defvar speed-type--max-paragraphs-for-code-samples 25)

(defun speed-type--min-chars ()
  "Retrieves the minimum number of characters to practice"
  (cond ((string= speed-type-practice-content speed-type--english-label) speed-type--min-chars)
        (t speed-type--min-chars-for-code-samples)))

(defun speed-type--max-chars ()
  "Retrieves the maximum number of characters to practice"
  (cond ((string= speed-type-practice-content speed-type--english-label) speed-type--max-chars)
        (t speed-type--max-chars-for-code-samples)))

(defun speed-type--skip-paragraphs ()
  "Retrieves the number of paragraphs to skip"
  (cond ((string= speed-type-practice-content speed-type--english-label) speed-type--skip-paragraphs)
        (t speed-type--skip-paragraphs-for-code-samples)))

(defun speed-type--max-paragraphs ()
  "Retrieves the maximun number of paragraphs"
  (cond ((string= speed-type-practice-content speed-type--english-label) speed-type--max-paragraphs)
        (t speed-type--max-paragraphs-for-code-samples)))

(defun speed-type--forward-java-paragraph ()
  "Moves the cursor forward one 'paragraph', adapted to Java content"
  (while (re-search-forward "^import " nil t)) ;; skipping imports
  (if (or (re-search-forward ";" nil t)
      (re-search-forward "{" nil t)
      (re-search-forward "}" nil t))
      (forward-char)))

(defun speed-type--backward-java-paragraph ()
  "Moves the cursor backward one 'paragraph', adapted to Java content"
  (beginning-of-line)
  (if (or (re-search-backward ";" nil t)
          (re-search-backward "{" nil t)
          (re-search-backward "}" nil t))
      (forward-char)))

(defun speed-type--mark-java-paragraph ()
  "Marks one 'paragraph', adapted to Java content"
  (speed-type--forward-java-paragraph)
  (push-mark)
  (speed-type--backward-java-paragraph)
  (exchange-point-and-mark))

(defun speed-type--goto-end-previous-java-sentence ()
  "Goes back to the end of the previous Java sentence"
  (if (or (re-search-backward ")" nil t)
          (re-search-backward ";" nil t)
          (re-search-backward "}" nil t))
      (forward-char)))

(defun speed-type--forward-code-paragraph (url)
  "Moves the cursor forward one 'paragraph', adapted to the code syntax in current buffer"
  (cond ((speed-type--code-sample-is-java url) (speed-type--forward-java-paragraph))
        ;; TODO: support more languages
        (t (forward-paragraph))))

(defun speed-type--backward-code-paragraph (url)
  "Moves the cursor backward one 'paragraph', adapted to the code syntax in current buffer"
  (cond ((speed-type--code-sample-is-java url) (speed-type--backward-java-paragraph))
        ;; TODO: support more languages
        (t (backward-paragraph))))

(defun speed-type--mark-code-paragraph (url)
  "Marks one 'paragraph', adapted to the code syntax in current buffer"
  (cond ((speed-type--code-sample-is-java url) (speed-type--mark-java-paragraph))
        ;; TODO: support more languages
        (t (mark-paragraph))))

(defun speed-type--goto-end-previous-code-sentence (url)
  "Goes back to the end of the previous sentence in the buffer, according to the language"
  (cond ((speed-type--code-sample-is-java url) (speed-type--goto-end-previous-java-sentence))
        ;; TODO: support more languages
        (t (search-backward "." (mark) t))))

(defun speed-type--forward-paragraph (url)
  "Moves forward the cursor one paragraph, according to the type of content"
  (cond ((string= speed-type-practice-content speed-type--code-label) (speed-type--forward-code-paragraph url))
        (t (forward-paragraph))))

(defun speed-type--backward-paragraph (url)
  "Moves backward the cursor one paragraph, according to the type of content"
  (cond ((string= speed-type-practice-content speed-type--code-label) (speed-type--backward-code-paragraph url))
        (t (backward-paragraph))))

(defun speed-type--mark-paragraph (url)
  "Marks the current paragraph, according to the type of content"
  (cond ((string= speed-type-practice-content speed-type--code-label) (speed-type--mark-code-paragraph url))
        (t (mark-paragraph))))

(defun speed-type--goto-end-previous-sentence (url)
  "Goes back to the end of the previous sentence"
  (cond ((string= speed-type-practice-content speed-type--code-label) (speed-type--goto-end-previous-code-sentence url))
        (t (search-backward "." (mark) t))))

;;;###autoload
(defun speed-type-text ()
  "Setup a new text sample to practice touch or speed typing."
  (interactive)
  (let* ((rand-num (random (length (speed-type--content-list))))
         (book-num (nth rand-num (speed-type--content-list)))
         (paragraph-num (+ (speed-type--skip-paragraphs)
                           (random (speed-type--max-paragraphs))))
         (fwd t)
         (p (point))
         (tries 20)
         (min-chars (speed-type--min-chars))
         (max-chars (speed-type--max-chars))
         (author nil)
         (title nil)
         (url (speed-type--sample-num-to-url book-num)))
    (with-current-buffer (speed-type--retrieve book-num)
      (speed-type--process-buffer url)
      (goto-char 0)
      (when (re-search-forward "^Title: " nil t)
        (setq title (buffer-substring (point) (line-end-position))))
      (when (re-search-forward "^Author: " nil t)
        (setq author (buffer-substring (point) (line-end-position))))
      (dotimes (i paragraph-num nil)
        (setq p (point))
        (if fwd (speed-type--forward-paragraph url)
          (speed-type--backward-paragraph url))
        (when (= p (point))
          (setq fwd (not fwd))))
      (speed-type--mark-paragraph url)
      (exchange-point-and-mark)
      (setq fwd nil)
      (while (> tries 0)
        (let ((size (- (point) (mark))))
          (cond ((< size min-chars)
                 (progn (speed-type--forward-paragraph url)
                        (when fwd
                          (speed-type--forward-paragraph url)
                          (speed-type--mark-paragraph url)
                          (exchange-point-and-mark))
                        (setq fwd nil)))
                ((> size max-chars)
                 (progn (speed-type--goto-end-previous-sentence url)
                        (setq fwd t)))
                (t (setq tries 1))))
        (cl-decf tries))
      (when fwd (forward-char))
      (speed-type--setup
       (buffer-substring (region-beginning) (region-end))
       url author title))))

(provide 'speed-type)

;;; speed-type.el ends here
