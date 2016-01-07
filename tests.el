;;; tests for speed-type

(ert-deftest speed-type--stats-test ()
    (cl-assert (= 3 (speed-type--seconds-to-minutes 180)))
    (cl-assert (= 30 (speed-type--gross-wpm 450 180)))
    (cl-assert (= 15 (speed-type--net-wpm 450 45 180)))
    (cl-assert (= 85 (speed-type--accuracy 100 90 5)))
    (cl-assert (string= "Beginner" (speed-type--skill 10)))
    (cl-assert (string= "Pro" (speed-type--skill 45)))
    (cl-assert (string= "Racer" (speed-type--skill 400))))

(ert-deftest speed-type--url-test ()
    (cl-assert (string= "https://www.gutenberg.org/cache/epub/1/pg1.txt"
                        (speed-type--gb-url 1))))

(ert-deftest speed-type--charfun-tests ()
    (cl-assert (speed-type--check-same 0 "\nfoo\n" "\nfoo\n"))
    (cl-assert (speed-type--check-same 1 "\nfoo\s" "\nfoo\n"))
    (cl-assert (speed-type--check-same 4 "\nfoo\s" "\nfoo\t"))
    (cl-assert (not (speed-type--check-same 2 "\nfoo\s" "\nfxo\n"))))

(ert-deftest speed-type--trim-tests ()
    (cl-assert (string= "foo\n\t\sbar"
                        (speed-type--trim "\n\nfoo\n\t\sbar\n\n\n")))
    (cl-assert (string= "\tfoo\n\t\sbar"
                        (speed-type--trim "\n\tfoo\n\t\sbar\n\n\n\n"))))

(ert-deftest speed-type--remove-response-headers-from-java-samples-tests ()
  "Removes unneeded response headers from the buffer whose contents are retrieved from an url"
  (let ((buf (url-retrieve-synchronously (speed-type--java-url 0))))
    (with-current-buffer buf
      (speed-type--remove-response-headers-from-buffer)
      (beginning-of-buffer)
      (let ((first-line (thing-at-point 'line t)))
        (cl-assert (or (string-prefix-p "import" first-line)
                       (string-prefix-p "/*" first-line)
                       (string-prefix-p "//" first-line)))))))

(ert-deftest speed-type--narrow-java-samples-to-n-lines-after-class-declaration-tests ()
  "Trims the Java sample to n lines after the class declaration"
  (let ((buf (url-retrieve-synchronously (speed-type--java-url 0))))
   (with-current-buffer buf
      (speed-type--remove-response-headers-from-buffer)
      (let ((orig-words (count-words (point-min) (point-max))))
        (speed-type--narrow-java-buffer)
        (cl-assert (and (<= 100 (count-words (point-min) (point-max)))
                        (>= orig-words (count-words (point-min) (point-max)))))))))

(ert-deftest speed-type--remove-java-comments-tests ()
  "Removes the comments in Java samples"
  (let ((buf (url-retrieve-synchronously (speed-type--java-url 0))))
    (with-current-buffer buf
      (speed-type--remove-response-headers-from-buffer)
      (speed-type--remove-java-comments-in-buffer)
      (beginning-of-buffer)
      (message (format "** lines in buffer: %d" (count-matches "\n")))
      (cl-assert (not (string-match-p "\\/\\*\\*" (buffer-string)))))))
