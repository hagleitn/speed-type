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

