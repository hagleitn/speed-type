;;; tests for speed-type
(require 'speed-type)

(ert-deftest speed-type--stats-test ()
    (should (= 3 (speed-type--seconds-to-minutes 180)))
    (should (= 30 (speed-type--gross-wpm 450 180)))
    (should (= 15 (speed-type--net-wpm 450 45 180)))
    (should (= 85 (speed-type--accuracy 100 90 5)))
    (should (string= "Beginner" (speed-type--skill 10)))
    (should (string= "Pro" (speed-type--skill 45)))
    (should (string= "Racer" (speed-type--skill 400))))

(ert-deftest speed-type--url-test ()
    (should (string= "https://www.gutenberg.org/cache/epub/1/pg1.txt"
                        (speed-type--gb-url 1))))

(ert-deftest speed-type--charfun-tests ()
    (should (speed-type--check-same 0 "\nfoo\n" "\nfoo\n"))
    (should (speed-type--check-same 1 "\nfoo\s" "\nfoo\n"))
    (should (speed-type--check-same 4 "\nfoo\s" "\nfoo\t"))
    (should (not (speed-type--check-same 2 "\nfoo\s" "\nfxo\n"))))

(ert-deftest speed-type--trim-tests ()
    (should (string= "foo\n\t\sbar"
                     (speed-type--trim "\n\nfoo\n\t\sbar\n\n\n")))
    (should (string= "\tfoo\n\t\sbar"
                     (speed-type--trim "\n\tfoo\n\t\sbar\n\n\n\n"))))
