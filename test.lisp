;;
;; test.lisp
;;
;; Package unit tests.
;;

(in-package :cl-phonetic)

(ql:quickload 'lisp-unit)
(use-package 'lisp-unit)

(define-test normalize-word
  (assert-equal "saturday" (normalize-word "SatURday!!!!!"))
  (assert-equal ""         (normalize-word ""))
  (assert-equal ""         (normalize-word ",,"))
  (assert-equal "she's"    (normalize-word "she's   ")))

(define-test phoneme-predicates
  (assert-false (consonant-p (get-phoneme-by-name "IY")))
  (assert-true  (vowel-p (get-phoneme-by-name "IY"))))

(define-test query-phonemes
  (assert-equal (mapcar #'get-phoneme-by-name '("CH" "JH"))
                (query-phonemes 'consonant :manner-of-articulation 'affricate))
  (assert-equal (mapcar #'get-phoneme-by-name '("IH" "UH"))
                (query-phonemes 'vowel :height 'near-close))
  (assert-equal (mapcar #'get-phoneme-by-name '("UH"))
                (query-phonemes 'vowel
                                :height 'near-close
                                :roundedness 'rounded)))

(define-test decode-cmudict-word
  (assert-equal "cat" (decode-cmudict-word "cat"))
  (assert-equal '("cat" 12) (multiple-value-list (decode-cmudict-word "caT(12)")))
  (assert-equal '("cat" 3)  (multiple-value-list (decode-cmudict-word "caT(3)")))
  (assert-equal "cat" (decode-cmudict-word "c.a.t"))
  (assert-equal "cat" (decode-cmudict-word "c.a.t()")))

(run-tests)
