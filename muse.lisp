;;
;; muse.lisp
;;

(in-package :cl-phonetic)

(defclass muse ()
  ((dict :initarg :dict
         :accessor dict))
  (:documentation
   "Provides text generation APIs."))

(defun phonelist-syllcount (phoneme-list)
  "Count the syllables in a simple phoneme list."
  (count-if #'vowel-p phoneme-list))

(defun random-word-lt-syllcount (dict max-syllcount)
  "Get a random word with no more than MAX-SYLLCOUNT syllables."
  (let*
      ((word-phoneme-alist (hash-table-alist (words dict)))
       (word-count         (length word-phoneme-alist)))
    (labels
        ((random-word ()
           (nth (random word-count) word-phoneme-alist)))
      (do ((word (random-word) (random-word)))
          ((<= (phonelist-syllcount (pronunciation-phonemes (first (cdr word))))
               max-syllcount)
           word)))))

(defun phonelist-idx-nth-vowel (n phoneme-list)
  "Get the position of the Nth vowel."
  (nth-position-if n #'vowel-p phoneme-list))

(defun next-line/inner (muse phonemes-from-line)
  ;; get phonemes-from-line
  ;; get random-word until random-word <= as many syllables as phonemes-from-line
  ;; return the catenation of random-word and phonemes-from-line after (syllable-count line)
  (let*
      ((fitting-word           (random-word-lt-syllcount dict (length (remove-if-not #'vowel-p phonemes-from-line))))
       (fitting-word-syllcount (phonelist-syllcount (pronunciation-phonemes (first (cdr fitting-word)))))
       (phonemes-clip-pos      (1+ (phonelist-idx-nth-vowel (1- fitting-word-syllcount) phonemes-from-line)))
       (recurse-phonemes       (subseq phonemes-from-line phonemes-clip-pos)))
    (if (zerop (phonelist-syllcount recurse-phonemes))
        (list fitting-word)
        (concatenate 'list
                     (list fitting-word)
                     (next-line/inner muse recurse-phonemes)))))

(defun next-line (muse line)
  "Respond with a fitting line.
MAJOR todo. This is basically a stub."
  (let
      ((word-pair-sequence (next-line/inner muse (pronunciation-phonemes (pronounce-utterance (dict muse) line)))))
    (join-strings " " (mapcar #'car word-pair-sequence))))
