;;
;; phonetic.lisp
;;
;; Search for phoneme patterns using regular expressions.
;;

;; Libraries

(ql:quickload 'cl-ppcre)
(ql:quickload 'cl-utilities)

;; Utility

(defmacro string-extract (str re names-list &rest body)
  (let ((whole-sym   (gensym))
        (matches-sym (gensym)))
    `(multiple-value-bind (,whole-sym ,matches-sym)
         (ppcre:scan-to-strings ,re ,str)
       (declare (ignore ,whole-sym))
       (destructuring-bind ,names-list (coerce ,matches-sym 'list)
         ,@body))))

;; Phonemes & Encoding

(defconstant +unicode-pua-base+ #x00E000
  "This is where we start encoding phoneme characters. Should be within the Unicode Private Use Area,
with at least 50 or so following codes to spare.")

(defclass phoneme ()
  ((name :initarg :name
         :accessor phoneme-name)
   (type :initarg :type
         :accessor phoneme-type)
   (representation :initarg :representation
                   :accessor phoneme-representation)
   (encoded-value :initarg :encoded-value
                  :accessor encoded-value)))

(defclass consonant (phoneme)
  ((voiced :initarg :voiced
           :accessor voiced-p)
   (place-of-articulation :initarg :place-of-articulation
                          :accessor place-of-articulation)
   (manner-of-articulation :initarg :manner-of-articulation
                           :accessor manner-of-articulation)))

(defclass vowel (phoneme)
  ((height :initarg :height
           :accessor height
           :initform nil)
   (backness :initarg :backness
             :accessor backness
             :initform nil)
   (roundedness :initarg :roundedness
                :accessor roundedness
                :initform nil)))

(let ((phoneme-counter 0))
  (defmethod initialize-instance :after ((p phoneme) &key name)
             (setf (encoded-value p) (+ +unicode-pua-base+ phoneme-counter))
             (setf (phoneme-representation p) (symbol-name name))
             (setf phoneme-counter (1+ phoneme-counter))))

(defmethod print-object ((p phoneme) out)
  (print-unreadable-object (p out :type t)
    (format out "~A" (phoneme-representation p))))

(defmethod encoded-char ((p phoneme))
  (code-char (encoded-value p)))

(defparameter *phonemes*
  (macrolet ((vowel (name height backness roundedness)
               `(make-instance 'vowel
                               :name ,name
                               :type 'vowel
                               :height ,height
                               :backness ,backness
                               :roundedness ,roundedness))
             (diphthong (name)
               `(make-instance 'vowel
                               :name ,name
                               :type 'vowel))
             (r-colored (name)
               `(make-instance 'vowel
                               :name ,name
                               :type 'vowel))
             (consonant (name voiced poa moa)
               `(make-instance 'consonant
                               :name ,name
                               :type 'consonant
                               :voiced ,voiced
                               :place-of-articulation ,poa
                               :manner-of-articulation ,moa)))
    (list
     (vowel     'AA 'open       'central       'unrounded)
     (vowel     'AE 'near-open  'front         'unrounded)
     (vowel     'AH 'near-open  'central       'any)
     (vowel     'AO 'open-mid   'back          'rounded)
     (diphthong 'AW)
     (diphthong 'AY)
     (consonant 'B  'voiced     'bilabial      'plosive)
     (consonant 'CH 'unvoiced   'post-alveolar 'affricate)
     (consonant 'D  'voiced     'alveolar      'plosive)
     (consonant 'DH 'voiced     'dental        'fricative)
     (vowel     'EH 'open-mid   'front         'unrounded)
     (r-colored 'ER)
     (diphthong 'EY)
     (consonant 'F  'unvoiced   'labio-dental  'fricative)
     (consonant 'G  'voiced     'velar         'plosive)
     (consonant 'HH 'unvoiced   'glottal       'fricative)
     (vowel     'IH 'near-close 'front         'unrounded)
     (vowel     'IY 'close      'front         'unrounded)
     (consonant 'JH 'voiced     'post-alveolar 'affricate)
     (consonant 'K  'unvoiced   'velar         'plosive)
     (consonant 'L  'voiced     'alveolar      'lateral)
     (consonant 'M  'voiced     'bilabial      'nasal)
     (consonant 'N  'voiced     'alveolar      'nasal)
     (consonant 'NG 'voiced     'velar         'nasal)
     (diphthong 'OW)
     (diphthong 'OY)
     (consonant 'P  'unvoiced   'bilabial      'plosive)
     (consonant 'R  'voiced     'post-alveolar 'approximant)
     (consonant 'S  'unvoiced   'alveolar      'fricative)
     (consonant 'SH 'unvoiced   'post-alveolar 'fricative)
     (consonant 'T  'unvoiced   'alveolar      'plosive)
     (consonant 'TH 'unvoiced   'dental        'fricative)
     (vowel     'UH 'near-close 'back          'rounded)
     (vowel     'UW 'close      'back          'rounded)
     (consonant 'V  'voiced     'labio-dental  'fricative)
     (consonant 'W  'voiced     'bilabial      'approximant)
     (consonant 'Y  'voiced     'palatal       'approximant)
     (consonant 'Z  'voiced     'alveolar      'fricative)
     (consonant 'ZH 'voiced     'post-alveolar 'fricative))))

(defparameter *consonant-voiced-char-map*
  '((#\v . voiced)
    (#\u . unvoiced)))

(defparameter *consonant-poa-char-map*
  '((#\a . alveolar)
    (#\b . bilabial)
    (#\d . dental)
    (#\g . glottal)
    (#\l . labio-dental)
    (#\p . post-alveolar)
    (#\t . palatal)
    (#\v . velar)))

(defparameter *consonant-moa-char-map*
  '((#\a . affricate)
    (#\f . fricative)
    (#\l . lateral)
    (#\n . nasal)
    (#\p . plosive)
    (#\x . approximant)))

(defparameter *vowel-height-char-map*
  '((#\1 . open)
    (#\2 . near-open)
    (#\3 . open-mid)
    (#\4 . mid)
    (#\5 . close-mid)
    (#\6 . near-close)
    (#\7 . close)))

(defparameter *vowel-backness-char-map*
  '((#\1 . front)
    (#\2 . central)
    (#\3 . back)))

(defparameter *vowel-roundedness-char-map*
  '((#\r . rounded)
    (#\u . unrounded)))

(defun encode-phonemes (pres)
  "Replace any phoneme literal sequences in PRES with their encoded values.
  TODO: This is fundamentally flawed -- may need to enforce whitespace delimiting of phoneme literals."
  (reduce (lambda (str p)
            (ppcre:regex-replace-all
             (concatenate 'string (phoneme-representation p) "(?=([^a-zA-Z]|$))") ; Prevent ambiguity by checking boundaries
             str
             (string (encoded-char p))))
          (sort *phonemes* #'> :key (lambda (entry) (length (phoneme-representation entry)))) ;; TODO HACK! Prevent misparsing
          :initial-value pres))

(defun get-phoneme-by-name (phoneme-str)
  "Return a phoneme instance from *phonemes* given something like \"AE\""
  (find phoneme-str
        *phonemes*
        :test 'equal
        :key #'phoneme-representation))

(defun query-phonemes (type &key voiced place-of-articulation manner-of-articulation height backness roundedness)
  "Return a list of all phonemes matching query. Each parameter can be
either a single value to match, or a list thereof. Unprovided parameters are
considered wildcards."
  (labels

      ((equal-if-applicable (param against)
         (or (not param)
             (if (listp param)
                 (member against param :test #'equal)
                 (equal param against))))

       (matching-phoneme-p (phoneme)
         (and
          (equal type (phoneme-type phoneme))
          (or (equal 'vowel (phoneme-type phoneme))
              (and
               (equal-if-applicable voiced (voiced-p phoneme))
               (equal-if-applicable place-of-articulation (place-of-articulation phoneme))
               (equal-if-applicable manner-of-articulation (manner-of-articulation phoneme))))
          (or (equal 'consonant (phoneme-type phoneme))
              (and
               (equal-if-applicable height (height phoneme))
               (equal-if-applicable backness (backness phoneme))
               (equal-if-applicable roundedness (roundedness phoneme)))))))

    (remove-if-not #'matching-phoneme-p *phonemes*)))

(defun expand-phoneme-expression/vowels (opts)
  "Replace a vowel expression with a Perl-compatible character class which implements it."
  (destructuring-bind (&optional height-chars backness-chars roundedness-chars) opts
    (flet
        ((remap-char-generator (char-alist)
           (lambda (char)
             (let ((cns (assoc char char-alist)))
               (if (null cns)
                   (format t "TODO signal here")
                   (cdr cns))))))
      (let
          ((height-list (mapcar (remap-char-generator *vowel-height-char-map*)
                             height-chars))
           (backness-list (mapcar (remap-char-generator *vowel-backness-char-map*)
                             backness-chars))
           (roundedness-list (mapcar (remap-char-generator *vowel-roundedness-char-map*)
                           roundedness-chars)))
        (apply #'concatenate `(string "[" ,@(mapcar
                                             (cl-utilities:compose #'string #'encoded-char)
                                             (query-phonemes 'vowel
                                                             :backness backness-list
                                                             :height height-list
                                                             :roundedness roundedness-list))
                                      "]"))))))

(defun expand-phoneme-expression/consonants (opts)
  "Replace a consonant expression with a Perl-compatible character class which implements it."
  (destructuring-bind (&optional voiced-chars poa-chars moa-chars) opts
    (flet
        ((remap-char-generator (char-alist)
           (lambda (char)
             (let ((cns (assoc char char-alist)))
               (if (null cns)
                   (format t "TODO signal here")
                   (cdr cns))))))
      (let
          ((moa-list (mapcar (remap-char-generator *consonant-moa-char-map*)
                             moa-chars))
           (poa-list (mapcar (remap-char-generator *consonant-poa-char-map*)
                             poa-chars))
           (voiced (mapcar (remap-char-generator *consonant-voiced-char-map*)
                           voiced-chars)))
        (apply #'concatenate `(string "[" ,@(mapcar
                                             (cl-utilities:compose #'string #'encoded-char)
                                             (query-phonemes 'consonant
                                                             :place-of-articulation poa-list
                                                             :manner-of-articulation moa-list
                                                             :voiced voiced))
                                      "]"))))))

(defun expand-phoneme-expression (expstr)
  "Convert something like @<v,,p> to [...encoded phonemes...]"
  (let ((opts (if (>= (length expstr) 3)
                  (mapcar (lambda (str) (coerce str 'list))
                   (cl-utilities:split-sequence #\, (subseq expstr 2 (- (length expstr) 1))))
                  nil)))
    (case (aref expstr 0)
      (#\@ (expand-phoneme-expression/vowels opts))
      (#\# (expand-phoneme-expression/consonants opts))
      (#\% " (?:#*@#*) ")
      (t (error 'simple-error "Invalid phoneme expression string.")))))

(defun encode-regex/phoneme-expressions (pres)
  "Replace consonant, vowel, and syllable expressions with encoded values."
  (let ((expstr-scanner (ppcre:create-scanner "[@#%](?:<([a-zA-Z0-9]*(?:,[a-zA-Z0-9]*){0,3})>)?")))
    (loop while
         (multiple-value-bind (start stop)
             (ppcre:scan expstr-scanner pres)
           (if start
               (setf pres (concatenate 'string
                                       (subseq pres 0 start)
                                       (expand-phoneme-expression (subseq pres start stop))
                                       (subseq pres stop)))
               nil)))
    pres))

(defun encode-regex/phoneme-literals (pres)
  "Replace phoneme literals with their encoded values."
  (encode-phonemes pres))

(defun encode-regex (pres)
  "Return a fully-encoded version of the given string."
  (concatenate 'string
               "^"
               (remove #\ (encode-regex/phoneme-expressions
                           (encode-regex/phoneme-literals pres)))
               "$"))

;; Pronunciations

(defclass pronunciation ()
    ((phoneme-stress-alist :initarg :phoneme-stress-alist
                           :accessor phoneme-stress-alist)
     (encoded-str :accessor encoded-str)))

(defmethod print-object ((pr pronunciation) out)
  (print-unreadable-object (pr out :type t)
    (format out "~A" (mapcar (cl-utilities:compose #'phoneme-representation #'car)
                      (phoneme-stress-alist pr)))))

(defmethod initialize-instance :after ((pr pronunciation) &rest ignored)
  "Render the encoded version."
  (declare (ignore ignored))
  (setf (encoded-str pr)
        (coerce (mapcar (cl-utilities:compose #'encoded-char #'car) (phoneme-stress-alist pr))
                'string)))

(defun make-pronunciation (phoneme-strings)
  "Given '(\"K\" \"AE1\" \"T\"), yield a pronunciation instance."
  (make-instance 'pronunciation :phoneme-stress-alist
                      (mapcar (lambda (phoneme-stress-str)
                                (string-extract phoneme-stress-str
                                                "([a-zA-Z]+)(\\d)?"
                                                (phoneme-str stress-str)
                                                (cons
                                                 (get-phoneme-by-name phoneme-str)
                                                 (and stress-str (parse-integer stress-str)))))
                              phoneme-strings)))

;; Regex Generation

(defun insert (sequence element idx)
  "Insert an element `element' into `sequence' at index `idx'."
  (append (subseq sequence 0 idx)
          (list element)
          (subseq sequence idx)))

(defun seqjoin (result-type sequence delimiter)
  "Insert delimiter between every element in sequence."
  (let
      ((alternator 1))
    (flet
        ((merge-closure (elt-left elt-right)
           "Yields an infinite series of nil, t, nil, t, ..."
           (declare (ignorable elt-left elt-right))
           (> (setq alternator (* alternator -1)) 0)))
      (merge result-type
             sequence
             (make-list (- (length sequence) 1) :initial-element delimiter)
             #'merge-closure))))

(defun generate-regex/near-rhyme (phonemes)
  "Simple rhyme metapattern. This is currently defined to mean that every phoneme after
the first vowel phoneme matches, but with extra consonants interspersed."
  (let*
      ((first-vowel-pos (position-if (lambda (phoneme)
                                     (eq (phoneme-type phoneme) 'vowel))
                                   phonemes))
       (first-vowel-to-end (subseq phonemes first-vowel-pos))
       (representations (mapcar 'phoneme-representation first-vowel-to-end))
       (consonants-added (seqjoin 'list representations " #* ")))
    (apply 'concatenate 'string `(".* " ,@consonants-added " #*"))))

(defun generate-regex/perfect-rhyme (phonemes)
  "Exact rhyme metapattern. This is defined to mean that every phoneme after the first
vowel phoneme matches exactly."
  (let*
      ((first-vowel-pos (position-if (lambda (phoneme)
                                     (eq (phoneme-type phoneme) 'vowel))
                                   phonemes))
       (first-vowel-to-end (subseq phonemes first-vowel-pos))
       (representations (mapcar 'phoneme-representation first-vowel-to-end))
       (spaces-added (seqjoin 'list representations " ")))
    (apply 'concatenate 'string `(".* " ,@spaces-added))))

(defun generate-regex/alliteration (phonemes)
  "A regex which matches all words which begin with the same phoneme; this is
going to match many, many words for virtually any input."
  (concatenate 'string (phoneme-representation (first phonemes)) " .* "))

(defun generate-regex (metapattern pronunciation)
  "Return an unencoded phonetic regex which implements `metapattern' over
`pronunciation'."
  (let*
      ((phonemes-stresses (phoneme-stress-alist pronunciation))
       (phonemes          (mapcar #'car phonemes-stresses)))
    (case metapattern
      (perfect-rhyme (generate-regex/perfect-rhyme phonemes))
      (near-rhyme    (generate-regex/near-rhyme phonemes))
      (alliteration  (generate-regex/alliteration phonemes)))))

;; Dictionary Processing

(defclass simple-phonetic-dictionary ()
  ((words :initarg :words
          :accessor words
          :initform (make-hash-table :test 'equal)))
  (:documentation "A simple hash-based phonetic dictionary."))

(defgeneric regex-search (dict phonetic-regex)
  (:documentation "Find all words in this dict whose pronunciation matches the given phonetic regex"))

(defgeneric pronounce-word (dict word)
  (:documentation "Find all pronunciations for the given word in this dictionary."))

(defun normalize-word (dirty-word)
  "Maps something like \"FooBaR(1)\" to (values \"foobar\" 1)"
  (setf dirty-word (string-downcase dirty-word))
  (string-extract dirty-word "([^\\s\\(\\)]+)(?:\\((\\d+)\\))?" (word index)
                  (values
                   word
                   (and index (parse-integer index)))))

(defun normalize-phoneme (dirty-phoneme)
  "Maps Ah to AH"
  (string-upcase dirty-phoneme))

(defun learn-word (dict word phonemes)
  "Given a phonetic dictionary, word and a list of phonemes, sanitizes
things and save it."
  (let* ((clean-word     (normalize-word word))
         (entry          (gethash clean-word (words dict)))
         (clean-phonemes (mapcar #'normalize-phoneme phonemes))
         (pronunciation  (make-pronunciation clean-phonemes)))
    (setf (gethash clean-word (words dict)) (cons pronunciation entry))))

;; Glue

(defun make-matcher (pres)
  "Return a lambda form which tests if a given entry from a dict matches the given pres."
  (handler-bind
      ((cl-ppcre:ppcre-syntax-error
        #'(lambda (c)
            (declare (ignorable c))
            (return-from make-matcher nil))))
    (let ((scanner (ppcre:create-scanner (encode-regex pres))))
      (lambda (word pronunciations)
        (declare (ignorable word))
        (if (some (lambda (prn) (ppcre:scan scanner (encoded-str prn))) pronunciations)
            (list (cons word pronunciations))
            nil)))))

(defun from-cmudict (path)
  "Read pronunciations in from a cmudict formatted text file."
  (let
      ((new-dict (make-instance 'simple-phonetic-dictionary)))
    (with-open-file (stream path :external-format :LATIN-1)
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line))
        (cond
          ((and (>= (length line) 3) (string-equal ";;;" (subseq line 0 3))) ;; Skip comment lines
           )
          ;; TODO skip symbols "!EXCLAMATION-POINT"
          (t ;; In any other case, try to parse the line
           (let*
               ((tokens   (cl-utilities:split-sequence #\  line :remove-empty-subseqs t))
                (word     (car tokens))
                (phonemes (cdr tokens)))
             (learn-word new-dict word phonemes))))))
    new-dict))

(defmethod pronounce-word ((dict simple-phonetic-dictionary) dirty-word)
  "Get the phoneme sequences stored for this word."
  (gethash (normalize-word dirty-word) (words dict)))

(defmethod regex-search ((dict simple-phonetic-dictionary) pres)
  "Return a list of words which have a pronunciation matching regex."
  (let* ((matcher (make-matcher pres))
         (results nil)
         (match-aggregator (lambda (matchret)
                             (if matchret
                                 (setq results (append matchret results))))))
    (if matcher
        (maphash (lambda (word prn)
                   (funcall match-aggregator (funcall matcher word prn)))
                 (words dict)))
    results))

(defmethod find-metapattern ((dict simple-phonetic-dictionary) metapattern word)
  "Find words in `dict' which satisfy an internally generated regular expression implementing the
provided `metapattern' over `word'."
  (let*
      ((first-pronunciation (first (pronounce-word dict word)))
       (regex (generate-regex metapattern first-pronunciation)))
    (regex-search dict regex)))

(defmethod test-metapattern ((dict simple-phonetic-dictionary) metapattern word-a word-b)
  "Test whether metapattern `metapattern' for word `word-a' is held over word `word-b'.
TODO: This is currently using the first pronunciation for `word-a' over any pronunciation
for `word-b'. It should probably just be an any/any (cross-product) sort of thing."
  (let*
      ((first-pronunciation-a (first (pronounce-word dict word-a)))
       (pronunciations-b      (pronounce-word dict word-b))
       (pres                  (generate-regex metapattern first-pronunciation-a))
       (matcher               (make-matcher pres)))
    (funcall matcher word-b pronunciations-b)))

;; Convenience

(defun the-words (dict-entries)
  "Many cl-phonetic APIs return an alist mapping words to pronunciations. Call this if
you just want the words themselves."
  (mapcar 'car dict-entries))

(defun the-pronunciations (dict-entries)
  "Many cl-phonetic APIs return an alist mapping words to pronunciations. Call this if
you just want the words themselves."
  (mapcar 'cdr dict-entries))
