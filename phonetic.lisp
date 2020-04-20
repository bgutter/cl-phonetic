;;
;; phonetic.lisp
;;
;; Search for phoneme patterns using regular expressions.
;;

(in-package :cl-phonetic)

;; Phonemes & Encoding

(defconstant +unicode-pua-base+ #x00E000
  "The first unicode value where encoded phonemes will be defined.
Currently, pattern matching is done by the CL-PPCRE package. This is
accomplished by masquerading phonemes as unicode characters. We map
phonemes to characters in the Unicode Private Use Area to help ensure
that malformed input regular expressions are consistently and
obviously broken. There should be enough space here to fit all English
phonemes.")

(defclass phoneme ()
  ((name :initarg :name
         :accessor phoneme-name)
   (type :initarg :type
         :accessor phoneme-type)
   (representation :initarg :representation
                   :accessor phoneme-representation)
   (encoded-value :initarg :encoded-value
                  :accessor encoded-value))
  (:documentation
   "Represents an English phoneme, including its textual
  representation, and CL-PPCRE encoded representation."))

(defun vowel-p (phoneme)
  "Is this `phoneme' object a vowel?"
  (equal 'vowel (phoneme-type phoneme)))

(defun consonant-p (phoneme)
  "Is this `phoneme' object a consonant?"
  (equal 'consonant (phoneme-type phoneme)))

(defclass consonant (phoneme)
  ((voiced :initarg :voiced
           :accessor voiced-p)
   (place-of-articulation :initarg :place-of-articulation
                          :accessor place-of-articulation)
   (manner-of-articulation :initarg :manner-of-articulation
                           :accessor manner-of-articulation))
  (:documentation
   "A specialization of `phoneme' to represent English consonants,
   including information about their voicing, place of articulation,
   and manner of articulation."))

(defclass vowel (phoneme)
  ((height :initarg :height
           :accessor height
           :initform nil)
   (backness :initarg :backness
             :accessor backness
             :initform nil)
   (roundedness :initarg :roundedness
                :accessor roundedness
                :initform nil))
  (:documentation
   "A specialization of `phoneme' to represent English vowels,
   including information about their height, backness, and
   roundedness."))

;;
;; After initialization of any phoneme
;;   1. Generate a unique encoding value and store it in the
;;      encoded-value slot.
;;   2. Fill the representation slot as the name of the symbol
;;      supplied for NAME.
;;
(let ((phoneme-counter 0))
  (defmethod initialize-instance :after ((p phoneme) &key name)
             (setf (encoded-value p) (+ +unicode-pua-base+ phoneme-counter))
             (setf (phoneme-representation p) (symbol-name name))
             (setf phoneme-counter (1+ phoneme-counter))))

(defmethod print-object ((p phoneme) out)
  "When a phoneme is printed, print its textual representation."
  (print-unreadable-object (p out :type t)
    (format out "~A" (phoneme-representation p))))

(defmethod encoded-char ((p phoneme))
  "Return a character whose Unicode value is the encoding value of the
given `phoneme' object PHONEME."
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
     (consonant 'ZH 'voiced     'post-alveolar 'fricative)))
  "Global table of `phoneme' objects. Represents the entirety of
  ARPABET.")

(defparameter *consonant-voiced-char-map*
  '((#\v . voiced)
    (#\u . unvoiced))
  "Maps consonant-class voicing characters to their symbols.")

(defparameter *consonant-poa-char-map*
  '((#\a . alveolar)
    (#\b . bilabial)
    (#\d . dental)
    (#\g . glottal)
    (#\l . labio-dental)
    (#\p . post-alveolar)
    (#\t . palatal)
    (#\v . velar))
  "Maps consonant-class place-of-articulation characters to their
  symbols.")

(defparameter *consonant-moa-char-map*
  '((#\a . affricate)
    (#\f . fricative)
    (#\l . lateral)
    (#\n . nasal)
    (#\p . plosive)
    (#\x . approximant))
  "Maps consonant-class manner-of-articulation characters to their
  symbols.")

(defparameter *vowel-height-char-map*
  '((#\1 . open)
    (#\2 . near-open)
    (#\3 . open-mid)
    (#\4 . mid)
    (#\5 . close-mid)
    (#\6 . near-close)
    (#\7 . close))
  "Maps vowel-class height characters to their symbols.")

(defparameter *vowel-backness-char-map*
  '((#\1 . front)
    (#\2 . central)
    (#\3 . back))
  "Maps vowel-class backness characters to their symbols.")

(defparameter *vowel-roundedness-char-map*
  '((#\r . rounded)
    (#\u . unrounded))
  "Maps vowel-class roundedness characters to their symbols.")

(defun encode-phonemes (phonemic-regex-string)
  "Given a phoneme regular expression string, replace every phoneme
literal with its CL-PPCRE encoded character, and return the result.

For example, if PHONEME-REGEX-STRING is something like \"K AE T\",
then the returned string will be only 5 characters long; 3
encoded (and possibly unprintable) characters joined by the remaining
two spaces.

Note that behavior is undefined (and frankly broken) if consecutive
phoneme literals are not separated by whitespace."
  (reduce (lambda (str phoneme)
            (ppcre:regex-replace-all
             (glue-strings (phoneme-representation phoneme) "(?=([^a-zA-Z]|$))") ; Prevent ambiguity by checking boundaries
             str
             (string (encoded-char phoneme))))
          (sort *phonemes* #'> :key (lambda (entry) (length (phoneme-representation entry)))) ;; TODO HACK! Prevent misparsing
          :initial-value phonemic-regex-string))

(defun get-phoneme-by-name (phoneme-str)
  "Return a `phoneme' object from `*phonemes*' given something like \"AE\""
  (find phoneme-str
        *phonemes*
        :test 'equal
        :key #'phoneme-representation))

(defun query-phonemes (type &key voiced place-of-articulation manner-of-articulation height backness roundedness)
  "Filter the global `*phonemes*' list and return the result.

Parameter TYPE should be either 'VOWEL or 'CONSONANT.

Parameters VOICED, PLACE-OF-ARTICULATION, and MANNER-OF-ARTICULATION
are only applicable when TYPE is 'CONSONANT, while HEIGHT,
BACKNESS, and ROUNDEDNESS are applicable only when TYPE is
'VOWEL. All of these parameters may be omitted or nil, in which case
they will not be used for filtering. If they are supplied with list
values, then filtering will preserve only phonemes whose relevant
properties are in the supplied lists. For any other parameter value,
filtering will preserve only phonemes whose relevant properties are
`EQUAL' to the supplied value.

Return a list of all phonemes matching query. Each parameter can be
either a single value to match, or a list thereof. Unprovided
parameters are considered wildcards."
  (labels

      ((phoneme-prop-fits-query-props (query-props phoneme-prop)
         (cond
           ((null query-props)  t)
           ((listp query-props) (member phoneme-prop query-props :test #'equal))
           (t                   (equal query-props phoneme-prop))))

       (matching-phoneme-p (phoneme)
         (and
          (equal type (phoneme-type phoneme))
          (or (vowel-p phoneme)
              (and
               (phoneme-prop-fits-query-props voiced (voiced-p phoneme))
               (phoneme-prop-fits-query-props manner-of-articulation (manner-of-articulation phoneme))
               (phoneme-prop-fits-query-props place-of-articulation (place-of-articulation phoneme))))
          (or (consonant-p phoneme)
              (and
               (phoneme-prop-fits-query-props height (height phoneme))
               (phoneme-prop-fits-query-props backness (backness phoneme))
               (phoneme-prop-fits-query-props roundedness (roundedness phoneme)))))))

    (remove-if-not #'matching-phoneme-p *phonemes*)))

(defun lookup-phoneme-class-chars (supplied-char-list char-mapping-alist)
  "Given a list of chars representing an argument to a phoneme
class (like #\v in \"#<v,,>\", etc), return their associated
symbols (like 'voiced, 'plosive, 'open-mid, etc)."
  (mapcar (lambda (char)
            (let
                ((sym-pair (assoc char char-mapping-alist)))
              (if (null sym-pair)
                  (error "Cannot find mapping for character ~A!" char)
                  (cdr sym-pair))))
          supplied-char-list))

(defun compile-phoneme-charclass (phoneme-list)
  "Return a string which contains a CL-PPCRE square-bracket
expression, whose contained characters are the encoded-character
representations of the phonemes in PHONEME-LIST."
  (let
      ((phoneme-encoded-strs (mapcar (cl-utilities:compose #'string #'encoded-char) phoneme-list)))
    (glue-strings "[" phoneme-encoded-strs "]")))

(defun expand-phoneme-expression/vowels (&optional height-chars backness-chars roundedness-chars)
  "Given the char arguments to a \"@<,,>\" expression, return an
encoded regex string to match those phonemes."
  (compile-phoneme-charclass
   (query-phonemes 'vowel
                   :height      (lookup-phoneme-class-chars height-chars      *vowel-height-char-map*)
                   :backness    (lookup-phoneme-class-chars backness-chars    *vowel-backness-char-map*)
                   :roundedness (lookup-phoneme-class-chars roundedness-chars *vowel-roundedness-char-map*))))

(defun expand-phoneme-expression/consonants (&optional voiced-chars poa-chars moa-chars)
  "Given the char arguments to a \"#<,,>\" expression, return an
encoded regex string to match those phonemes."
  (compile-phoneme-charclass
   (query-phonemes 'consonant
                   :voiced                 (lookup-phoneme-class-chars voiced-chars *consonant-voiced-char-map*)
                   :manner-of-articulation (lookup-phoneme-class-chars moa-chars    *consonant-moa-char-map*)
                   :place-of-articulation  (lookup-phoneme-class-chars poa-chars    *consonant-poa-char-map*))))

(defun expand-phoneme-expression (expstr)
  "Process a single phoneme-class expression, such as \"@\",
\"@<,2,>\", \"%\", or \"#<>\". Returns an encoded regex string."
  (let ((phoneme-class-option-chars
          (and (>= (length expstr) 3)
               (mapcar (lambda (str) (coerce str 'list))
                       (cl-utilities:split-sequence #\, (subseq expstr 2 (- (length expstr) 1)))))))
    (case (aref expstr 0)
      (#\@ (apply #'expand-phoneme-expression/vowels phoneme-class-option-chars))
      (#\# (apply #'expand-phoneme-expression/consonants phoneme-class-option-chars))
      (#\% " (?:#*@#*) ")
      (t (error "Invalid phoneme expression string: ~A." expstr)))))

(defun encode-regex/phoneme-expressions (pres)
  "Find and replace all consonant and vowel class expressions with
plain Jane CL-PPCRE square-bracket expressions."
  (let ((expstr-scanner (ppcre:create-scanner "[@#%](?:<([a-zA-Z0-9]*(?:,[a-zA-Z0-9]*){0,3})>)?")))
    (loop while
         (multiple-value-bind (start stop)
             (ppcre:scan expstr-scanner pres)
           (if start
               (setf pres (glue-strings (subseq pres 0 start)
                                        (expand-phoneme-expression (subseq pres start stop))
                                        (subseq pres stop))))))
    pres))

(defun encode-regex/phoneme-literals (pres)
  "Replace phoneme literals with their encoded values."
  (encode-phonemes pres))

(defun encode-regex (pres)
  "Return a fully-encoded version of the given string."
  (glue-strings "^"
                (-<> pres
                     (encode-regex/phoneme-literals)
                     (encode-regex/phoneme-expressions)
                     (remove #\  <>))
                "$"))

;; Pronunciations

(defclass pronunciation ()
    ((phoneme-stress-alist :initarg :phoneme-stress-alist
                           :accessor phoneme-stress-alist)
     (encoded-str :accessor encoded-str))
  (:documentation
   "Represents a pronunciation; a sequence of phonemes and
   stresses. Includes a slot to cache a string which is a sequence of
   the character encodings of each phoneme in the pronunciation."))

(defmethod print-object ((pr pronunciation) out)
  "When a `PRONUNCIATION' object is printed, print the textual
representation of each of its phonemes."
  (print-unreadable-object (pr out :type t)
    (format out "~A" (join-strings " "
                                   (mapcar (cl-utilities:compose #'phoneme-representation #'car)
                                             (phoneme-stress-alist pr))))))

(defmethod initialize-instance :after ((pr pronunciation) &rest ignored)
  "After each `PRONUNCIATION' is created, automatically compile its
encoded string representation for fast matching."
  (declare (ignore ignored))
  (setf (encoded-str pr)
        (coerce (mapcar (cl-utilities:compose #'encoded-char #'car) (phoneme-stress-alist pr))
                'string)))

(defun make-pronunciation (phoneme-strings)
  "Given a list of phoneme literal strings, like '(\"K\" \"AE1\"
\"T\"), yield a `PRONUNCIATION' object."
  (let
      ((phoneme-stress-alist (mapcar (lambda (phoneme-stress-str)
                                       (string-extract phoneme-stress-str "([a-zA-Z]+)(\\d)?" (phoneme-str stress-str)
                                                       (cons (get-phoneme-by-name phoneme-str)
                                                             (and stress-str (parse-integer stress-str)))))
                                     phoneme-strings)))
    (make-instance 'pronunciation
                   :phoneme-stress-alist phoneme-stress-alist)))

(defun join-pronunciations (&rest pronunciations)
  "Return a combined `pronunciation'."
  (make-instance 'pronunciation
                 :phoneme-stress-alist (apply #'concatenate 'list
                                              (mapcar #'phoneme-stress-alist
                                                      pronunciations))))

;; Regex Generation

(defun generate-regex/rhyme (phonemes &rest ignored &key loose near)
  "Matches all words where every phoneme after the first vowel phoneme
matches exactly.
When LOOSE is non-nil, additional consonants may be interspersed.
When NEAR is non-nil, vowel phonemes may match similar vowels."
  (declare (ignorable ignored))
  (let*
      ((first-vowel-pos    (position-if (lambda (phoneme)
                                          (eq (phoneme-type phoneme) 'vowel))
                                        phonemes))
       (first-vowel-to-end (subseq phonemes first-vowel-pos))
       (pad-str           (if loose " #* " " ")))
    (glue-strings
     ".* "
     (join-strings pad-str
                   (mapcar (lambda (phoneme)
                             (if (and near (vowel-p phoneme) (backness phoneme))
                                 (join-strings " "
                                               "["
                                               (mapcar #'phoneme-representation
                                                       (query-phonemes 'vowel :backness (backness phoneme)))
                                               "]")
                                 (phoneme-representation phoneme)))
                           first-vowel-to-end)
                   pad-str))))

(defun generate-regex/alliteration (phonemes &rest ignored)
  "A regex which matches all words which begin with the same phoneme; this is
going to match many, many words for virtually any input."
  (declare (ignorable ignored))
  (glue-strings (phoneme-representation (first phonemes)) " .* "))

(defun generate-regex/assonance (phonemes &rest ignored &key loose)
  "Matches all words which contain the same vowels, with any consonants intermixed.
When LOOSE is non-nil, additional vowels may occur before or after the sequence."
  (declare (ignorable ignored))
  (let
      ((pad-str (if loose " .* " " #* ")))
    (glue-strings pad-str
                  (join-strings " #* " (mapcar #'phoneme-representation (remove-if-not #'vowel-p phonemes)))
                  pad-str)))

(defun generate-regex/consonance (phonemes &rest ignored &key loose)
  "Generate a regex which will find repetitions of consonants in this phoneme sequence.
When LOOSE is non-nil, additional consonants are allowed."
  (declare (ignorable ignored))
  (let
      ((pad-str (if loose " .* " " @* ")))
    (glue-strings " @* "
                  (join-strings pad-str (mapcar #'phoneme-representation
                                                (remove-if-not #'consonant-p
                                                               phonemes)))
                  " @* ")))

(defun generate-regex (metapattern pronunciation &rest generator-options)
  "Return an unencoded phonetic regex which implements METAPATTERN over
PRONUNCIATION."
  (let*
      ((phonemes-stresses (phoneme-stress-alist pronunciation))
       (phonemes          (mapcar #'car phonemes-stresses))
       (generator-func    (case metapattern
                            (rhyme         #'generate-regex/rhyme)
                            (assonance     #'generate-regex/assonance)
                            (consonance    #'generate-regex/consonance)
                            (alliteration  #'generate-regex/alliteration))))
    (apply generator-func phonemes generator-options)))

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

(defun normalize-word (word)
  "Downcase & keep only apostrophes."
  (-<> word
      (string-downcase)
      (ppcre:regex-replace-all "[^a-z']" <> "")))

(defun decode-cmudict-word (word-val-pair)
  "Maps something like \"FooBaR(1)\" to (values \"foobar\" 1)"
  (string-extract word-val-pair "([^\\s\\(\\)]+)(?:\\((\\d+)\\))?" (word index)
                  (values
                   (normalize-word word)
                   (and index (parse-integer index)))))

(defun normalize-phoneme (dirty-phoneme)
  "Fix case, and potentially other cleanup."
  (string-upcase dirty-phoneme))

(defun learn-word (dict word phonemes)
  "Given a phonetic dictionary, word and a list of phonemes, sanitizes
things and save it."
  (let* ((clean-word     (decode-cmudict-word word))
         (entry          (gethash clean-word (words dict)))
         (clean-phonemes (mapcar #'normalize-phoneme phonemes))
         (pronunciation  (make-pronunciation clean-phonemes)))
    (setf (gethash clean-word (words dict)) (cons pronunciation entry))))

;; utterance handling

(defun tokenize-utterance (utterance)
  "Split English text into sequence of normalized words."
  (mapcar #'normalize-word
          (ppcre:split "\\s+" utterance)))

(defun pronounce-utterance (dict utterance)
  "Return a `pronunciation' from UTTERANCE.
UTTERANCE may be either a string of English text, or a list of
tokenized words."
  (let*
      ((utt-tokens (if (listp utterance)
                       utterance
                       (tokenize-utterance utterance)))
       (utt-pronunciations (mapcar (lambda (w)
                                     (first (pronounce-word dict w)))
                                   utt-tokens)))
    (apply #'join-pronunciations (remove-if #'null utt-pronunciations))))

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

(defmethod find-metapattern ((dict simple-phonetic-dictionary) metapattern word &rest generator-options)
  "Find words in DICT which satisfy an internally generated regular
expression implementing the provided METAPATTERN over WORD. Any
additional arguments are forwarded to `GENERATE-REGEX'."
  (let*
      ((pronunciation (if (typep word 'pronunciation)
                          word
                          (first (pronounce-word dict word))))
       (regex         (apply #'generate-regex metapattern pronunciation generator-options)))
    (regex-search dict regex)))

(defmethod test-metapattern ((dict simple-phonetic-dictionary) metapattern word-a word-b)
  "Test whether METAPATTERN for word WORD-A is held over word WORD-B.
TODO: This is currently using the first pronunciation for WORD-A over
any pronunciation for WORD-B. It should probably just be an
any/any (cross-product) sort of thing."
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
you just want the pronunciations themselves."
  (mapcar 'cdr dict-entries))
