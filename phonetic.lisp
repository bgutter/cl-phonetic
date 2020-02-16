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
  ())

(let ((phoneme-counter 0))
  (defmethod initialize-instance :after ((p phoneme) &key name)
             (setf (encoded-value p) (+ +unicode-pua-base+ phoneme-counter))
             (setf (phoneme-representation p) (symbol-name name))
             (setf phoneme-counter (1+ phoneme-counter))))

(defmethod print-object ((p phoneme) out)
  (print-unreadable-object (p out :type t)
    (format out "~A" (list (phoneme-name p)
                           (phoneme-type p)
                           (phoneme-representation p)
                           (encoded-value p)))))

(defmethod encoded-char ((p phoneme))
  (code-char (encoded-value p)))

(defparameter *phonemes*
  (macrolet ((vowel (name)
               `(make-instance 'vowel
                               :name ,name
                               :type :vowel))
             (consonant (name voiced poa moa)
               `(make-instance 'consonant
                               :name ,name
                               :type :consonant
                               :voiced (equal ,voiced :voiced)
                               :place-of-articulation ,poa
                               :manner-of-articulation ,moa)))
    (list
     (vowel     :AA)
     (vowel     :AE)
     (vowel     :AH)
     (vowel     :AO)
     (vowel     :AW)
     (vowel     :AY)
     (consonant :B  :voiced   :bilabial      :plosive)
     (consonant :CH :unvoiced :post-alveolar :affricate)
     (consonant :D  :voiced   :alveolar      :plosive)
     (consonant :DH :voiced   :dental        :fricative)
     (vowel     :EH)
     (vowel     :ER)
     (vowel     :EY)
     (consonant :F  :unvoiced :labio-dental  :fricative)
     (consonant :G  :voiced   :velar         :plosive)
     (consonant :HH :unvoiced :glottal       :fricative)
     (vowel     :IH)
     (vowel     :IY)
     (consonant :JH :voiced   :post-alveolar :affricate)
     (consonant :K  :unvoiced :velar         :plosive)
     (consonant :L  :voiced   :alveolar      :lateral)
     (consonant :M  :voiced   :bilabial      :nasal)
     (consonant :N  :voiced   :alveolar      :nasal)
     (consonant :NG :voiced   :velar         :nasal)
     (vowel     :OW)
     (vowel     :OY)
     (consonant :P  :unvoiced :bilabial      :plosive)
     (consonant :R  :voiced   :post-alveolar :approximant)
     (consonant :S  :unvoiced :alveolar      :fricative)
     (consonant :SH :unvoiced :post-alveolar :fricative)
     (consonant :T  :unvoiced :alveolar      :plosive)
     (consonant :TH :unvoiced :dental        :fricative)
     (vowel     :UH)
     (vowel     :UW)
     (consonant :V  :voiced   :labio-dental  :fricative)
     (consonant :W  :voiced   :bilabial      :approximant)
     (consonant :Y  :voiced   :palatal       :approximant)
     (consonant :Z  :voiced   :alveolar      :fricative)
     (consonant :ZH :voiced   :post-alveolar :fricative))))

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

(defun query-phonemes (&key (type nil type-p) (place-of-articulation nil poa-p) (manner-of-articulation nil moa-p))
  "Return a list of all phonemes matching query. Each parameter can be
either a single value to match, or a list thereof."
  (remove-if-not #'(lambda (phoneme)
                     (let ((this-type (phoneme-type phoneme))
                           (this-poa  (and (eq (phoneme-type phoneme) :consonant)
                                           (place-of-articulation phoneme)))
                           (this-moa  (and (eq (phoneme-type phoneme) :consonant)
                                           (manner-of-articulation phoneme))))
                       ;; TODO: This code should be more DRY
                       (and
                        (or (not type-p)
                            (if (listp type)
                                (member this-type type)
                                (equal this-type type)))
                        (or (not poa-p)
                            (if (listp place-of-articulation)
                                (member this-poa place-of-articulation)
                                (equal this-poa place-of-articulation)))
                        (or (not moa-p)
                            (if (listp manner-of-articulation)
                                (member this-moa manner-of-articulation)
                                (equal this-moa manner-of-articulation))))))
                 *phonemes*))

(defun expand-phoneme-expression/vowels (vowel-expstr)
  "Replace a vowel expression with a Perl-compatible character class which implements it."
  (declare (ignorable vowel-expstr))
  ;; TODO
  (apply #'concatenate `(string "[" ,@(mapcar
                                       (cl-utilities:compose #'string #'encoded-char)
                                       (query-phonemes :type :vowel))
                                "]")))

(defun expand-phoneme-expression/consonants (opts)
  "Replace a consonant expression with a Perl-compatible character class which implements it."
  (destructuring-bind (&optional voiced-chars poa-chars moa-chars) opts
    (let
        ((poa-list (reduce (lambda (lis pair)
                             (format t "PAIR: ~A~%LIST:~A ~%" pair lis)
                             (substitute (car pair)
                                         (cadr pair)
                                         lis))
                           '((:bilabial      #\b)
                             (:post-alveolar #\p)
                             (:alveolar      #\a))
                           :initial-value (coerce poa-chars 'list)))
         (moa-list ())
         (voice    ()))
      (apply #'concatenate `(string "[" ,@(mapcar
                                           (cl-utilities:compose #'string #'encoded-char)
                                           (query-phonemes :type :consonant
                                                           :place-of-articulation poa-list
                                                           :manner-of-articulation moa-list
                                                           :voiced voiced))
                                    "]")))))

(defun expand-phoneme-expression (expstr)
  "Convert something like @<v,,p> to [...encoded phonemes...]"
  (let ((opts (if (>= (length expstr) 3)
                  (cl-utilities:split-sequence #\, (subseq expstr 2 (- (length expstr) 1)))
                  nil)))
    (case (aref expstr 0)
      (#\@ (expand-phoneme-expression/vowels opts))
      (#\# (expand-phoneme-expression/consonants opts))
      (#\% " #*@#* ")
      (t (error 'simple-error "Invalid phoneme expression string.")))))

(defun encode-regex/phoneme-expressions (pres)
  "Replace consonant, vowel, and syllable expressions with encoded values."
  (let ((expstr-scanner (ppcre:create-scanner "[@#%](?:<([a-zA-Z]*(?:,[a-zA-Z]*){0,3})>)?")))
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
  (format t "Encoding ~a~%" pres)
  (remove #\ (encode-regex/phoneme-expressions
           (encode-regex/phoneme-literals pres))))

;; Pronunciations

(defclass pronunciation ()
    ((phoneme-stress-alist :initarg :phoneme-stress-alist
                           :accessor phoneme-stress-alist)
     (encoded-str :accessor encoded-str)))

(defmethod print-object ((pr pronunciation) out)
  (print-unreadable-object (pr out :type t)
    (format out "~A" (phoneme-stress-alist pr))))

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

;; cmudict Processing

(defparameter *words* (make-hash-table :test 'equal))

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

(defun learn-word (word phonemes)
  "Given a word and a list of phonemes, sanitizes things and saves it."
  (let* ((clean-word     (normalize-word word))
         (entry          (gethash clean-word *words*))
         (clean-phonemes (mapcar #'normalize-phoneme phonemes))
         (pronunciation  (make-pronunciation clean-phonemes)))
    (setf (gethash clean-word *words*) (cons pronunciation entry))))

(defun read-cmudict (path)
  "Read pronunciations in from a cmudict formatted text file."
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
           (learn-word word phonemes)))))))

;; Glue

(defun make-matcher (pres)
  "Return a lambda form which tests if a given entry in *words* matches the given pres."
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

;; API

#|
Maybe an OO interface -- something like:

(let
    ((my-phonetic (phonetic:from-dict "~/cmudict")))

  (find-words my-phonetic "@/v,,f/+ # @/,,p/")
  ;; -> at least one voiced, fricative consonant, followed by any vowel, followed by a plosive consonant

  (pronounce-word my-phonetic "CATS"))
  ;; -> '("K" "AE" "T" "S")

|#

(defun pronounce-word (dirty-word)
  "Get the phoneme sequences stored for this word."
  (gethash (normalize-word dirty-word) *words*))

(defun find-words (pres)
  "Return a list of words which have a pronunciation matching regex."
  (let* ((matcher (make-matcher pres))
         (results nil)
         (match-aggregator (lambda (matchret)
                             (if matchret
                                 (setq results (append matchret results))))))
    (if matcher
        (maphash (lambda (word prn) (funcall match-aggregator (funcall matcher word prn))) *words*))
    results))

