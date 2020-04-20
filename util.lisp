;;
;; util.lisp
;;
;; Various utilities
;;

(in-package :cl-phonetic)

(defmacro string-extract (str re names-list &rest body)
  "Basically, `multiple-value-bind', but for CL-PPCRE capture groups.
Scan STR with RE, binding NAMES-LIST to the resulting matches, and
finally, evaluate BODY. Behavior is undefined if RE does not match
STR, or if the number of capture groups is not equal to the number of
names in NAMES-LIST."
  (let ((whole-sym   (gensym))
        (matches-sym (gensym)))
    `(multiple-value-bind (,whole-sym ,matches-sym)
         (ppcre:scan-to-strings ,re ,str)
       (declare (ignore ,whole-sym))
       (destructuring-bind ,names-list (coerce ,matches-sym 'list)
         ,@body))))

(defun insert (sequence element idx)
  "Insert an element ELEMENT into SEQUENCE at index IDX."
  (append (subseq sequence 0 idx)
          (list element)
          (subseq sequence idx)))

(defun seqjoin (result-type sequence delimiter)
  "Insert DELIMITER between every element in SEQUENCE."
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

(defun glue-strings (&rest strings-or-lists)
  "Combine a bunch of strings, or lists of strings, into one final string.
TODO: This is wildly inefficient. Should be doable with a single CONCATENATE."
  (apply #'concatenate 'string (flatten strings-or-lists)))

(defun join-strings (delimiter &rest strings-or-lists)
  "seq-join for strings"
  (glue-strings (seqjoin 'list (flatten strings-or-lists) delimiter)))
