;;; CAVIART-VCGEN - A verification condition generator for the CAVI-ART project
;;; developed originally at GPD UCM.
;;; Copyright (C) 2016 Santiago Saavedra López, Grupo de Programación Declarativa -
;;; Universidad Complutense de Madrid
;;;
;;; This file is part of CAVIART-VCGEN.
;;;
;;; CAVIART-VCGEN is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Affero General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; CAVIART-VCGEN is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with CAVIART-VCGEN.  If not, see <http://www.gnu.org/licenses/>.

(cl:defpackage :ir.mlw.formatter
  (:use :cl :ir.vc.theories)

  ;;; Export IR keywords
  (:export :assertion :precd :postcd #:true #:false)
  (:export #:-> #:<-> #:and #:or #:forall #:exists)
  (:export :the :type)
  (:export :tuple)
  (:export :verification-unit)
  (:export :predicate)
  (:export :int)
  (:export :bool :true :false)
  (:export :*assume-verified* :*verify-only* :*external-functions* :*goal-set-hook* :*default-goal-set-hook*)
  (:export :define :asserts :lettype :letvar :letconst :let :let* :letfun :case :default "@" "@@")
  ;;; End IR keywords


  
  (:export #:clir->mlw #:clir-file->mlw #:generate-module #:*clir-extension*)
  (:export #:read-clir-file))
(cl:in-package :ir.mlw.formatter)
(defparameter *indent-level* 1)
(defparameter *indent-size* 4)


(defun get-assertions (function-body)
  "Return the (possibly NIL) assertions in a FUNCTION-BODY."
  (let* ((d (cdr (assoc 'declare function-body)))
         (a (cdr (assoc 'assertion d)))
         (pre (second (assoc 'precd a)))
         (post (second (assoc 'postcd a))))
    (list pre post)))

(defun handle-typed-arguments (typed-lambda-list)
  (format nil "~:{(~(~A~): ~(~A~))~:^ ~}" typed-lambda-list))

(defun handle-return-type (typed-result-list)
  (if (> (length typed-result-list) 1)

      ;; On multiple-values: use parentheses
      (format nil "(~:{~*~(~A~)~:^,~})" typed-result-list)

      ;; On single value, trivial
      (format nil "~:{~(~*~A~)~}" typed-result-list)))

(defun drop-decls (body)
  (if (and (consp (car body))
           (eq (car (car body))
               'declare))
      (drop-decls (cdr body))
      body))

(defun clir-formula-to-string (formula &key tupled)
  (let ((inner (clir-formula-to-string% formula)))
    (when inner
      (if (cddr tupled)
                                        ; More than one result
                                        ; parameter: introduce a
                                        ; destructuring let
          (format nil
                  "~@<~15Ilet ~A = ~A in ~:@_~A~:>"
                  (handle-let-lhs (cdr tupled))
                  (clir-formula-to-string (car tupled))
                  (clir-formula-to-string formula))

                                        ; Else we need to replace our
                                        ; parameter by "result", as
                                        ; mlw wants it.
          (clir-formula-to-string% (replace-symbol formula (caadr tupled) (car tupled)))))))

(defun replace-symbol (formula original-symbol replacement-symbol)
  (typecase formula
    ((or nil number) formula)
    ((or symbol string) (or (and (equal formula original-symbol)
                                 replacement-symbol)
                            formula))
    (cons (cons (replace-symbol (car formula) original-symbol replacement-symbol)
                (replace-symbol (cdr formula) original-symbol replacement-symbol)))
    (t (error "Unknown formula type for formula: ~S" formula))))


(defun clir-formula-to-string% (formula)
  (labels ((is-infix (op)
             (and (symbolp op)
                  (member op
                          '(+ - * / < <= >= = > % <>)
                          :test (lambda (a b) (string-equal (symbol-name a) (symbol-name b))))))
           (maybe-downcase-symbol (symbol)
             (when symbol
               (let ((term-as-string (symbol-name symbol)))
                 (if (equal term-as-string
                            (string-upcase term-as-string))
                     (string-downcase term-as-string)
                     term-as-string))))
           (clir-term-to-string (term &optional recursive)
             (typecase term
               (number (write-to-string term))
               (symbol (maybe-downcase-symbol term))
               (string term)
               (cons (case (first term)
                       (quote (second term))
                       (the (string-capitalize (write-to-string (third term))))
                       (@ (apply-predicate (rest term) recursive))
                       (t (error "Term ~S not understood." term))))
               (t (error "Term ~S not understood." term))))
           (is-array-access (fname)
             (string-equal (symbol-name fname)
                           :get))
           (apply-predicate (p &optional recursive)
             (let ((predicate-str
                    (if (is-infix (first p))
                        (format nil (format nil "~~{(~~A)~~^ ~A ~~}"
                                            (first p))
                                (mapcar (lambda (x) (clir-term-to-string x t)) (rest p)))
                        (if (is-array-access (first p))
                            (format nil "~(~A[~A]~)" (second p) (third p))
                            (format nil "~(~A~) ~{~A~^ ~}" (first p) (mapcar (lambda (x) (clir-term-to-string x t)) (rest p)))))))
               (if recursive
                   (format nil "(~A)" predicate-str)
                   predicate-str))))
    (typecase formula
      (symbol (maybe-downcase-symbol formula))
      (number (write-to-string formula))
      (string formula)
      (cons (case (car formula)
              (forall (format nil "forall ~:{~(~A:~A~)~:^,~}. ~A"
                               (second formula)
                               (if (third formula)
                                   (clir-formula-to-string (third formula))
                                   "")))
              (-> (format nil "~{(~A)~^ -> ~}" (mapcar #'clir-formula-to-string (rest formula))))
              (and (format nil "(~{(~A)~^ /\\ ~})" (mapcar #'clir-formula-to-string (rest formula))))
              (or (format nil "~{(~A)~^ \\/ ~}" (mapcar #'clir-formula-to-string (rest formula))))
              (:postcd_placeholder (format nil "true (* POSTCD OF ~A([~A]) *)" (second formula) (rest formula)))
              (:precd_placeholder (format nil "true (* PRECD OF ~A([~A]) *)" (second formula) (rest (rest formula))))
              (@ (apply-predicate (rest formula)))
              (t (error "Formula ~S not understood. (car=~S)" formula (car formula)))))
      (t (error "Formula ~S not understood." formula)))))

(defun handle-function-definitions% (function-list body)
  (let ((first-function (apply #'handle-function-definition% (first function-list)))
        (rest-functions (mapcar (lambda (f) (apply #'handle-function-definition%% "with" f))
                                (rest function-list))))
    (format nil
            "~@<~4:I~{~A~^~:@_~}~:@_in~:@_~A~:>"
            (cons first-function rest-functions)
            (clir->mlw%indent body))))


(defun handle-function-definition% (&rest args)
  (apply #'handle-function-definition%% "let" args))

(defun handle-asserts% (args)
  (let ((formula (first args))
        (body (second args)))
    (format nil
            "~@<~{assert { ~A };~:@_~:}~A~:>"
            (mapcar #'clir-formula-to-string formula)
            (clir->mlw body))))

(defmacro with-assertions ((pre-post-list function-body) &body body)
  (let ((assertions (gensym)))
    `(let ((,assertions (get-assertions ,function-body)))
       (if (or (car ,assertions)
               (cdr ,assertions))
         (destructuring-bind ,pre-post-list ,assertions
           ,@body)
         (let ,pre-post-list
           ,@body))))
  )


(defun handle-function-definition%% (let-name function-name typed-lambda-list typed-result-list &rest body)
  (with-assertions ((pre post) body)
    (let ((let-name (if (string-equal let-name "let")
                        "let rec"
                        let-name)))
      (format nil
              (concatenate
               'string
               "~@<~4:I~A ~(~A~) ~A : ~A"
               "~@[~:@_requires { ~A }~]"
               "~@[~:@_~-2:Iensures  { ~A }~]"
               "~:@_~-4:I="
               "~:@_~A"
               "~:>"
               )
              let-name
              function-name
              (handle-typed-arguments typed-lambda-list)
              (handle-return-type typed-result-list)
              (clir-formula-to-string pre)
              (clir-formula-to-string post :tupled (cons '#:result ;; The name of the "result" function
                                                         typed-result-list))
              (clir->mlw%indent (first (drop-decls body)))))))


(defun handle-let-lhs (lhs)
  (if (> (length lhs) 1)
      ;; On multiple-values: use parentheses
      ;; {i.e.,    ( tuple values)   : (tuple type) }
      (format nil "(~:{~(~A~)~:^,~}) : (~:*~:{~*~(~A~)~:^, ~})" lhs)

      ;; On single value, trivial
      (format nil "~:{~(~A : ~A~)~}" lhs)))

(defun handle-case-pattern (pattern)
  (if (eq pattern 'default)
      "_"
      (typecase pattern
        (symbol pattern)
        (cons (case (car pattern)
                (the (string-capitalize (write-to-string (third pattern))))
                (t (error "Unknown pattern ~S" pattern))))))
  )

(defun handle-case-alternative (alt)
  (destructuring-bind (pattern body) alt
    (list (handle-case-pattern pattern)
          (clir->mlw%indent body))))

(defun handle-case% (discriminant &rest alternatives)
  (format nil
          "~@<match ~(~A~) with~
           ~:{~:@_  | ~A -> ~:@_~A~}~:@_~-4:Iend~:>"
          discriminant
          (mapcar #'handle-case-alternative alternatives)))

(defun is-infix (name)
  (member name
          '(< <= > >= = == != <> and or /\ \/ + - * /)))

(defun handle-infix% (name args)
  (format nil (format nil "~~{~~A~~^ ~(~A~) ~~}"
                      name)
          (mapcar #'clir->mlw args)))

(defun handle-replacement-funcall% (name args)
  (flet ((is-array-access (fname)
           (string-equal (symbol-name fname)
                         :get)))
    (cond
      ((is-array-access name)
       (format nil "~(~A[~A]~)"
               (first args)
               (second args)))
      (t nil))))

(defun handle-funcall% (name &rest args)
  (if (is-infix name)
      (handle-infix% name args)
      (or (handle-replacement-funcall% name args)
          (format nil
                  "~(~A~) ~{~A~^ ~}"
                  name
                  (mapcar #'clir->mlw args)))))


(defun handle-let% (lhs rhs body)
  (format nil
          "~@<let ~A = ~A in ~:@_~A~:>"
          (handle-let-lhs lhs)
          (clir->mlw rhs)
          (clir->mlw body)))

(defun handle-tuple% (elements)
  (format nil "~@<(~{~A~^, ~})~:>"
          (mapcar #'clir->mlw elements)))


(defun indent% (text)
  "Indents a text string.

We use `*indent-size*' times `*indent-level*' (although the
`*indent-level*' should probably be kept at one if you may be calling
`indent%' recursively.

Algorithm: We use two synthetic streams (`in' and `out'). We read
lines from `in' until EOF. For each line read, we first output as many
spaces as needed, and then the read line with the line terminator. The
streams are closed and the resulting string from the output stream is
returned as per `with-output-stream'."

  (labels ((indent-stream (in out)
             (let ((line (read-line in nil :eof)))
               (unless (eq line :eof)
                 (progn (print-spaces out)
                        (write-line line out)
                        (indent-stream in out)))))
           (print-spaces (stream)
             (let ((times (* *indent-level* *indent-size*)))
               (dotimes (i times)
                 (write-char #\  stream)))))
    (with-output-to-string (out)
      (with-input-from-string (in text)
        (indent-stream in out)))))

(defun clir->mlw%indent (body &key no-indent)
  "clir->mlw modulo indentation. It enables indentation, unless the
`NO-INDENT' paramter is set to exactly the next form of the body, or
unless it is exactly `T'."
  (if (or (eq no-indent
              t)
          (and (consp body)
               (eq (first body)
                   no-indent)))
      (clir->mlw body)
      (indent% (clir->mlw body))))

(defun clir->mlw (form)
  "Transforms a single CLIR FORM (toplevel or not) into its equivalent
WhyML representation."
  (typecase form
    (symbol (format nil "~(~A~)" form))
    (cons (case (car form)
            (define (apply #'handle-function-definition% (cdr form)))
            (letfun (apply #'handle-function-definitions% (cdr form)))
            (let (apply #'handle-let% (cdr form)))
            (case (apply #'handle-case% (cdr form)))
            (the (string-capitalize (format nil "~A" (third form))))
            (tuple (handle-tuple% (cdr form)))
            (asserts (handle-asserts% (cdr form)))
            (@ (apply #'handle-funcall% (cdr form)))
            (t (error "Unknown grammar node: ~S" (car form)))))))

(defun imports->mlw (import-list)
  (format nil "~{~&use import ~A~}~%"
          import-list))

(defun generate-module (verification-unit parsed-forms &key stream)
  "Generates a WhyML module from a VERIFICATION-UNIT and a list of
PARSED-FORMS, outputing the result in STREAM."
  (let ((theory-name (first verification-unit))
        (imports (imports->mlw (remove-if-not
                                #'identity
                                (mapcar
                                 #'find-import-in-theory-db
                                 (getf (cdr verification-unit) :uses))))))
    (format stream "module ~A ~&~A~&~{~A~%~}~&end~%"
            theory-name
            imports
            parsed-forms)))

(defun clir-file->mlw (form-list &key current-vu passed-forms stream)
  "Transforms a list of toplevel CLIR forms into its WhyML equivalent
file."
  (if form-list
      (let ((form (first form-list)))
        (case (car form)
          (verification-unit (clir-file->mlw (cdr form-list)
                                             :current-vu (cdr form)
                                             :passed-forms nil
                                             :stream stream))
          (define (clir-file->mlw (cdr form-list)
                                  :current-vu current-vu
                                  :passed-forms (cons (clir->mlw form) passed-forms)
                                  :stream stream))
          (t (error "Unknown element ~S (probably because it is on package ~S)" (car form) (symbol-package (car form))))))

      ;; No more stuff to process.
      (generate-module current-vu (nreverse passed-forms) :stream stream)))

(defun read-clir-file (pathspec)
  "Reads a CLIR file with symbols in the ir.mlw.formatter package, so
that it can be easily converted to a WhyML output via
`CLIR-FILE->MLW'."
  (let ((prev-package *package*))
    (unwind-protect
         (let ((content))
           (with-open-file (clir-stream pathspec)
             (setf content (loop for a = (read clir-stream nil)
                                 while a
                                 collect a)))
           content)
      (setf *package* prev-package))))


(defvar *clir-extension* ".clir")

(defmacro easy-file (basename &optional (extension *clir-extension*))
  "Returns the path to a file in ../test/basename.clir"
  (format nil "../test/~(~A~)~A" (symbol-name basename) extension))

(defmacro easy-mlw (basename)
  `(clir-file->mlw (read-clir-file (easy-file ,basename))))

