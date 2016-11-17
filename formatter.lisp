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
  (:use :cl)

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
  (:export :define :lettype :letvar :letconst :let :let* :letfun :case :default "@" "@@")
  ;;; End IR keywords


  
  (:export #:clir->mlw #:clir-file->mlw #:generate-module #:*clir-extension*)
  (:export #:read-mlw-file))
(cl:in-package :ir.mlw.formatter)

(defun get-assertions (function-body)
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

(defun clir-formula-to-string (formula)
  (labels ((is-infix (op)
             (and (symbolp op)
                  (member op
                          '(+ - * / < <= >= = > % <>)
                          :test (lambda (a b) (string-equal (symbol-name a) (symbol-name b))))))
           (clir-term-to-string (term &optional recursive)
             (typecase term
               (number term)
               (symbol term)
               (string term)
               (cons (case (first term)
                       (quote (second term))
                       (the (third term))
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
                            (format nil "~A[~A]" (second p) (third p))
                            (format nil "~A ~{~A~^ ~}" (first p) (mapcar (lambda (x) (clir-term-to-string x t)) (rest p)))))))
               (if recursive
                   (format nil "(~A)" predicate-str)
                   predicate-str))))
    (typecase formula
      (symbol formula)
      (string formula)
      (number formula)
      (cons (case (car formula)
              (:forall (format nil "forall ~:{~A:~A~:^,~}. ~A" (second formula) (if (third formula)
                                                                                    (clir-formula-to-string (third formula))
                                                                                    "")))
              (-> (format nil "~{(~A)~^ -> ~}" (mapcar #'clir-formula-to-string (rest formula))))
              (and (format nil "(~{~A~^ /\\ ~})" (mapcar #'clir-formula-to-string (rest formula))))
              (or (format nil "~{~A~^ \\/ ~}" (mapcar #'clir-formula-to-string (rest formula))))
              (:postcd_placeholder (format nil "true (* POSTCD OF ~A([~A]) *)" (second formula) (rest formula)))
              (:precd_placeholder (format nil "true (* PRECD OF ~A([~A]) *)" (second formula) (rest (rest formula))))
              (@ (apply-predicate (rest formula)))
              (t (error "Formula ~S not understood. (car=~S)" formula (car formula)))))
      (t (error "Formula ~S not understood." formula)))))

(defun handle-function-definitions% (function-list body)
  (let ((first-function (apply #'handle-function-definition% (first function-list)))
        (rest-functions (mapcar (lambda (f) (apply #'handle-function-definition%% "and" f))
                                (rest function-list))))
    (format nil
            "~@<~4:I~{~(~A~)~^~:@_~}~:@_in~:@_~A~:>"
            (cons first-function rest-functions)
            (clir->mlw body))))


(defun handle-function-definition% (&rest args)
  (apply #'handle-function-definition%% "let" args))

(defmacro with-assertions ((pre-post-list function-body) &body body)
  (let ((assertions (gensym)))
    `(let ((,assertions (get-assertions ,function-body)))
       (if (car ,assertions)
         (destructuring-bind ,pre-post-list ,assertions
           ,@body)
         (let ,pre-post-list
           ,@body))))
  )


(defun handle-function-definition%% (let-name function-name typed-lambda-list typed-result-list &rest body)
  (with-assertions ((pre post) body)
    (let ((recursive? "rec"))
      (format nil
              (concatenate
               'string
               "~@<~4:I~A ~A ~A ~A : ~A"
               "~@[~:@_requires { ~A }~]"
               "~@[~:@_~-2:Iensures  { ~A }~]"
               "~:@_~2:I="
               "~:@_~A"
               "~:>"
               )
              let-name
              recursive?
              function-name
              (handle-typed-arguments typed-lambda-list)
              (handle-return-type typed-result-list)
              (clir-formula-to-string pre)
              (clir-formula-to-string post)
              (clir->mlw (first (drop-decls body)))))))


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
          (clir->mlw body))))

(defun handle-case% (discriminant &rest alternatives)
  (format nil
          "~@<~4:Imatch ~(~A~) with~
           ~:{~:@_ | ~A -> ~A~}~:@_~-4:Iend~:>"
          discriminant
          (mapcar #'handle-case-alternative alternatives)))

(defun is-infix (name)
  (member name
          '(< <= > >= = == != <> and or /\ \/ + - * /)))

(defun handle-infix% (name args)
  (format nil (format nil "~~{~~(~~A~~)~~^ ~(~A~) ~~}"
                      name)
          (mapcar #'clir->mlw args)
          ))

(defun handle-funcall% (name &rest args)
  (if (is-infix name)
      (handle-infix% name args)
      (format nil
              "~(~A~) ~{~(~A~)~^ ~}"
              name
              (mapcar #'clir->mlw args))))


(defun handle-let% (lhs rhs body)
  (format nil
          "~@<let ~A = ~A in ~:@_~A~:>"
          (handle-let-lhs lhs)
          (clir->mlw rhs)
          (clir->mlw body)))
(defun handle-tuple% (elements)
  (declare (ignorable elements)))


(defun clir->mlw (form)
  (typecase form
    (symbol (format nil "~(~A~)" form))
    (cons (case (car form)
            (define (apply #'handle-function-definition% (cdr form)))
            (letfun (apply #'handle-function-definitions% (cdr form)))
            (let (apply #'handle-let% (cdr form)))
            (case (apply #'handle-case% (cdr form)))
            (the (third form))
            (tuple (handle-tuple% (cdr form)))
            (@ (apply #'handle-funcall% (cdr form)))))))

(defun imports->mlw (import-list)
  (format nil "~{~&use import ~A~}~%"
          import-list))

(defun generate-module (verification-unit parsed-forms &key stream)
  (let ((theory-name (first verification-unit))
        (imports (imports->mlw '("int.Int"
                                 "ref.Ref"
                                 "array.Array"
                                 "array.IntArraySorted"
                                 "array.ArraySwap"
                                 "array.ArrayPermut"
                                 "array.ArrayEq"))))
    (format stream "module ~A ~&~A~&~{~A~%~}~&end~%"
            theory-name
            imports
            parsed-forms)))

(defun clir-file->mlw (form-list &key current-vu passed-forms stream)
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
      (generate-module current-vu passed-forms :stream stream)))

(defun read-mlw-file (pathspec)
  (let ((content))
    (with-open-file (clir-stream pathspec)
      (setf content (loop for a = (read clir-stream nil)
                       while a
                       collect a)))
    content))


(defvar *clir-extension* ".clir")

(defmacro easy-file (basename &optional (extension *clir-extension*))
  "Returns the path to a file in ../test/basename.clir"
  (format nil "../test/~(~A~)~A" (symbol-name basename) extension))

(defmacro easy-mlw (basename)
  `(clir-file->mlw (read-mlw-file (easy-file ,basename))))

