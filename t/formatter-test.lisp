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

(defpackage :ir.mlw.test
  (:use :cl
        :prove
        :ir.mlw.formatter
        :cl-ppcre))
(in-package :ir.mlw.test)

(defun replace-all-whitespace (s)
  "Removes all whitespace characters from s. Needs CL-PPCRE."
  (regex-replace
   "\\s+$"
   (regex-replace
    "^\\s+"
    (regex-replace-all "\\s+" s " ")
    "")
   ""))

(defun equal-ignore-whitespace (str1 str2)
  (let ((tstr1 (replace-all-whitespace str1))
        (tstr2 (replace-all-whitespace str2)))
    (string-equal tstr1 tstr2)))

(subtest "The ignoring-whitespace comparing functions used through this test work as intended."
  (plan 4)
  (ok (equal-ignore-whitespace " a  a " "a a")
      "Ignore spaces before and after the text, as well two spaces to one")
  (ok (equal-ignore-whitespace " a a
" "a a")
      "Ignore newlines in the end")
  (ok (not (equal-ignore-whitespace " a  a " "aa"))
      "Do not treat the same two symbols with NO space between them")
  (ok (not (equal-ignore-whitespace "a" "b"))
      "Treat different symbols differently")
  (finalize)
)

(defun test/letfun-1 ()
  (clir->mlw '(letfun ((f ((a (array int))) ((b int))
                        (declare (assertion
                                  (precd true)
                                  (postcd (@ = c b))))
                        (let ((b1 bool)) (@ > b (the int 0))
                             (case b1
                               ((the bool true) b)
                               ((the bool false) (@ + b (the int 0)))))))
               (the bool true))))

(plan 11)

(is (test/letfun-1) "let rec f (a: (array int)) : int
    requires { true }
    ensures  { (c) = (b) }
  =
    let b1 : bool = b > 0 in
match b1 with
     | True -> b
     | False -> b + 0
    end
    in
    true" "A letfun should be properly parsed" :test #'equal-ignore-whitespace)


(defun test/handle-fun-def-1 ()
  (clir->mlw '(define f ((a (array int)) (b int)) ((c int))
               (declare (assertion
                         (precd true)
                         (postcd (@ = c b))))
               (let ((b1 bool)) (@ > b (the int 0))
                    (case b1
                      ((the bool true) b)
                      ((the bool false) (@ + b (the int 0))))))))

(is (test/handle-fun-def-1) "let rec f (a: (array int)) (b: int) : int
    requires { true }
    ensures  { (c) = (b) }
  =
    let b1 : bool = b > 0 in
      match b1 with
       | True -> b
     | False -> b + 0
    end" "A toplevel function definition should be properly transformed into a let rec construct" :test #'equal-ignore-whitespace)

(defun test/let-1 ()
  (clir->mlw '(let ((a int)) (the int 1)
               (the int 2))))

(is (test/let-1)
    "let a : int = 1 in 2"
    "A simple let is properly transformed"
    :test #'equal-ignore-whitespace)

(defun test/case-1 ()
  (clir->mlw '(case b
               ((the int 0) (the int 0))
               ((the int 1) (the int 1))
               (default b))))

(is (test/case-1) "match b with | 0 -> 0 | 1 -> 1 | _ -> b end"
    "A case is transformed into a match. Handles default cases"
    :test #'equal-ignore-whitespace)

(defun test/let&case-1 ()
  (clir->mlw '(let ((a int)) (the bool true)
               (case a
                 ((the bool true) (the int 4))
                 ((the bool false) (the int 5))))))

(is (test/let&case-1)
    "let a : int = True in
    match a with
     | True -> 4
     | False -> 5
    end"
    "A let with a case is transformed into a let with a match"
    :test #'equal-ignore-whitespace)

(defun test/constructor/true ()
  (clir->mlw '(the bool true)))

(is (test/constructor/true) "True")

(subtest "Constructors work in a case expression"
  (labels ((test/constructor/case/true ()
              (clir->mlw '(case b
                           ((the bool true) (the bool true))
                           ((the bool false) (the bool false))))))
    (ok (not (ppcre:scan "true -> true"
                         (test/constructor/case/true)))
        "We do not see any lowercase constructors in the case")
    (like (test/constructor/case/true)
          "True -> True"
          "We see a proper capitalized constructor in a two-fold case")
    (like (test/constructor/case/true)
          "False -> False"
          "We see a proper capitalized constructor in a two-fold case")))


(is (clir->mlw '(@ > b (the int 1)))
    "b > 1"
    "A binary infix function is properly transformed")

(is (clir->mlw '(@ > a b c d))
    "a > b > c > d"
    "A binary infix function with multiple parameters is also properly handled")

(is (clir->mlw '(define f ((a int)) ((b int))
                 (declare
                  (assertion
                   (postcd (@ = b a))))
                 a))
    "let rec f (a: int) : int
    ensures { (result) = (a) }
    =
      a"
    "Postconditions use the result keyword in the MLW result, replacing symbols in the original formula"
    :test #'equal-ignore-whitespace)


(subtest "Tuples"
  (is (clir->mlw '(define f ((a int)) ((r1 int) (r2 int))
                   (tuple a a)))
      "let rec f (a: int) : (int,int)
    =
      (a, a)"
      "Tuples are handled at a toplevel function definition"
      :test #'equal-ignore-whitespace)

  (is (clir->mlw '(define f ((a int)) ((r1 int) (r2 int))
                   (letfun ((g ((ga1 int)) ((gr1 int) (gr2 int))
                              (tuple a a)))
                     (let ((b int) (c int)) (@ g a)
                          (tuple b c)))))
      "let rec f (a: int) : (int,int)
  =
      let rec g (ga1: int) : (int,int)
  =
    (a, a)
  in
  let (b,c) : (int, int) = g a in
(b, c)" "Tuples are handled at a let left-hand side" :test #'equal-ignore-whitespace)

  (is
   (clir->mlw '(define f ((a int)) ((r1 int) (r2 int))
                (declare
                 (assertion
                  (precd (@ = a (the int 1)))
                  (postcd (@ = r1 r2))))
                (tuple a a)))
   "let rec f (a: int) : (int,int)
    requires { (a) = (1) }
    ensures  { let (r1,r2) : (int, int) = result in (r1) = (r2) }
  =
    (a, a)" :test #'equal-ignore-whitespace))

(finalize)
