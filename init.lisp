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

(cl:require "asdf")
(cl:in-package :cl-user)
(defpackage :ir.mlw.load
  (:use :cl :asdf))
(cl:in-package :ir.mlw.load)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :ql)
    (error "ERROR: We need QuickLisp already installed ~
on your lisp environment to work.")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :qlot))

(load "clir2mlw.asd")

;; Check that dependencies are installed
(qlot:install 'clir2mlw)
(qlot:quickload 'clir2mlw)

;;; When testing
(load "clir2mlw-test.asd")
(asdf:test-system 'clir2mlw-test)



