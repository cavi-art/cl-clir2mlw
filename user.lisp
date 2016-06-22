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

(defpackage :ir.mlw.user
  (:use :cl :ir.mlw.formatter)
  (:export #:clir-stream->mlw #:clir-batch->multifile-mlw #:clir-batch->mlw)
  (:export #:*output-stream* #:*output-file-name-extension*)
  (:export #:*dump-binary*))
(in-package :ir.mlw.user)

(defvar *output-stream*)
(defvar *auto-file-name* nil)
(defvar *output-file-name-extension* "mlw")

(defun auto-file-name-for (path)
  (let* ((name (pathname-name path))
         (basename (subseq name 0 (find #\. name :from-end t))))
    (merge-pathnames
     (make-pathname :directory (pathname-directory path) :type :unspecific)
     (make-pathname :name (concatenate 'string basename (concatenate 'string "." *output-file-name-extension*)) :type :unspecific))))


(defun clir-stream->mlw (clir-stream &key (stream *standard-output*))
  (clir-file->mlw (loop for a = (read clir-stream nil)
                     while a
                     collect a)
                  :stream stream))

(defun clir-batch->multifile-mlw (file-list &key (auto-file-name t))
  "If auto-file-name is nil, file-list must be an evenly-numbered
  list, whose odd elements are clir files and the even elements are
  the paths to the wanted mlw files."
  (if auto-file-name
      (clir-batch->multifile-mlw (loop for file in file-list
                                    collecting file
                                    collecting (auto-file-name-for file))
                                 :auto-file-name nil)

      (destructuring-bind (input-file output-file . rest) file-list
        (format *error-output* "Processing ~A and saving in ~A ~%" input-file output-file)
        (with-open-file (*output-stream* output-file
                                         :direction :output
                                         :element-type :default
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
          (clir-file->mlw (read-mlw-file input-file) :stream *output-stream*))
        (when rest
          (clir-batch->multifile-mlw rest :auto-file-name nil)))))

(defun print-comment (format-string &rest args)
  (format *output-stream* "~&(******************************************************)~%")
  (format *output-stream* "~&(* ~50@<~?~> *)~%" format-string args)
  (format *output-stream* "~&(******************************************************)~%"))

(defun clir-batch->mlw (file-list)
  (dolist (input-file file-list)
    (print-comment "BEGIN FILE ~A" input-file)
    (clir-file->mlw (read-mlw-file input-file) :stream *output-stream*)
    (print-comment "END FILE ~A" input-file)
    (format *output-stream* "~%~%")))





