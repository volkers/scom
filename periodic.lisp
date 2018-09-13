;;;; periodic.lisp

#|
    Copyright (C) 2018 Volker Sarodnick

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#

(in-package #:scom)

(defclass periodic ()
  ((cmd :accessor cmd :initarg :cmd)
   (period :accessor period :initarg :period) ; [ms]
   (stopped :accessor stopped :initform nil))
  (:documentation "Periodic command"))

(defgeneric run-command (periodic)
  (:documentation "Run the command when not stopped."))

(defmethod run-command ((per periodic))
  (unless (stopped per)
    (convert-and-send (cmd per)) ; todo: use only send here instead
    (ltk:after (slot-value per 'period) (lambda () (run-command per)))))

(defgeneric stop (periodic)
  (:documentation "Stop the running periodic command"))

(defmethod stop ((per periodic))
    (setf (stopped per) t))

(defmethod initialize-instance :after ((per periodic) &key)
  (convert-and-send (cmd per))
  (ltk:after (slot-value per 'period) (lambda () (run-command per))))

(defun make-periodic (cmd period)
  (make-instance 'periodic :cmd cmd :period (* 1000 (parse-integer period))))
