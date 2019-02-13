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

(defun make-periodic-frame ()
  (let* ((channel)
         (button-handle)
         (cmd-fr (make-instance 'ltk:frame :borderwidth 2 :relief :raised))
         (cmd-lbl (make-instance 'ltk:label :master cmd-fr :text "Periodic Input:"))
         (cmd-entry (make-instance 'ltk:entry :master cmd-fr
                                   :state :normal))
         (cmd-p-lbl (make-instance 'ltk:label :master cmd-fr :text "Period [s]:"))
         (cmd-p-entry (make-instance 'ltk:entry :master cmd-fr
                                     :state :normal))
         (cmd-button (make-instance 'ltk:button :master cmd-fr :text "Start"
                                      :command (lambda ()
                                                 (if channel
                                                     (progn
                                                       (stop channel)
                                                       (setf (ltk:text button-handle) "Start")
                                                       (setq channel nil))
                                                     (progn
                                                       (setq channel (make-periodic (ltk:text cmd-entry)
                                                                                    (ltk:text cmd-p-entry)))
                                                       (setf (ltk:text button-handle) "Stop")))))))
    (setq button-handle cmd-button)
    (ltk:pack cmd-lbl :side :left :padx 2)
    (ltk:pack cmd-entry :side :left :fill :x :padx 2 :expand t)
    (ltk:pack cmd-p-lbl :side :left :padx 2)
    (ltk:pack cmd-p-entry :side :left :padx 2 :expand t)
    (ltk:pack cmd-button :side :left :padx 2)
    cmd-fr))

