;;;; settings.lisp

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

(defclass sdevice ()
  ((name :accessor name :initarg :name)
   (baudrate :accessor baudrate :initarg :baudrate)
   (baudrate-vals :reader baudrate-vals :initform '("1200" "4800" "9600" "19200" "38400" "57600" "115200" "Custom"))
   (flow-control :accessor flow-control :initarg :flow-control)
   (flow-control-vals :reader flow-control-vals :initform '("On" "Off"))
   (data-bits :accessor data-bits :initarg :data-bits)
   (data-bits-vals :reader data-bits-vals :initform '("5" "6" "7" "8"))
   (parity :accessor parity :initarg :parity)
   (parity-vals :reader parity-vals :initform '("None" "Even" "Odd"))
   (stop-bits :accessor stop-bits :initarg :stop-bits)
   (stop-bits-vals :reader stop-bits-vals :initform '("1" "2")))
  (:documentation "Settings of serial device"))

(defun get-dev-list ()
  "Return a list of serial device files, e.g. (\"/dev/ttyS0\" ... \"/dev/ttyUSB0\")."
  ;; there seems to be a problem with using wildcards in uiop:run-program
  ;; therefore ppcre is used to filter instead
  ;; maybe this could be implemented much simpler...
  (mapcar #'(lambda (devs) (concatenate 'string "/dev/" devs))
          (cl-ppcre:all-matches-as-strings "(ttyS.+)\|(ttyUSB.+)"
                                           (uiop:run-program (list "ls" "/dev")
                                                             :output :string))))

(defun make-sdevice (name)
  (let* ((s-string (uiop:run-program (list "stty" "-a" "-F" name) :output :string))
         (baudrate (first (cl-ppcre:all-matches-as-strings "\\d+(?= baud)" s-string)))
         (flow-control (if (search "-ixon" s-string)
                           "Off"
                           "On"))
         (data-bits (first (cl-ppcre:all-matches-as-strings "(?<= cs)\\d" s-string)))
         (parity (cond ((search "-parenb" s-string) "None")
                       ((search "-parodd" s-string) "Even")
                       (t "Odd")))
         (stop-bits (if (search "-cstopb" s-string)
                        "1"
                        "2")))
    ;; (format t "~a~%" s-string)
    (make-instance 'sdevice :name name
                            :baudrate baudrate
                            :flow-control flow-control
                            :data-bits data-bits
                            :parity parity
                            :stop-bits stop-bits)))

(defmethod print-object ((sd sdevice) stream)
  (format stream "#<~s name: ~a baudrate: ~a flow-control: ~a data-bits: ~a parity: ~a stop-bits: ~a>"
          (type-of sd)
          (if (slot-boundp sd 'name)
              (name sd)
              "(no name)")
          (if (slot-boundp sd 'baudrate)
              (baudrate sd)
              "(no baudrate)")
          (if (slot-boundp sd 'flow-control)
              (flow-control sd)
              "(no flow-control)")
          (if (slot-boundp sd 'data-bits)
              (data-bits sd)
              "(no data-bits)")
          (if (slot-boundp sd 'parity)
              (parity sd)
              "(undefined parity)")
          (if (slot-boundp sd 'stop-bits)
              (stop-bits sd)
              "(undefined number of stop-bits)")))

(defgeneric write-to-device (sdevice)
  (:documentation "Writes the settings back to the device."))

(defmethod write-to-device ((sd sdevice))
  ;; set some sensible defaults:
  (let ((std-options (list "raw" "-brkint" "igncr" "-imaxbel" "-echo" "min" "0" "time" "1"))
        (speed (baudrate sd))
        (ixon (if (string= "Off" (flow-control sd))
                  "-ixon"
                  "ixon"))
        (csn (concatenate 'string "cs" (data-bits sd)))
        (parenb (if (string= "None" (parity sd))
                    "-parenb"
                    "parenb"))
        (parodd (if (string= "Even" (parity sd))
                    "-parodd"
                    "parodd"))
        (cstopb (if (string= "1" (stop-bits sd))
                    "-cstopb"
                    "cstopb")))
    (uiop:run-program (append (list "stty" "-F" (name sd)) std-options (list speed ixon csn parenb parodd cstopb)))))

(defun apply-changes (sdev baud-cb flow-cb data-bits-cb parity-cb stop-bits-cb)
  (setf (baudrate sdev) (ltk:text baud-cb))
  (setf (flow-control sdev) (ltk:text flow-cb))
  (setf (data-bits sdev) (ltk:text data-bits-cb))
  (setf (parity sdev) (ltk:text parity-cb))
  (setf (stop-bits sdev) (ltk:text stop-bits-cb))
  (write-to-device sdev))


(defun settings (dev-name)
  (ltk:with-modal-toplevel (tl :title dev-name)
    (let* ((sdev (make-sdevice dev-name))
           (f (make-instance 'ltk:frame :master tl))
           (baud-label (make-instance 'ltk:label :master f :text "Baudrate"))
           (baud-combo (make-instance 'ltk:combobox :master f :text (baudrate sdev) :values (baudrate-vals sdev)))
           (flow-label (make-instance 'ltk:label :master f :text "Flow Control"))
           (flow-combo (make-instance 'ltk:combobox :master f :text (flow-control sdev) :values (flow-control-vals sdev)))
           (data-bits-label (make-instance 'ltk:label :master f :text "Data Bits"))
           (data-bits-combo (make-instance 'ltk:combobox :master f :text (data-bits sdev) :values (data-bits-vals sdev)))
           (parity-label (make-instance 'ltk:label :master f :text "Parity"))
           (parity-combo (make-instance 'ltk:combobox :master f :text (parity sdev) :values (parity-vals sdev)))
           (stop-bits-label (make-instance 'ltk:label :master f :text "Stop bits"))
           (stop-bits-combo (make-instance 'ltk:combobox :master f :text (stop-bits sdev) :values (stop-bits-vals sdev)))
           (cancel-b (make-instance 'ltk:button
                                    :master tl
                                    :text "Cancel"
                                    :command (lambda () (return))))
           (apply-b (make-instance 'ltk:button
                                    :master tl
                                    :text "Apply"
                                    :command (lambda ()
                                               (apply-changes sdev baud-combo flow-combo data-bits-combo parity-combo stop-bits-combo)
                                               (return)))))
      (ltk:pack f)
      (ltk:grid baud-label 0 0 :sticky "w")
      (ltk:grid baud-combo 0 1 :sticky "w")
      (ltk:grid flow-label 1 0 :sticky "w")
      (ltk:grid flow-combo 1 1 :sticky "w")
      (ltk:grid data-bits-label 2 0 :sticky "w")
      (ltk:grid data-bits-combo 2 1 :sticky "w")
      (ltk:grid parity-label 3 0 :sticky "w")
      (ltk:grid parity-combo 3 1 :sticky "w")
      (ltk:grid stop-bits-label 4 0 :sticky "w")
      (ltk:grid stop-bits-combo 4 1 :sticky "w")
      (ltk:pack apply-b :side :right :padx 2)
      (ltk:pack cancel-b :side :right :padx 2))))


(defun general-settings ()
  (ltk:with-modal-toplevel (tl :title "General Settings")
    (let* ((f (make-instance 'ltk:frame :master tl))
           (pnum-label (make-instance 'ltk:label :master f :text "Number of Periodic (restart needed)"))
           (pnum-entry (make-instance 'ltk:entry :master f :text "1"))
           (cancel-b (make-instance 'ltk:button
                                    :master tl
                                    :text "Cancel"
                                    :command (lambda () (return))))
           (apply-b (make-instance 'ltk:button
                                   :master tl
                                   :text "Apply (Not working yet)"
                                   :command (lambda () (return)))))
      (ltk:pack f)
      (ltk:grid pnum-label 0 0 :sticky "w")
      (ltk:grid pnum-entry 0 1 :sticky "w")
      (ltk:pack apply-b :side :right :padx 2)
      (ltk:pack cancel-b :side :right :padx 2))))
