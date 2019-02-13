;;;; scom.lisp

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


(ql:quickload 'bordeaux-threads)
(ql:quickload 'cl-ppcre)
(ql:quickload 'ltk)
(ql:quickload 'ltk-mw)
(ql:quickload 'uiop)

(defpackage #:scom
  (:use :common-lisp)
  (:export #:main))

(in-package #:scom)

(setq ltk:*wish-args* '("-name" "scom"))

(defvar *serial-stream* nil)
(defvar *outconsole-hex* nil)
(defvar *outconsole-ascii* nil)
(defvar *input-ascii-hex* nil)
(defparameter *s-lock* (bordeaux-threads:make-lock))

(load "help")
(load "settings")
(load "sending")
(load "periodic")

(defun quit ()
  (when *serial-stream*
    (close *serial-stream*)
    (setq *serial-stream* nil))
  (setf ltk:*exit-mainloop* t))

(defun about ()
  (ltk:do-msg "This is scom, a serial communication terminal for Linux.
Copyright (C) 2018 Volker Sarodnick
GNU General Public License"))

(defun console-writer ()
  (unless ltk:*exit-mainloop*
    (let ((add-nl nil))
      (and *outconsole-hex*
           *serial-stream*
           (loop :while (listen *serial-stream*)
                 :do (let ((c-byte (read-byte *serial-stream*)))
                       (setq add-nl t) ;; we got output
                       (ltk:append-text *outconsole-hex* (format nil " 0x~16r" c-byte))
                       (ltk:append-text *outconsole-ascii* (format nil "~a" (code-char c-byte))))))
      (when add-nl ;; add a newline after output
        (ltk:append-newline *outconsole-hex*)
        (ltk:append-newline *outconsole-ascii*)
        (ltk:see *outconsole-hex* "end") ;; always show the last line
        (ltk:see *outconsole-ascii* "end"))
      (ltk:after 100 'console-writer))))

(defun main ()
  (ltk:with-ltk ()
    (let* ((open-button-handle nil)
           (cmd-entry-handle nil)
           (menu-settings-handle nil)
           ;; device bar
           (bar (make-instance 'ltk:frame))
           (dev-list (get-dev-list))
           (dev-name (make-instance 'ltk:combobox :master bar :text (first dev-list) :values dev-list))
           (open-button (make-instance 'ltk:button :master bar :text "Open"
                                                   :command (lambda ()
                                                              (if *serial-stream*
                                                                  (progn
                                                                    ;; close
                                                                    (close *serial-stream*)
                                                                    (setq *serial-stream* nil)
                                                                    (ltk:configure cmd-entry-handle :state :disabled)
                                                                    (ltk:configure menu-settings-handle :state :normal)
                                                                    (setf (ltk:text open-button-handle) "Open"))
                                                                  (progn
                                                                    ;; open
                                                                    (setq *serial-stream* (open (ltk:text dev-name)
                                                                                                :direction :io
                                                                                                :if-exists :overwrite
                                                                                                :element-type :default))
                                                                    (ltk:configure cmd-entry-handle :state :normal)
                                                                    (ltk:configure menu-settings-handle :state :disabled)
                                                                    (setf (ltk:text open-button-handle) "Close"))))))
           ;; cmd frame
           (cmd-fr (make-instance 'ltk:frame :borderwidth 2 :relief :raised))
           (cmd-lbl (make-instance 'ltk:label :master cmd-fr :text "Input:"))
           (cmd-entry (make-instance 'ltk-mw:history-entry :master cmd-fr
                                                           :state :disabled
                                                           :command 'convert-and-send))
           (cmd-ascii-hex (make-instance 'ltk:combobox :master cmd-fr
                                                       :width 8
                                                       :text "Hex" :values '("LF" "CR" "CR/LF" "None" "Hex")))
           (cmd-send-button (make-instance 'ltk:button :master cmd-fr
                                                       :text "Send"
                                                       :command (lambda ()
                                                                  (convert-and-send (ltk:text cmd-entry)))))
           ;; periodic frame made in (make-periodic-frame)
           ;; out frame
           (out-f (make-instance 'ltk:frame :borderwidth 2 :relief :raised))
           (outu-f (make-instance 'ltk:frame :master out-f))
           (out-lbl (make-instance 'ltk:label :master outu-f :text "Output:"))
           (out-clear-button (make-instance 'ltk:button :master outu-f :text "Clear"
                                                        :command (lambda ()
                                                                   (setf (ltk:text *outconsole-hex*) "")
                                                                   (setf (ltk:text *outconsole-ascii*) ""))))
           (outc-f (make-instance 'ltk:frame :master out-f))
           (outconsole-hex (make-instance 'ltk:text :width 50 :master outc-f))
           (outconsole-ascii (make-instance 'ltk:text :width 50 :master outc-f))
           (outconsole-scrollbar-hex (ltk:make-scrollbar outc-f))
           (outconsole-scrollbar-ascii (ltk:make-scrollbar outc-f))
           ;; menubar
           (mb (ltk:make-menubar))
           (mfile (ltk:make-menu mb "File"))
           (mf-exit (ltk:make-menubutton mfile "Quit" 'quit
                                         :underline 0
                                         :accelerator "Alt q"))
           (msettings (ltk:make-menu mb "Settings"))
           (mf-general-settings (ltk:make-menubutton msettings "General Settings"
                                                     (lambda () (unless *serial-stream*
                                                                  (general-settings)))
                                                     :underline 0
                                                     :accelerator "Alt g"))
           (mf-settings (ltk:make-menubutton msettings "Port Settings"
                                             (lambda () (unless *serial-stream*
                                                          (settings (ltk:text dev-name))))
                                             :underline 0
                                             :accelerator "Alt s"))
           (mhelp (ltk:make-menu mb "Help"))
           (mh-help (ltk:make-menubutton mhelp "Help"
                                         'help
                                         :underline 0
                                         :accelerator "Alt h"))
           (mh-about (ltk:make-menubutton mhelp "About"
                                          'about
                                          :underline 0)))
      (declare (ignore mf-exit mf-general-settings mh-help mh-about))
      (setq open-button-handle open-button)
      (setq cmd-entry-handle cmd-entry)
      (setq menu-settings-handle mf-settings)
      (ltk:wm-title ltk:*tk* "scom")
      (ltk:on-close ltk:*tk* 'quit)
      (ltk:bind ltk:*tk* "<Alt-q>" (lambda (event) (declare (ignore event)) (quit)))
      (ltk:bind ltk:*tk* "<Alt-g>" (lambda (event) (declare (ignore event)) (unless *serial-stream*
                                                                              (general-settings))))
      (ltk:bind ltk:*tk* "<Alt-s>" (lambda (event) (declare (ignore event)) (unless *serial-stream*
                                                                              (settings (ltk:text dev-name)))))
      (ltk:bind ltk:*tk* "<Alt-h>" (lambda (event) (declare (ignore event)) (help)))
      (ltk:pack bar :side :top :anchor :w)
      (ltk:pack open-button :side :left)
      (ltk:pack dev-name :side :left)
      ;; update device list when mouse is entering the combo-box field
      ;; problem: it's not updated when pressing, so you need to move the mouse into the field to get the new vals
      (ltk:bind dev-name "<Enter>" (lambda (evt)
                                     (declare (ignore evt))
                                     (setf (ltk:options dev-name) (get-dev-list))))

      (ltk:pack cmd-fr :fill :x :side :top)
      (ltk:pack cmd-lbl :side :left :padx 2)
      (ltk:pack cmd-entry :side :left :fill :x :padx 2 :expand t)
      (ltk:pack cmd-ascii-hex :side :left :padx 2)
      (setq *input-ascii-hex* (ltk:text cmd-ascii-hex))
      (ltk:bind cmd-ascii-hex "<<ComboboxSelected>>" (lambda (event)
                                                       (declare (ignore event))
                                                       (setq *input-ascii-hex* (ltk:text cmd-ascii-hex))))
      (ltk:pack cmd-send-button :side :left :padx 2)
      (ltk:pack (make-periodic-frame) :fill :x :side :top)
      (ltk:pack out-f :fill :both :expand t)
      (ltk:pack outu-f :fill :x :side :top :anchor :w)
      (ltk:pack out-lbl :side :left :padx 2)
      (ltk:pack out-clear-button :side :left :padx 2)
      (ltk:pack outc-f :fill :both :side :bottom :expand t)
      (ltk:grid outconsole-hex 0 0 :sticky "nsew")
      (ltk:grid outconsole-scrollbar-hex 0 1 :sticky "ns")
      (ltk:grid outconsole-ascii 0 2 :sticky "nsew")
      (ltk:grid outconsole-scrollbar-ascii 0 3 :sticky "ns")
      (ltk:grid-columnconfigure outc-f 0 :weight 1)
      (ltk:grid-columnconfigure outc-f 1 :weight 0)
      (ltk:grid-columnconfigure outc-f 2 :weight 1)
      (ltk:grid-columnconfigure outc-f 3 :weight 0)
      (ltk:grid-rowconfigure outc-f 0 :weight 1)
      (setq *outconsole-hex* outconsole-hex)
      (setq *outconsole-ascii* outconsole-ascii)
      (ltk:configure outconsole-scrollbar-hex "command" (concatenate 'string (ltk:widget-path outconsole-hex) " yview"))
      (ltk:configure outconsole-hex "yscrollcommand" (concatenate 'string (ltk:widget-path outconsole-scrollbar-hex) " set"))
      (ltk:configure outconsole-scrollbar-ascii "command" (concatenate 'string (ltk:widget-path outconsole-ascii) " yview"))
      (ltk:configure outconsole-ascii "yscrollcommand" (concatenate 'string (ltk:widget-path outconsole-scrollbar-ascii) " set"))
      (console-writer))))
