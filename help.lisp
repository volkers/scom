;;;; help.lisp

#|
    Copyright (C) 2019 Volker Sarodnick

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

(defun help ()
  "Help window."
  (ltk:with-ltk ()
    (let* ((f (make-instance 'ltk:frame))
           (helpout (make-instance 'ltk:text :borderwidth 2 :relief :raised :master f))
           (quit-button (make-instance 'ltk:button
                                       :master f
                                       :text "Exit help"
                                       :command (lambda () (setf ltk:*exit-mainloop* t))))
           (helptext (format nil "SCOM serial communication terminal

Keybindings in the main window:
Alt-q: quit
Alt-h: help
Alt-s: settings of the currently chosen port (port must be closed)

Input type:
Hex: hexadecimal bytes (e.g. \"12ab\" or \"0x12 0xab\")
None: ASCII
LF: ASCII Linefeed added
CR: ASCII Carriage return added
CR/LF: ASCII Carriage return and Linefeed added

Use up/down keys for in the input field for accessing the history.")))
      (ltk:wm-title ltk:*tk* "Scom help")
      (ltk:pack f)
      (ltk:pack helpout)
      (ltk:pack quit-button :pady 2)
      (ltk:append-text helpout helptext)
      (ltk:configure helpout :state :disabled))))


