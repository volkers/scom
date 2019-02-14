;;;; sending.lisp

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

(defun hex-str-to-byte-list (str)
  "Convert a string of digits to the corresponding list of bytes coding the pairs of numbers in the string.
E.g. \"0a41\" -> (list #x0a #x41)."
  (cond
    ((zerop (length str)) nil)
    (t (let ((i (parse-integer str :end 2 :radix 16)))
         (cons i
               (hex-str-to-byte-list (subseq str 2)))))))

(defun hex-to-bytes (hex-string)
  "Convert hexadecimal numbers to a list of bytes.
Spaces and '0x' tokens are removed, absent leading 0's in 0x\d are inserted.
Ex.: \"0xa 0x41 7d\" -> \"0a417d\" -> (list #xa #x41 #x7d})"
  (let* ((normalized-0 (cl-ppcre:regex-replace-all " (?=\[\\dabcdefABCDEF\]\\b)" hex-string "0")) ; 2 -> 02
         (normalized-0x (cl-ppcre:regex-replace-all "0x(?=\[\\dabcdefABCDEF\]\\b)" normalized-0 "0")) ; 0x2 -> 02
         (without-0x (cl-ppcre:regex-replace-all "0x" normalized-0x ""))
         (without-spaces (remove #\space without-0x)))
    (if (evenp (length without-spaces))
        (hex-str-to-byte-list without-spaces)
        (and
         (ltk:do-msg "Number of digits odd, can't be decoded to bytes. Check your input! Message dropped.")
         nil))))

(defun send-b-list (b-list)
  "Send byte list."
  (and
   b-list ; no idea to take the lock if b-list is empty
   (bordeaux-threads:with-lock-held (*s-lock*)
     (when *serial-stream*
       (loop for b in b-list do (write-byte b *serial-stream*))
       (force-output *serial-stream*)))))

(defun send-ascii (ascii-string)
  "Send ascii-string."
  (bordeaux-threads:with-lock-held (*s-lock*)
    (format *serial-stream* "~a" ascii-string)
    (force-output *serial-stream*)))

(defun convert-and-send (txt)
  "Convert text to byte-list and send it."
  (cond
    ((string= *input-ascii-hex* "Hex") (send-b-list (hex-to-bytes txt)))
    ((string= *input-ascii-hex* "None") (send-ascii txt))
    ((string= *input-ascii-hex* "LF") (send-ascii (format nil "~a~c" txt #\linefeed)))
    ((string= *input-ascii-hex* "CR") (send-ascii (format nil "~a~c" txt #\return)))
    ((string= *input-ascii-hex* "CR/LF") (send-ascii (format nil "~a~c~c" txt #\return #\linefeed)))
    (t (error "Not supported input type or line ending"))))

