;;; sensors.scm: this initialization file is automatically loaded by default
;;; author: Anand Babu <ab@gnu.org.in>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or (at
;;; your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
;;; 
;;; /usr/share/fish/extensions/init.scm will be automatically
;;; loaded during the startup. this file inturn loads the standard
;;; fish extensions and also does other external customizations.
;;; To overload this init.scm file, place your own init.scm in ur
;;; ~/.fish/extensions folder.

;; utility procedures used by sensors-conf.scm

(define sensors-group-alias-list '())
(define sensors-ignored-list '())

(define (sensors-ignore! ignored-list)
  "ignore this list of sensors"
  (set! sensors-ignored-list (append sensors-ignored-list ignored-list)))

(define (sensors-ignored? sensor-id)
  "check if this SENSOR-ID is ignored"
  (list? (member sensor-id sensors-ignored-list)))

(define (sensors-group-alias-list-append! aliases)
  "append to alias list"
  (set! sensors-group-alias-list (append sensors-group-alias-list aliases)))

;; Alias all system groups to its equivalent lower case
(map
 (lambda (group)
   (sensors-group-alias-list-append! 
    (list (list (string->symbol (string-replace (string-downcase group) #\space #\_))
		group))))
 (fi-sensors-get-group-list))

