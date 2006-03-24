;;; sensors-alias.scm: sensors alias procedures
;;; authors: Balamurugan <bala@zresearch.com>

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

(define sensors-alias-list '(
			     ; (power . ("Power Supply"))
			     ; (security . ("Platform Chassis Intrusion" "Platform Security Violation"))
			     ))

(define (sensors-get-aliased-groups alias-name)
  (letrec ((get-aliased-groups 
	    (lambda (alias-list)
	      (if (null? alias-list)
		  '()
		  (if (string-ci=? alias-name (any->string (caar alias-list)))
		      (cdar alias-list)
		      (get-aliased-groups (cdr alias-list)))))))
    (get-aliased-groups sensors-alias-list)))

(define (sensors-alias? alias-name)
  (letrec ((compare-alias 
	    (lambda (alias-list)
	      (if (null? alias-list)
		  #f
		  (if (string-ci=? alias-name (any->string (caar alias-list)))
		      #t
		      (compare-alias (cdr alias-list)))))))
    (compare-alias sensors-alias-list)))

(define (sensors-alias-list-append! aliases)
  "append to alias list"
  (set! sensors-alias-list (append sensors-alias-list aliases)))

;; Alias all system groups to its equivalent lower case
(map
 (lambda (group)
   (sensors-alias-list-append! 
    (list (list (string->symbol (string-replace (string-downcase group) #\space #\_))
		group))))
 (fi-sensors-get-group-list))

