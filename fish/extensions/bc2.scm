;;; bc2.scm: BMC configurator
;;; authors: Balamurugan <bala.a@californiadigital.com>

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
;;; bc2.scm should be automatically loaded thru init.scm

(fi-load "bc-common.scm")
(fi-load "bc-user-section.scm")
(fi-load "bc-lan-serial-channel-section.scm")
(fi-load "bc-lan-conf-section.scm")
(fi-load "bc-lan-conf-auth-section.scm")
(fi-load "bc-lan-conf-misc-section.scm")
(fi-load "bc-serial-conf-section.scm")
(fi-load "bc-misc-section.scm")
(fi-load "bc-section.scm")

(define (validate-conf-file fd)
  (let ((section (read-section fd)))
    (if (null? section)
	#t
	(if (validate-section section)
	    (validate-conf-file fd)
	    #f))))

(define (commit-conf-file fd)
  (let ((section (read-section fd)))
    (if (null? section)
	#t
	(if (commit-section section)
	    (commit-conf-file fd)
	    #f))))

;;; main starts here ;;;

; (define fd (open-input-file "bc2.conf"))
; (if (validate-conf-file fd)
;     (begin 
;       (display "bc2.conf is validated\n")
;       (seek fd 0 SEEK_SET)
;       (if (commit-conf-file fd)
; 	  (display "bc2.conf is committed\n")
; 	  (display "unable to commit file bc2.conf\n")))
;     (display "fix the file and rerun\n"))
; (close fd)
