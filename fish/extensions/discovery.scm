;;; discover.scm: discover list of IPMI compatible systems
;;; author: M.P.Anand Babu <ab@gnu.org.in>

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
;;; discover.scm should be automatically loaded thru init.scm

(define (fi-ipaddr-range start-ip end-ip)
  "returns list contains ip addresses between start-ip and end-ip"
  (let
      ((start-ip (inet-aton start-ip))
       (end-ip (inet-aton end-ip)))
    (if (<= start-ip end-ip)
	(begin
	  (append (list (inet-ntoa start-ip))
		  (fi-ipaddr-range (inet-ntoa (+ start-ip 1))
			    (inet-ntoa end-ip))))
	'())))

(define fi-discovery-cache '())
(define (fi-discover! start-ip end-ip)
  "discovers list of IPMI compatible systems and caches them"
  ;flush the cache
  (set! fi-discovery-cache '())
  (map
   (lambda (ipaddr)
     (if (eq? (fi-ping ipaddr) #t)
	 (begin
	   (set! fi-discovery-cache
		 (append fi-discovery-cache (list ipaddr)))
	   (display (string-append ipaddr " IPMIv1.5"))
	   (newline))
	 (begin
	   (display (string-append ipaddr " No response"))
	   (newline))))
   (fi-ipaddr-range start-ip end-ip)))

(define (discover args)
  "discovers list of IPMI compatible systems and caches them"
  (set! args (list->strlist args))
  (if (= (length args) 2)
      (fi-discover! (car args) (cadr args))
      (display "discover: wrong number of arguments, type \"help discover\" for more info\n")))

(fi-register-command! 
 '("discover" 
   "discover IPADDR-START IPADDR-END \n\t- discover IPMI compatible systems"))


(define (who args)
  "discovers list of IPMI compatible systems and caches them"
  (display fi-discovery-cache)
  (newline))

(fi-register-command! 
 '("who" 
   "who\n\t- dislay list of IPMI compatible systems discovered"))

(define (set-sock-timeout! args)
  "set socket timeout value in milli seconds"
  (set! args (list->strlist args))
  (if (= (length args) 1)
      (fi-set-sock-timeout! (string->number (car args)))
      (display "set-sock-timeout!: wrong number of arguments, type \"help set-sock-timeout!\" for more info\n")))

(fi-register-command! 
 '("set-sock-timeout!"
   "set-sock-timeout!\n\t- sets socket timeout value in milli seconds"))

(define (get-sock-timeout args)
  "get current socket timeout value in milli seconds"
  (display (string-append "current socket timeout: "
			  (number->string (fi-get-sock-timeout))
			  "ms.\n")))
(fi-register-command! 
 '("get-sock-timeout"
   "get-sock-timeout\n\t- get current socket timeout value in milli seconds"))

   
      