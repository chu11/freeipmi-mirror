;;; init.scm: this initialization file is automatically loaded by default
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
;;; /usr/share/fish/extensions/init.scm will be automatically
;;; loaded during the startup. this file inturn loads the standard
;;; fish extensions and also does other external customizations.
;;; To overload this init.scm file, place your own init.scm in ur
;;; ~/.fish/extensions folder.

(use-modules (ice-9 debugger))
(debug-enable 'backtrace)
;; (debug-set! stack 9999999) ; set bigger stack size

(catch #t
       (lambda ()
	 ;; load extensions
	 (fi-load "utils.scm")
	 (fi-load "shell.scm")
	 (fi-load "discovery.scm")
	 (fi-load "sensors.scm")
	 (fi-load "bmc-config.scm")
	 (fi-load "sel.scm")
	 (fi-load "bmc-info.scm"))
       (lambda (k args . opts)
	 (display ">>--:>  >>--:>  >>--:> >>--:>\n")
	 (display "~ ~   Cat ate the fish!!  ~ ~\n")
	 (display ">>--:>  >>--:>  >>--:> >>--:>\n")
	 (display "\nFish Exception (SCM handler dump):")
	 (display "\nkey        : ")
	 (display k)
	 (display "\nthrow args : ")
	 (display args)
	 (display "\nopts       : ")
	 (display opts)
	 (newline)
	 (backtrace)
	 (newline)))

