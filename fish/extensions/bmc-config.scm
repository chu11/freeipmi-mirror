;;; bmc-config.scm: BMC configurator
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
;;; bmc-config.scm should be automatically loaded thru init.scm

(use-modules (ice-9 getopt-long))

(define bmc-config-exit-status 0)

(fi-load "bc-common.scm")
(fi-load "bc-user-section.scm")
(fi-load "bc-lan-serial-channel-section.scm")
(fi-load "bc-lan-conf-section.scm")
(fi-load "bc-lan-conf-auth-section.scm")
(fi-load "bc-lan-conf-misc-section.scm")
(fi-load "bc-serial-conf-section.scm")
(fi-load "bc-misc-section.scm")
(fi-load "bc-section.scm")

(define user1_s '("User1" 
		  "Username" 
 		  "Enable_User" 
; 		  "Clear_Password" 
		  "Password" 
		  "LAN_Enable_IPMI_Msgs" 
		  "LAN_Enable_Link_Auth" 
		  "LAN_Enable_Restrict_To_Callback" 
		  "LAN_Privilege_Limit" 
		  "LAN_Session_Limit" 
		  "Serial_Enable_IPMI_Msgs" 
		  "Serial_Enable_Link_Auth" 
		  "Serial_Enable_Restrict_To_Callback" 
		  "Serial_Privilege_Limit" 
		  "Serial_Session_Limit"))

(define user2_s '("User2" 
		  "Username" 
 		  "Enable_User" 
; 		  "Clear_Password" 
		  "Password" 
		  "LAN_Enable_IPMI_Msgs" 
		  "LAN_Enable_Link_Auth" 
		  "LAN_Enable_Restrict_To_Callback" 
		  "LAN_Privilege_Limit" 
		  "LAN_Session_Limit" 
		  "Serial_Enable_IPMI_Msgs" 
		  "Serial_Enable_Link_Auth" 
		  "Serial_Enable_Restrict_To_Callback" 
		  "Serial_Privilege_Limit" 
		  "Serial_Session_Limit"))

(define user3_s '("User3" 
		  "Username" 
 		  "Enable_User" 
; 		  "Clear_Password" 
		  "Password" 
		  "LAN_Enable_IPMI_Msgs" 
		  "LAN_Enable_Link_Auth" 
		  "LAN_Enable_Restrict_To_Callback" 
		  "LAN_Privilege_Limit" 
		  "LAN_Session_Limit" 
		  "Serial_Enable_IPMI_Msgs" 
		  "Serial_Enable_Link_Auth" 
		  "Serial_Enable_Restrict_To_Callback" 
		  "Serial_Privilege_Limit" 
		  "Serial_Session_Limit"))

(define user4_s '("User4" 
		  "Username" 
 		  "Enable_User" 
; 		  "Clear_Password" 
		  "Password" 
		  "LAN_Enable_IPMI_Msgs" 
		  "LAN_Enable_Link_Auth" 
		  "LAN_Enable_Restrict_To_Callback" 
		  "LAN_Privilege_Limit" 
		  "LAN_Session_Limit" 
		  "Serial_Enable_IPMI_Msgs" 
		  "Serial_Enable_Link_Auth" 
		  "Serial_Enable_Restrict_To_Callback" 
		  "Serial_Privilege_Limit" 
		  "Serial_Session_Limit"))

(define lan_channel_s '("LAN_Channel" 
			"Volatile_Access_Mode" 
			"Volatile_Enable_User_Level_Auth" 
			"Volatile_Enable_Per_Message_Auth" 
			"Volatile_Enable_Pef_Alerting" 
			"Volatile_Channel_Privilege_Limit" 
			"Non_Volatile_Access_Mode" 
			"Non_Volatile_Enable_User_Level_Auth" 
			"Non_Volatile_Enable_Per_Message_Auth" 
			"Non_Volatile_Enable_Pef_Alerting" 
			"Non_Volatile_Channel_Privilege_Limit"))

(define lan_conf_s '("LAN_Conf" 
		     "IP_Address_Source" 
		     "IP_Address" 
		     "MAC_Address" 
		     "Subnet_Mask" 
		     "Default_Gateway_IP_Address" 
		     "Default_Gateway_MAC_Address" 
		     "Backup_Gateway_IP_Address" 
		     "Backup_Gateway_MAC_Address"))

(define lan_conf_auth_s '("LAN_Conf_Auth" 
			  "Callback_Enable_Auth_Type_None" 
			  "Callback_Enable_Auth_Type_MD2" 
			  "Callback_Enable_Auth_Type_MD5" 
			  "Callback_Enable_Auth_Type_Straight_Password" 
			  "Callback_Enable_Auth_Type_OEM_Proprietary" 
			  "User_Enable_Auth_Type_None" 
			  "User_Enable_Auth_Type_MD2" 
			  "User_Enable_Auth_Type_MD5" 
			  "User_Enable_Auth_Type_Straight_Password" 
			  "User_Enable_Auth_Type_OEM_Proprietary" 
			  "Operator_Enable_Auth_Type_None" 
			  "Operator_Enable_Auth_Type_MD2" 
			  "Operator_Enable_Auth_Type_MD5" 
			  "Operator_Enable_Auth_Type_Straight_Password" 
			  "Operator_Enable_Auth_Type_OEM_Proprietary" 
			  "Admin_Enable_Auth_Type_None" 
			  "Admin_Enable_Auth_Type_MD2" 
			  "Admin_Enable_Auth_Type_MD5" 
			  "Admin_Enable_Auth_Type_Straight_Password" 
			  "Admin_Enable_Auth_Type_OEM_Proprietary" 
			  "OEM_Enable_Auth_Type_None" 
			  "OEM_Enable_Auth_Type_MD2" 
			  "OEM_Enable_Auth_Type_MD5" 
			  "OEM_Enable_Auth_Type_Straight_Password" 
			  "OEM_Enable_Auth_Type_OEM_Proprietary"))

(define lan_conf_misc_s '("LAN_Conf_Misc" 
			  "Enable_Gratuitous_ARPs" 
			  "Enable_ARP_Response" 
			  "Gratuitous_ARP_Interval"))

(define serial_channel_s '("Serial_Channel" 
			   "Volatile_Access_Mode" 
			   "Volatile_Enable_User_Level_Auth" 
			   "Volatile_Enable_Per_Message_Auth" 
			   "Volatile_Enable_Pef_Alerting" 
			   "Volatile_Channel_Privilege_Limit" 
			   "Non_Volatile_Access_Mode" 
			   "Non_Volatile_Enable_User_Level_Auth" 
			   "Non_Volatile_Enable_Per_Message_Auth" 
			   "Non_Volatile_Enable_Pef_Alerting" 
			   "Non_Volatile_Channel_Privilege_Limit"))

(define serial_conf_s '("Serial_Conf" 
			"Enable_Basic_Mode" 
			"Enable_PPP_Mode" 
			"Enable_Terminal_Mode" 
			"Connect_Mode" 
			"Page_Blackout_Interval" 
			"Call_Retry_Time" 
			"Enable_DTR_Hangup" 
			"Flow_Control" 
			"Bit_Rate"))

(define misc_s '("Misc" 
		 "Power_Restore_Policy"))

(define (checkout-conf)
  (checkout-section user1_s (current-output-port))
  (checkout-section user2_s (current-output-port))
  (checkout-section user3_s (current-output-port))
  (checkout-section user4_s (current-output-port))
  (checkout-section lan_channel_s (current-output-port))
  (checkout-section lan_conf_s (current-output-port))
  (checkout-section lan_conf_auth_s (current-output-port))
  (checkout-section lan_conf_misc_s (current-output-port))
  (checkout-section serial_channel_s (current-output-port))
  (checkout-section serial_conf_s (current-output-port))
  (checkout-section misc_s (current-output-port)))

(define (checkout-conf-to-file filename)
  (if (string-null? filename)
      (checkout-conf)
      (let ((fp (open-output-file filename)))
	(checkout-section user1_s fp)
	(checkout-section user2_s fp)
	(checkout-section user3_s fp)
	(checkout-section user4_s fp)
	(checkout-section lan_channel_s fp)
	(checkout-section lan_conf_s fp)
	(checkout-section lan_conf_auth_s fp)
	(checkout-section lan_conf_misc_s fp)
	(checkout-section serial_channel_s fp)
	(checkout-section serial_conf_s fp)
	(checkout-section misc_s fp)
	(close fp))))

(define (validate-conf-file fp)
  (let ((section (read-section fp)))
    (if (null? section)
	#t 
	(if (validate-commit-section section)
	    (validate-conf-file fp)
	    #f))))

(define (commit-conf-file fp)
  (let ((section (read-section fp)))
    (if (null? section)
	#t
	(if (commit-section section)
	    (commit-conf-file fp)
	    #f))))

(define (validate-key-pair-list key-pair-list)
  (if (null? key-pair-list)
      #t
      (let* ((key-string (car key-pair-list))
	     (value      (cadr key-pair-list))
	     (section-data (make-section key-string value)))
	(if (list? section-data)
	    (if (validate-commit-section section-data)
		(validate-key-pair-list (cddr key-pair-list))
		#f)
	    #f))))

(define (commit-key-pair-list key-pair-list)
  (if (null? key-pair-list)
      #t
      (let* ((key-string (car key-pair-list))
	     (value      (cadr key-pair-list))
	     (section-data (make-section key-string value)))
	(if (list? section-data)
	    (if (commit-section section-data)
		(commit-key-pair-list (cddr key-pair-list))
		#f)
	    #f))))

(define (diff-key-pair-list key-pair-list)
  (if (null? key-pair-list)
      #t
      (let* ((key-string (car key-pair-list))
	     (value      (cadr key-pair-list))
	     (section-data (make-section key-string value)))
	(if (list? section-data)
	    (if (diff-section section-data)
		(diff-key-pair-list (cddr key-pair-list))
		#f)
	    #f))))

(define (diff-conf-file fp)
  (let ((section (read-section fp)))
    (if (null? section)
	#t
	(if (diff-section section)
	    (diff-conf-file fp)
	    #f))))


(define (bc-display-usage)
  (display "bmc-config --usage --help --version  --checkout --commit --diff --filename=FILENAME --key-pair=KEY-PAIR\n\tBMC Configurator.\n"))

(define (bc-display-help)
  (begin 
    (display "BMC Configurator.\n\n")
    (display "Options:\n")
    (display "  -u, --usage                Usage message\n") 
    (display "  -h, --help                 Show help\n")
    (display "  -V, --version              Show version\n")
    (display "  -o, --checkout             Fetch configuration information from BMC.\n")
    (display "  -i, --commit               Update configuration information to BMC\n")
    (display "  -d, --diff                 Show differences with BMC\n")
    (display "  -f FILENAME, --filename=FILENAME    Use this file for checkout/commit/diff\n")
    (display "  -k \"KEY=VALUE\", --key-pair=\"KEY=VALUE\"    checkout/diff only this KEY/VALUE\n")
))

(define (bc-display-version)
  (display (string-append 
	    "BMC Configurator." 
	    (fi-version)))
  (newline))

(define (bc-main args)
  (let* ((option-spec '((usage    (single-char #\u) (value #f))
			(help     (single-char #\h) (value #f))
			(version  (single-char #\V) (value #f))
			(checkout (single-char #\o) (value #f))
			(commit   (single-char #\i) (value #f))
			(diff     (single-char #\d) (value #f))
			(filename (single-char #\f) (value #t))
			(key-pair (single-char #\k) (value #t))))
	 (options (getopt-long args option-spec))
	 (usage-wanted         (option-ref options 'usage    #f))
	 (help-wanted          (option-ref options 'help     #f))
	 (version-wanted       (option-ref options 'version  #f))
	 (checkout-wanted      (option-ref options 'checkout #f))
	 (commit-wanted        (option-ref options 'commit   #f))
	 (diff-wanted          (option-ref options 'diff     #f))
	 (filename             (option-ref options 'filename ""))
	 (key-pair-list   (if (option-ref options 'key-pair #f)
			      (let ((klist '()))
				(map (lambda (arg)
				       (if (equal? (car arg) 'key-pair)
					   (if (string-index (cdr arg) #\=) 
					       (set! klist 
						     (append 
						      klist 
						      (string-separate (cdr arg) #\=)))
                                               (begin
                                                 (set! usage-wanted #t)
                                                 (set! bmc-config-exit-status -1)))))
				     options)
				klist)
			      '()))
; 	 (key-pair-list (if (option-ref options 'key-pair #f)
; 			    (let ((klist '()))
; 			      (map (lambda (arg)
; 				     (if (equal? (car arg) 'key-pair)
; 					 (begin 
; 					   (display (cdr arg))
; 					   (newline))))
; 				   options)
; 			      klist)
; 			    '()))
)
    (cond 
     ;; argument type check
     (usage-wanted
      (bc-display-usage))
     (help-wanted
      (bc-display-help))
     (version-wanted
      (bc-display-version))
     (checkout-wanted 
      (if (not (checkout-conf-to-file filename))
	  (set! bmc-config-exit-status -1)))
     (commit-wanted 
      (if (and (string-null? filename) (null? key-pair-list))
	  (bc-display-help)
	  (begin 
	    (if (not (string-null? filename))
		(if (file-exists? filename)
		    (let ((fp (open-input-file filename)))
		      (if (validate-conf-file fp)
			  (begin 
			    (seek fp 0 SEEK_SET)
			    (if (not (commit-conf-file fp))
				(set! bmc-config-exit-status -1)))
			  (set! bmc-config-exit-status -1))
		      (close fp))))
	    (if (not (null? key-pair-list))
		(if (validate-key-pair-list key-pair-list)
		    (if (not (commit-key-pair-list key-pair-list))
			(set! bmc-config-exit-status -1))
		    (set! bmc-config-exit-status -1))))))
     (diff-wanted 
      (if (and (string-null? filename) (null? key-pair-list))
	  (bc-display-help)
	  (begin 
	    (if (not (string-null? filename))
		(if (file-exists? filename)
		    (let ((fp (open-input-file filename)))
		      (if (validate-conf-file fp)
			  (begin 
			    (seek fp 0 SEEK_SET)
			    (if (not (diff-conf-file fp))
				(set! bmc-config-exit-status -1)))
			  (set! bmc-config-exit-status -1))
		      (close fp))))
	    (if (not (null? key-pair-list))
		(if (validate-key-pair-list key-pair-list)
		    (if (not (diff-key-pair-list key-pair-list))
			(set! bmc-config-exit-status -1))
		    (set! bmc-config-exit-status -1))))))
     (else 
      (bc-display-help)))))


(define (bmc-config args)
  "fish bmc-config main"
  (set! args (list->strlist args))
  (catch 'misc-error
	 (lambda ()
	   (bc-main (append '("bmc-config") args)))
	 (lambda (k args . opts)
	   (display "bmc-config: error: ")
	   (display (cadr opts))
	   (newline))))

(fi-register-command! 
 '("bmc-config" 
   "bmc-config --usage --help --version --checkout --commit --diff --filename FILENAME --key-pair=\"KEY=VALUE\" ...\n\tGet and set BMC configurations."))

;;; main starts here ;;;

; (define fp (open-input-file "bc2.conf"))
; (if (validate-conf-file fp)
;     (begin 
;       (display "bc2.conf is validated\n")
;       (seek fp 0 SEEK_SET)
;       (if (commit-conf-file fp)
; 	  (display "bc2.conf is committed\n")
; 	  (display "unable to commit file bc2.conf\n")))
;     (display "fix the file and rerun\n"))
; (close fp)

