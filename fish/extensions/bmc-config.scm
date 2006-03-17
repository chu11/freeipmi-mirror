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

(fi-load "bc-common.scm")
(fi-load "bc-user-section.scm")
(fi-load "bc-lan-serial-channel-section.scm")
(fi-load "bc-lan-conf-section.scm")
(fi-load "bc-lan-conf-auth-section.scm")
(fi-load "bc-lan-conf-security-keys-section.scm")
(fi-load "bc-lan-conf-misc-section.scm")
(fi-load "bc-rmcpplus-conf-privilege-section.scm")
(fi-load "bc-serial-conf-section.scm")
(fi-load "bc-sol-conf-section.scm")
(fi-load "bc-pef-conf-section.scm")
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
                  "SOL_Payload_Access"
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
                  "SOL_Payload_Access"
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
                  "SOL_Payload_Access"
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
                  "SOL_Payload_Access"
		  "Serial_Enable_IPMI_Msgs" 
		  "Serial_Enable_Link_Auth" 
		  "Serial_Enable_Restrict_To_Callback" 
		  "Serial_Privilege_Limit" 
		  "Serial_Session_Limit"))

(define user5_s '("User5" 
		  "Username" 
 		  "Enable_User" 
; 		  "Clear_Password" 
		  "Password" 
		  "LAN_Enable_IPMI_Msgs" 
		  "LAN_Enable_Link_Auth" 
		  "LAN_Enable_Restrict_To_Callback" 
		  "LAN_Privilege_Limit" 
		  "LAN_Session_Limit" 
                  "SOL_Payload_Access"
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
		     "Backup_Gateway_MAC_Address"
                     "Vlan_Id_Enable"
                     "Vlan_Id"                     
                     "Vlan_Priority"))

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

(define lan_conf_security_keys_s '("LAN_Conf_Security_Keys" 
                                   "K_R" 
                                   "K_G"))

(define lan_conf_misc_s '("LAN_Conf_Misc" 
			  "Enable_Gratuitous_ARPs" 
			  "Enable_ARP_Response" 
			  "Gratuitous_ARP_Interval"))

(define rmcpplus_conf_privilege_s '("Rmcpplus_Conf_Privilege" 
				    "Maximum_Privilege_Cipher_Suite_Id_0"
				    "Maximum_Privilege_Cipher_Suite_Id_1"
				    "Maximum_Privilege_Cipher_Suite_Id_2"
				    "Maximum_Privilege_Cipher_Suite_Id_3"
				    "Maximum_Privilege_Cipher_Suite_Id_4"
				    "Maximum_Privilege_Cipher_Suite_Id_5"
				    "Maximum_Privilege_Cipher_Suite_Id_6"
				    "Maximum_Privilege_Cipher_Suite_Id_7"
				    "Maximum_Privilege_Cipher_Suite_Id_8"
				    "Maximum_Privilege_Cipher_Suite_Id_9"
				    "Maximum_Privilege_Cipher_Suite_Id_10"
				    "Maximum_Privilege_Cipher_Suite_Id_11"
				    "Maximum_Privilege_Cipher_Suite_Id_12"
				    "Maximum_Privilege_Cipher_Suite_Id_13"
				    "Maximum_Privilege_Cipher_Suite_Id_14"))
  
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

(define pef_conf_s '("PEF_Conf" 
		     "Enable_PEF" 
		     "Enable_PEF_Event_Messages" 
		     "Enable_PEF_Startup_Delay" 
		     "Enable_PEF_Alert_Startup_Delay" 
		     "Enable_Alert_Action" 
		     "Enable_Powerdowm_Action" 
		     "Enable_Reset_Action" 
		     "Enable_Powercycle_Action" 
		     "Enable_OEM_Action" 
		     "Enable_Diagnostic_Interrupt" 
		     "PEF_Startup_Delay" 
		     "PEF_Alert_Startup_Delay"))

(define sol_conf_s '("SOL_Conf" 
		     "Enable_SOL" 
                     "SOL_Privilege_Level"
                     "Force_SOL_Payload_Authentication"
                     "Force_SOL_Payload_Encryption"
                     "Character_Accumulate_Interval"
                     "Character_Send_Threshold"
                     "SOL_Retry_Count"
                     "SOL_Retry_Interval"
                     "Non_Volatile_Bit_Rate"
                     "Volatile_Bit_Rate"
                     "SOL_Payload_Port_Number"))

(define misc_s '("Misc" 
		 "Power_Restore_Policy"))

(define (checkout-conf)
  (checkout-section user1_s (current-output-port))
  (checkout-section user2_s (current-output-port))
  (checkout-section user3_s (current-output-port))
  (checkout-section user4_s (current-output-port))
  (checkout-section user5_s (current-output-port))
  (checkout-section lan_channel_s (current-output-port))
  (checkout-section lan_conf_s (current-output-port))
  (checkout-section lan_conf_auth_s (current-output-port))
  (checkout-section lan_conf_security_keys_s (current-output-port))
  (checkout-section lan_conf_misc_s (current-output-port))
  (checkout-section rmcpplus_conf_privilege_s (current-output-port))
  (checkout-section serial_channel_s (current-output-port))
  (checkout-section serial_conf_s (current-output-port))
  (checkout-section pef_conf_s (current-output-port))
  (checkout-section sol_conf_s (current-output-port))
  (checkout-section misc_s (current-output-port)))

(define (checkout-conf-to-file filename)
  (if (boolean? filename)
      (checkout-conf)
      (let ((fp (open-output-file filename)))
	(checkout-section user1_s fp)
	(checkout-section user2_s fp)
	(checkout-section user3_s fp)
	(checkout-section user4_s fp)
	(checkout-section user5_s fp)
	(checkout-section lan_channel_s fp)
	(checkout-section lan_conf_s fp)
	(checkout-section lan_conf_auth_s fp)
	(checkout-section lan_conf_security_keys_s fp)
	(checkout-section lan_conf_misc_s fp)
	(checkout-section serial_channel_s fp)
	(checkout-section serial_conf_s fp)
	(checkout-section pef_conf_s fp)
	(checkout-section sol_conf_s fp)
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

(define (bmc-config-main cmd-args)
  (cond 
   ((bmc-config-get-help-option cmd-args)
    (bmc-config-display-help))
   ((bmc-config-get-usage-option cmd-args)
    (bmc-config-display-usage))
   ((bmc-config-get-version-option cmd-args)
    (bmc-config-display-version))
   (else 
    (and (fi-ipmi-open cmd-args)
	 (cond 
	  ;; checkout
	  ((bmc-config-get-checkout-option cmd-args)
	   (if (not (checkout-conf-to-file 
		     (bmc-config-get-filename-option cmd-args)))
	       (set! bmc-config-exit-status -1)))
	  ;; commit
	  ((bmc-config-get-commit-option cmd-args)
	   (let ((filename (bmc-config-get-filename-option cmd-args))
		 (key-pair-list (bmc-config-get-key-pair-option cmd-args)))
	     (begin 
	       (if (string? filename)
		   (if (file-exists? filename)
		       (let ((fp (open-input-file filename)))
			 (if (validate-conf-file fp)
			     (begin 
			       (seek fp 0 SEEK_SET)
			       (if (not (commit-conf-file fp))
				   (set! bmc-config-exit-status -1)))
			     (set! bmc-config-exit-status -1))
			 (close fp))))
	       (if (list? key-pair-list)
		   (if (validate-key-pair-list key-pair-list)
		       (if (not (commit-key-pair-list key-pair-list))
			   (set! bmc-config-exit-status -1))
		       (set! bmc-config-exit-status -1))))))
	  ;; diff
	  ((bmc-config-get-diff-option cmd-args)
	   (let ((filename (bmc-config-get-filename-option cmd-args))
		 (key-pair-list (bmc-config-get-key-pair-option cmd-args)))
	     (begin 
	       (if (string? filename)
		   (if (file-exists? filename)
		       (let ((fp (open-input-file filename)))
			 (if (validate-conf-file fp)
			     (begin 
			       (seek fp 0 SEEK_SET)
			       (if (not (diff-conf-file fp))
				   (set! bmc-config-exit-status -1)))
			     (set! bmc-config-exit-status -1))
			 (close fp))))
	       (if (list? key-pair-list)
		   (if (validate-key-pair-list key-pair-list)
		       (if (not (diff-key-pair-list key-pair-list))
			   (set! bmc-config-exit-status -1))
		       (set! bmc-config-exit-status -1)))))))
	 (fi-ipmi-close)))))

(define (bmc-config args)
  "fish bmc-config main"
  (let ((cmd-args (bmc-config-argp (append (list "bmc-config") 
					   (list->strlist args)))))
    (if (list? cmd-args)
	(bmc-config-main cmd-args))))

(fi-register-command! 
 (list "bmc-config" 
       (string-append 
	"bmc-config [--no-probing] [--driver-type=IPMIDRIVER]\n"
	"           [--driver-address=DRIVERADDR] [--driver-device=DEVICE]\n"
	"           [--hostname=IPMIHOST] [--username=USERNAME]\n"
	"           [--password=PASSWORD] [--auth-type=AUTHTYPE]\n"
	"           [--priv-level=PRIVILEGE-LEVEL] [--checkout] [--commit]\n"
	"           [--diff] [--filename=FILENAME] [--key-pair=KEY-PAIR]\n"
	"           [--help] [--usage] [--version]\n"
	"\n"
	"   Displays information about BMC.")))
