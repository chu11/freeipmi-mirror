;;; sensors-very-verbose-display.scm: sensors very verbose display procedures
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

(use-modules (ice-9 format))

(define (sensors-display-very-verbose-full-record sdr-record sensor-reading)
  (let ((record-id  (assoc-ref sdr-record "record_id"))
	(record-type (assoc-ref sdr-record "record_type"))
	(sensor-name    (assoc-ref sdr-record "sensor_name"))
	(group-name     (assoc-ref sdr-record "group_name"))
	(sensor-number  (assoc-ref sdr-record "sensor_number"))
	(event-reading-type (assoc-ref sdr-record "event_reading_type"))
	(lower-critical        (assoc-ref sdr-record "lower_critical_threshold"))
	(upper-critical        (assoc-ref sdr-record "upper_critical_threshold"))
	(lower-non-critical    (assoc-ref sdr-record "lower_non_critical_threshold"))
	(upper-non-critical    (assoc-ref sdr-record "upper_non_critical_threshold"))
	(lower-non-recoverable (assoc-ref sdr-record "lower_non_recoverable_threshold"))
	(upper-non-recoverable (assoc-ref sdr-record "upper_non_recoverable_threshold"))
	(is-lower-critical (assoc-ref sdr-record "readable_lower_critical_threshold"))
	(is-upper-critical (assoc-ref sdr-record "readable_upper_critical_threshold"))
	(is-lower-non-critical (assoc-ref sdr-record 
					  "readable_lower_non_critical_threshold"))
	(is-upper-non-critical (assoc-ref sdr-record 
					  "readable_upper_non_critical_threshold"))
	(is-lower-non-recoverable (assoc-ref sdr-record 
					     "readable_lower_non_recoverable_threshold"))
	(is-upper-non-recoverable (assoc-ref sdr-record 
					     "readable_upper_non_recoverable_threshold"))
	(sensor-min-reading (assoc-ref sdr-record "sensor_min_reading"))
	(sensor-max-reading (assoc-ref sdr-record "sensor_max_reading"))
	(normal-min (assoc-ref sdr-record "normal_min"))
	(normal-max (assoc-ref sdr-record "normal_max"))
	(nominal-reading (assoc-ref sdr-record "nominal_reading"))
	(unit-string   (assoc-ref sdr-record "unit_string"))
	(slave-system-software-id (assoc-ref sdr-record "slave_system_software_id"))
	(negative-hysteresis (assoc-ref sdr-record "negative_hysteresis"))
	(positive-hysteresis (assoc-ref sdr-record "positive_hysteresis"))
	(b (assoc-ref sdr-record "b"))
	(m (assoc-ref sdr-record "m"))
	(r_exponent (assoc-ref sdr-record "r_exponent"))
	(b_exponent (assoc-ref sdr-record "b_exponent"))
	(linear (assoc-ref sdr-record "linear"))
	(analog-data-format (assoc-ref sdr-record "analog_data_format"))
	(current-reading     (assoc-ref sensor-reading "current_reading"))
	(event-message (assoc-ref sensor-reading "event_message"))
	(status              (assoc-ref sensor-reading "status")))
    (format #t "Record ID: ~d~%" record-id)
    (format #t "Record Type: ~ah~%" (get-hex-string record-type))
    (format #t "Sensor Name: ~a~%" sensor-name)
    (format #t "Group Name: ~a~%" group-name)
    (format #t "Sensor Number: ~d~%" sensor-number)
    (format #t "Event/Reading Type Code: ~ah~%" (get-hex-string event-reading-type))
    (format #t "Slave Address/System Software ID: ~ah~%" 
	    (get-hex-string slave-system-software-id))
    (format #t "B: ~d~%" b)
    (format #t "M: ~d~%" m)
    (format #t "R Exponent: ~d~%" r_exponent)
    (format #t "B Exponent: ~d~%" b_exponent)
    (format #t "Linear: ~d~%" linear)
    (format #t "Analog Data Format: ~d~%" analog-data-format)
    (if is-lower-critical
	(format #t "Lower Critical Threshold: ~f ~a~%" lower-critical unit-string)
	(format #t "Lower Critical Threshold: ~a~%" "NA"))
    (if is-upper-critical
	(format #t "Upper Critical Threshold: ~f ~a~%" upper-critical unit-string)
	(format #t "Upper Critical Threshold: ~a~%" "NA"))
    (if is-lower-non-critical
	(format #t "Lower Non-Critical Threshold: ~f ~a~%" lower-non-critical unit-string)
	(format #t "Lower Non-Critical Threshold: ~a~%" "NA"))
    (if is-upper-non-critical
	(format #t "Upper Non-Critical Threshold: ~f ~a~%" upper-non-critical unit-string)
	(format #t "Upper Non-Critical Threshold: ~a~%" "NA"))
    (if is-lower-non-recoverable
	(format #t "Lower Non-Recoverable Threshold: ~f ~a~%" 
		lower-non-recoverable unit-string)
	(format #t "Lower Non-Recoverable Threshold: ~a~%" "NA"))
    (if is-upper-non-recoverable
	(format #t "Upper Non-Recoverable Threshold: ~f ~a~%" 
		upper-non-recoverable unit-string)
	(format #t "Upper Non-Recoverable Threshold: ~a~%" "NA"))
    (format #t "Sensor Min. Reading: ~f ~a~%" sensor-min-reading unit-string)
    (format #t "Sensor Max. Reading: ~f ~a~%" sensor-max-reading unit-string)
    (format #t "Normal Min.: ~f ~a~%" normal-min unit-string)
    (format #t "Normal Max.: ~f ~a~%" normal-max unit-string)
    (format #t "Nominal reading: ~f ~a~%" nominal-reading unit-string)
    (format #t "Negative Hysteresis: ~d~%" negative-hysteresis)
    (format #t "Positive Hysteresis: ~d~%" positive-hysteresis)
    (if (boolean? sensor-reading)
	(format #t "Sensor Reading: ~a~%" "NA")
	(format #t "Sensor Reading: ~f ~a~%" current-reading unit-string))
    (if (boolean? sensor-reading)
	(format #t "Sensor Status: ~a~%~%" "Unknown")
	(if status 
	    (format #t "Sensor Status: ~a~%~%" "OK")
	    (format #t "Sensor Status: ~a~%~%" event-message)))))

(define (sensors-display-very-verbose-compact-record sdr-record sensor-reading)
  (let ((record-id          (assoc-ref sdr-record "record_id"))
	(record-type (assoc-ref sdr-record "record_type"))
	(sensor-name        (assoc-ref sdr-record "sensor_name"))
	(group-name         (assoc-ref sdr-record "group_name"))
	(sensor-number      (assoc-ref sdr-record "sensor_number"))
	(event-reading-type (assoc-ref sdr-record "event_reading_type"))
	(event-message      (assoc-ref sensor-reading "event_message"))
	(status             (assoc-ref sensor-reading "status"))
	(slave-system-software-id (assoc-ref sdr-record "slave_system_software_id"))
	(negative-hysteresis (assoc-ref sdr-record "negative_hysteresis"))
	(positive-hysteresis (assoc-ref sdr-record "positive_hysteresis")))
    (format #t "Record ID: ~d~%" record-id)
    (format #t "Record Type: ~ah~%" (get-hex-string record-type))
    (format #t "Sensor Name: ~a~%" sensor-name)
    (format #t "Group Name: ~a~%" group-name)
    (format #t "Sensor Number: ~d~%" sensor-number)
    (format #t "Slave Address/System Software ID: ~ah~%" 
	    (get-hex-string slave-system-software-id))
    (format #t "Event/Reading Type Code: ~ah~%" (get-hex-string event-reading-type))
    (format #t "Negative Hysteresis: ~d~%" negative-hysteresis)
    (format #t "Positive Hysteresis: ~d~%" positive-hysteresis)
    (if (or (boolean? sensor-reading) 
	    (boolean? event-message))
	(format #t "Sensor Status: ~a~%~%" "Unknown")
	(format #t "Sensor Status: ~a~%~%" event-message))))


(define (sensors-display-very-verbose-event-only-record sdr-record sensor-reading)
  (let ((record-id          (assoc-ref sdr-record "record_id"))
	(record-type (assoc-ref sdr-record "record_type"))
	(sensor-name        (assoc-ref sdr-record "sensor_name"))
	(group-name         (assoc-ref sdr-record "group_name"))
	(sensor-number      (assoc-ref sdr-record "sensor_number"))
	(event-reading-type (assoc-ref sdr-record "event_reading_type"))
	(event-message      (assoc-ref sensor-reading "event_message"))
	(status             (assoc-ref sensor-reading "status"))
	(slave-system-software-id (assoc-ref sdr-record "slave_system_software_id")))
    (format #t "Record ID: ~d~%" record-id)
    (format #t "Record Type: ~ah~%" (get-hex-string record-type))
    (format #t "Sensor Name: ~a~%" sensor-name)
    (format #t "Group Name: ~a~%" group-name)
    (format #t "Sensor Number: ~d~%" sensor-number)
    (format #t "Slave Address/System Software ID: ~ah~%" 
	    (get-hex-string slave-system-software-id))
    (format #t "Event/Reading Type Code: ~ah~%" (get-hex-string event-reading-type))
    (if (or (boolean? sensor-reading) 
	    (boolean? event-message))
	(format #t "Sensor Status: ~a~%~%" "Unknown")
	(format #t "Sensor Status: ~a~%~%" event-message))))

(define (sensors-display-very-verbose-entity-asso-record sdr-record sensor-reading)
  (let ((record-id          (assoc-ref sdr-record "record_id"))
	(record-type (assoc-ref sdr-record "record_type"))
	(container-entity-id (assoc-ref sdr-record "container_entity_id"))
	(container-entity-instance (assoc-ref sdr-record "container_entity_instance")))
    (format #t "Record ID: ~d~%" record-id)
    (format #t "Record Type: ~ah~%" (get-hex-string record-type))
    (format #t "Sensor Name: ~a~%" "NONE")
    (format #t "Container Entity ID: ~ah~%" 
	    (get-hex-string container-entity-id))
    (format #t "Container Entity Instance: ~ah~%~%" 
	    (get-hex-string container-entity-instance))))

(define (sensors-display-very-verbose-gen-dev-locator-record sdr-record sensor-reading)
  (let ((record-id          (assoc-ref sdr-record "record_id"))
	(record-type (assoc-ref sdr-record "record_type"))
	(direct-access-address (assoc-ref sdr-record "direct_access_address"))
	(channel-number (assoc-ref sdr-record "channel_number"))
	(device-slave-address (assoc-ref sdr-record "device_slave_address"))
	(private-bus-id (assoc-ref sdr-record "private_bus_id"))
	(lun-master-write-read-command (assoc-ref sdr-record "lun_master_write_read_command"))
	(address-span (assoc-ref sdr-record "address_span"))
	(device-type (assoc-ref sdr-record "device_type"))
	(device-type-modifier (assoc-ref sdr-record "device_type_modifier"))
	(entity-id (assoc-ref sdr-record "entity_id"))
	(entity-instance (assoc-ref sdr-record "entity_instance"))
	(device-name (assoc-ref sdr-record "device_name")))
    (format #t "Record ID: ~d~%" record-id)
    (format #t "Record Type: ~ah~%" (get-hex-string record-type))
    (format #t "Device Name: ~a~%" device-name)
    (format #t "Direct Access Address: ~ah~%" 
	    (get-hex-string direct-access-address))
    (format #t "Channel Number: ~ah~%" 
	    (get-hex-string channel-number))
    (format #t "Direct Slave Address: ~ah~%" 
	    (get-hex-string direct-slave-address))
    (format #t "Access LUN/Bus ID: ~ah~%" 
	    (get-hex-string lun-master-write-read-command))
    (format #t "Address Span: ~ah~%" 
	    (get-hex-string address-span))
    (format #t "Device Type: ~ah~%" 
	    (get-hex-string device-type))
    (format #t "Device Type Modifier: ~ah~%" 
	    (get-hex-string device-type-modifier))
    (format #t "Entity ID: ~ah~%" 
	    (get-hex-string entity-id))
    (format #t "Entity Instance: ~ah~%~%" 
	    (get-hex-string entity-instance))))

(define (sensors-display-very-verbose-fru-dev-locator-record sdr-record sensor-reading)
  (let ((record-id          (assoc-ref sdr-record "record_id"))
	(record-type (assoc-ref sdr-record "record_type"))
	(device-type (assoc-ref sdr-record "device_type"))
	(device-type-modifier (assoc-ref sdr-record "device_type_modifier"))
	(fru-entity-id (assoc-ref sdr-record "fru_entity_id"))
	(fru-entity-instance (assoc-ref sdr-record "fru_entity_instance"))
	(device-name (assoc-ref sdr-record "device_name")))
    (format #t "Record ID: ~d~%" record-id)
    (format #t "Record Type: ~ah~%" (get-hex-string record-type))
    (format #t "Device Name: ~a~%" device-name)
    (format #t "Device Type: ~ah~%" 
	    (get-hex-string device-type))
    (format #t "Device Type Modifier: ~ah~%" 
	    (get-hex-string device-type-modifier))
    (format #t "FRU Entity ID: ~ah~%" 
	    (get-hex-string fru-entity-id))
    (format #t "FRU Entity Instance: ~ah~%~%" 
	    (get-hex-string fru-entity-instance))))

(define (sensors-display-very-verbose-mgmt-cntrlr-dev-locator-record sdr-record sensor-reading)
  (let ((record-id          (assoc-ref sdr-record "record_id"))
	(record-type (assoc-ref sdr-record "record_type"))
	(entity-id (assoc-ref sdr-record "entity_id"))
	(entity-instance (assoc-ref sdr-record "entity_instance"))
	(device-name (assoc-ref sdr-record "device_name")))
    (format #t "Record ID: ~d~%" record-id)
    (format #t "Record Type: ~ah~%" (get-hex-string record-type))
    (format #t "Device Name: ~a~%" device-name)
    (format #t "Entity ID: ~ah~%" 
	    (get-hex-string entity-id))
    (format #t "Entity Instance: ~ah~%~%" 
	    (get-hex-string entity-instance))))

(define (sensors-display-very-verbose-oem-record sdr-record sensor-reading)
  (let ((record-id          (assoc-ref sdr-record "record_id"))
	(record-type (assoc-ref sdr-record "record_type"))
	(manufacturer-id (assoc-ref sdr-record "manufacturer_id"))
	(oem-data (assoc-ref sdr-record "oem_data")))
    (format #t "Record ID: ~d~%" record-id)
    (format #t "Record Type: ~ah~%" (get-hex-string record-type))
    (format #t "Sensor Name: ~a~%" "NONE")
    (format #t "Manufacturer ID: ~ah~%" 
	    (get-hex-string manufacturer-id))
    (format #t "OEM Data: ~a~%~%" oem-data)))

(define (sensors-display-very-verbose sdr-record sensor-reading)
  (let ((record-type (assoc-ref sdr-record "record_type")))
    (cond 
     ((= record-type FI-SDR-FULL-RECORD)
      (begin (sensors-display-very-verbose-full-record sdr-record sensor-reading) #t))
     ((= record-type FI-SDR-COMPACT-RECORD)
      (begin (sensors-display-very-verbose-compact-record sdr-record sensor-reading) #t))
     ((= record-type FI-SDR-EVENT-ONLY-RECORD)
      (begin (sensors-display-very-verbose-event-only-record sdr-record sensor-reading) #t))
     ((= record-type FI-SDR-ENTITY-ASSO-RECORD)
      (begin (sensors-display-very-verbose-entity-asso-record sdr-record sensor-reading) #t))
     ((= record-type FI-SDR-GEN-DEV-LOCATOR-RECORD)
      (begin (sensors-display-very-verbose-gen-dev-locator-record sdr-record sensor-reading) #t))
     ((= record-type FI-SDR-FRU-DEV-LOCATOR-RECORD)
      (begin (sensors-display-very-verbose-fru-dev-locator-record sdr-record sensor-reading) #t))
     ((= record-type FI-SDR-MGMT-CNTRLR-DEV-LOCATOR-RECORD)
      (begin (sensors-display-very-verbose-mgmt-cntrlr-dev-locator-record sdr-record sensor-reading) #t))
     ((= record-type FI-SDR-OEM-RECORD)
      (begin (sensors-display-very-verbose-oem-record sdr-record sensor-reading) #t))
     (else #f))))
