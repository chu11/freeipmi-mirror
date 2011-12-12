MANS_CPP = \
	bmc-config.8 \
	bmc-config.conf.5 \
	bmc-device.8 \
	bmc-info.8 \
	bmc-watchdog.8 \
	ipmi-chassis.8 \
	ipmi-chassis-config.8 \
	ipmi-dcmi.8 \
	ipmi-fru.8 \
	ipmi-locate.8 \
	ipmi-oem.8 \
	ipmi-pef-config.8 \
	ipmi-pet.8 \
	ipmi-raw.8 \
	ipmi-sel.8 \
	ipmi-sensors.8 \
	ipmi-sensors-config.8 \
	ipmiconsole.8 \
	ipmidetect.8 \
	ipmidetect.conf.5 \
	ipmidetectd.8 \
	ipmidetectd.conf.5 \
	ipmiping.8 \
	ipmipower.8 \
	freeipmi.7 \
	freeipmi.conf.5 \
	freeipmi_interpret_sel.conf.5 \
	freeipmi_interpret_sensor.conf.5 \
	libipmiconsole.3 \
	libipmiconsole.conf.5 \
	libipmidetect.3 \
	libfreeipmi.3 \
	libipmimonitoring.3 \
	rmcpping.8

MANS_NOCPP = \
	ipmi-console.8 \
	ipmi-detect.8 \
	ipmi-ping.8 \
	ipmi-power.8 \
	ipmiconsole.conf.5 \
	ipmipower.conf.5 \
	ipmimonitoring.8 \
	ipmimonitoring_sensors.conf.5 \
	ipmimonitoring.conf.5 \
	ipmi_monitoring_sensors.conf.5 \
	libipmimonitoring.conf.5 \
	pef-config.8 \
	rmcp-ping.8

$(MANS_CPP): $(MANS_CPP:%=%.pre)
	$(CPP) -w -C -P -I. $@.pre  $@

dist_man_MANS = \
	$(MANS_CPP) \
	$(MANS_NOCPP)

CLEANFILES = $(MANS_CPP) *~
DISTCLEANFILES = $(MANS_CPP) $(MANS_CPP:%=%.pre) .deps/*.P

EXTRA_DIST = $(MANS_NOCPP) 