SUBDIRS = src

MANS_CPP = \
	ipmimonitoring.8 \
	libipmimonitoring.3 \
	ipmi_monitoring_sensors.conf.5 \
	ipmimonitoring_sensors.conf.5 \
	ipmimonitoring.conf.5 \
	libipmimonitoring.conf.5

$(MANS_CPP): $(MANS_CPP:%=%.pre)
	$(CPP) -C -P -I. $@.pre  $@

dist_man_MANS = $(MANS_CPP) 

EXTRA_DIST = ipmi_monitoring_sensors.conf

CLEANFILES = $(MANS_CPP) *~

DISTCLEANFILES = $(MANS_CPP) $(MANS_CPP:%=%.pre) .deps/*.P

Monitoringconfdir     = $(sysconfdir)
Monitoringconf_DATA   = ipmi_monitoring_sensors.conf

install-data-local:
	$(INSTALL) -m 755 -d $(DESTDIR)$(sysconfdir)
	$(INSTALL) -m 755 $(srcdir)/ipmi_monitoring_sensors.conf $(DESTDIR)$(IPMI_MONITORING_SENSOR_CONFIG_FILE_DEFAULT)

