SUBDIRS = src

dist_man_MANS = ipmimonitoring.8 libipmimonitoring.3

EXTRA_DIST = ipmi_monitoring_sensors.conf

CLEANFILES = *~
DISTCLEANFILES = .deps/*.P

Monitoringconfdir     = $(sysconfdir)
Monitoringconf_DATA   = ipmi_monitoring_sensors.conf

install-data-local:
	$(INSTALL) -m 755 -d $(DESTDIR)$(sysconfdir)
	$(INSTALL) -m 755 $(srcdir)/ipmi_monitoring_sensors.conf $(DESTDIR)$(sysconfdir)/

