SUBDIRS = src

MANS_CPP = ipmimonitoring.8

$(MANS_CPP): $(MANS_CPP:%=%.pre)
	cpp -P -I. -x c $@.pre -o $@

dist_man_MANS = $(MANS_CPP) libipmimonitoring.3

EXTRA_DIST = ipmi_monitoring_sensors.conf

CLEANFILES = $(MANS_CPP) *~
DISTCLEANFILES = $(MANS_CPP) $(MANS_CPP:%=%.pre) .deps/*.P

Monitoringconfdir     = $(sysconfdir)
Monitoringconf_DATA   = ipmi_monitoring_sensors.conf

install-data-local:
	$(INSTALL) -m 755 -d $(DESTDIR)$(sysconfdir)
	$(INSTALL) -m 755 $(srcdir)/ipmi_monitoring_sensors.conf $(DESTDIR)$(sysconfdir)/

