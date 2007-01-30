SUBDIRS = src

dist_man_MANS = ipmimonitoring.8 libipmimonitoring.3

EXTRA_DIST = ipmi_monitoring_sensors.conf

CLEANFILES = *~
DISTCLEANFILES = .deps/*.P

install-data-local:
	$(top_srcdir)/install-sh -m 755 -d $(DESTDIR)/$(sysconfdir)
	$(top_srcdir)/install-sh -m 755 $(srcdir)/ipmi_monitoring_sensors.conf $(DESTDIR)$(sysconfdir)/

uninstall-local:
	$(RM) -f $(DESTDIR)$(sysconfdir)//ipmi_monitoring_sensors.conf
