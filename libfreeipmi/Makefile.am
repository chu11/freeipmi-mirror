SUBDIRS = include src 

MANS_CPP = libfreeipmi.3 freeipmi_interpret_sensors.conf.5 freeipmi_interpret_sel.conf.5

$(MANS_CPP): $(MANS_CPP:%=%.pre)
	$(CPP) -C -P -I. $@.pre  $@

dist_man_MANS = $(MANS_CPP)

CLEANFILES = $(MANS_CPP) *~

DISTCLEANFILES = $(MANS_CPP) $(MANS_CPP:%=%.pre) .deps/*.P

# Semaphore IPC Key file for ftok:
Ipckeydir     = $(localstatedir)/lib/$(PACKAGE_NAME)
Ipckey_DATA   = ipckey

EXTRA_DIST = ipckey freeipmi_interpret_sensors.conf freeipmi_interpret_sel.conf

Interpretconfdir     = $(sysconfdir)
Interpretconf_DATA   = freeipmi_interpret_sensors.conf freeipmi_interpret_sel.conf

install-data-local:
	$(INSTALL) -m 755 -d $(DESTDIR)$(sysconfdir)
	$(INSTALL) -m 755 $(srcdir)/freeipmi_interpret_sensors.conf $(DESTDIR)$(INTERPRET_SENSOR_CONFIG_FILE_DEFAULT)
	$(INSTALL) -m 755 $(srcdir)/freeipmi_interpret_sel.conf $(DESTDIR)$(INTERPRET_SEL_CONFIG_FILE_DEFAULT)

uninstall-local:
	$(RM) -f $(DESTDIR)$(IPMI_MONITORING_SENSOR_CONFIG_FILE_DEFAULT)/freeipmi_interpret_sensors.conf
	$(RM) -f $(DESTDIR)$(IPMI_MONITORING_SEL_CONFIG_FILE_DEFAULT)/freeipmi_interpret_sel.conf



