SUBDIRS = include src 

MANS_CPP = freeipmi_interpret_sel.conf.5 freeipmi_interpret_sensor.conf.5  libfreeipmi.3

$(MANS_CPP): $(MANS_CPP:%=%.pre)
	$(CPP) -C -P -I. $@.pre  $@

dist_man_MANS = $(MANS_CPP)

CLEANFILES = $(MANS_CPP) *~

DISTCLEANFILES = $(MANS_CPP) $(MANS_CPP:%=%.pre) .deps/*.P

# Semaphore IPC Key file for ftok:
Ipckeydir     = $(localstatedir)/lib/$(PACKAGE_NAME)
Ipckey_DATA   = ipckey

EXTRA_DIST = freeipmi_interpret_sel.conf freeipmi_interpret_sensor.conf ipckey

Interpretconfdir     = $(FREEIPMI_SYSCONFDIR)
Interpretconf_DATA   = freeipmi_interpret_sel.conf freeipmi_interpret_sensor.conf

install-data-local:
	$(INSTALL) -m 755 -d $(DESTDIR)$(FREEIPMI_SYSCONFDIR)
	$(INSTALL) -m 644 $(srcdir)/freeipmi_interpret_sel.conf $(DESTDIR)$(INTERPRET_SEL_CONFIG_FILE_DEFAULT)
	$(INSTALL) -m 644 $(srcdir)/freeipmi_interpret_sensor.conf $(DESTDIR)$(INTERPRET_SENSOR_CONFIG_FILE_DEFAULT)

uninstall-local:
	$(RM) -f $(DESTDIR)$(IPMI_MONITORING_SEL_CONFIG_FILE_DEFAULT)
	$(RM) -f $(DESTDIR)$(IPMI_MONITORING_SENSOR_CONFIG_FILE_DEFAULT)



