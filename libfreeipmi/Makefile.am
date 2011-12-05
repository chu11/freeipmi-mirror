SUBDIRS = include src 

MANS_CPP = freeipmi_interpret_sel.conf.5 freeipmi_interpret_sensor.conf.5  libfreeipmi.3

$(MANS_CPP): $(MANS_CPP:%=%.pre)
	$(CPP) -w -C -P -I. $@.pre  $@

dist_man_MANS = $(MANS_CPP)

if WITH_PKG_CONFIG
pkgconfig_DATA = libfreeipmi.pc
endif

CLEANFILES = $(MANS_CPP) *~

DISTCLEANFILES = $(MANS_CPP) $(MANS_CPP:%=%.pre) .deps/*.P

EXTRA_DIST = freeipmi_interpret_sel.conf freeipmi_interpret_sensor.conf ipckey

install-data-local:
	$(INSTALL) -m 755 -d $(DESTDIR)$(FREEIPMI_SYSCONFDIR)
	$(INSTALL) -m 644 $(srcdir)/freeipmi_interpret_sel.conf $(DESTDIR)$(INTERPRET_SEL_CONFIG_FILE_DEFAULT)
	$(INSTALL) -m 644 $(srcdir)/freeipmi_interpret_sensor.conf $(DESTDIR)$(INTERPRET_SENSOR_CONFIG_FILE_DEFAULT)
	$(INSTALL) -m 755 -d $(DESTDIR)$(localstatedir)/lib/$(PACKAGE_NAME)
	$(INSTALL) -m 644 $(srcdir)/ipckey $(DESTDIR)$(localstatedir)/lib/$(PACKAGE_NAME)/ipckey

uninstall-local:
	$(RM) -f $(DESTDIR)$(INTERPRET_SEL_CONFIG_FILE_DEFAULT)
	$(RM) -f $(DESTDIR)$(INTERPRET_SENSOR_CONFIG_FILE_DEFAULT)
	$(RM) -f $(DESTDIR)$(localstatedir)/lib/$(PACKAGE_NAME)/ipckey



