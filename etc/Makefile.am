CLEANFILES = *~
DISTCLEANFILES = .deps/*.P

EXTRA_DIST = freeipmi.conf

Freeipmiconfdir     = $(FREEIPMI_SYSCONFDIR)
Freeipmiconf_DATA   = freeipmi.conf

install-data-local:
	$(INSTALL) -m 755 -d $(DESTDIR)$(FREEIPMI_SYSCONFDIR)
	$(INSTALL) -m 644 $(srcdir)/freeipmi.conf $(DESTDIR)$(FREEIPMI_CONFIG_FILE_DEFAULT)

uninstall-local:
	$(RM) -f $(DESTDIR)$(FREEIPMI_CONFIG_FILE_DEFAULT)
