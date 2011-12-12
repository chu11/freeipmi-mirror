SUBDIRS = src

EXTRA_DIST = ipmidetect.conf

install-data-local:
	$(INSTALL) -m 755 -d $(DESTDIR)$(FREEIPMI_SYSCONFDIR)
	$(INSTALL) -m 644 $(srcdir)/ipmidetect.conf $(DESTDIR)$(IPMIDETECT_CONFIG_FILE_DEFAULT)

uninstall-local:
	$(RM) -f $(DESTDIR)$(IPMIDETECT_CONFIG_FILE_DEFAULT)
