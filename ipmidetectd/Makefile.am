SUBDIRS = src

EXTRA_DIST = ipmidetectd.init ipmidetectd.conf

install-data-local: $(INSTALL_INIT_SCRIPTS)
	$(INSTALL) -m 755 -d $(DESTDIR)$(FREEIPMI_SYSCONFDIR)
	$(INSTALL) -m 644 $(srcdir)/ipmidetectd.conf $(DESTDIR)$(IPMIDETECTD_CONFIG_FILE_DEFAULT)

install-init-scripts:
	$(INSTALL) -m 755 -d $(DESTDIR)$(sysconfdir)/init.d
	$(INSTALL) -m 755 $(srcdir)/ipmidetectd.init $(DESTDIR)$(sysconfdir)/init.d/ipmidetectd

uninstall-local:
	$(RM) -f $(DESTDIR)$(sysconfdir)/init.d/ipmidetectd
	$(RM) -f $(DESTDIR)$(IPMIDETECTD_CONFIG_FILE_DEFAULT)
