SUBDIRS = include src 

if WITH_PKG_CONFIG
pkgconfig_DATA = libfreeipmi.pc
endif

EXTRA_DIST = ipckey

install-data-local:
	$(INSTALL) -m 755 -d $(DESTDIR)$(localstatedir)/lib/$(PACKAGE_NAME)
	$(INSTALL) -m 644 $(srcdir)/ipckey $(DESTDIR)$(localstatedir)/lib/$(PACKAGE_NAME)/ipckey

uninstall-local:
	$(RM) -f $(DESTDIR)$(localstatedir)/lib/$(PACKAGE_NAME)/ipckey



