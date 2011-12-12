SUBDIRS = src

if WITH_PKG_CONFIG
pkgconfig_DATA = libipmiconsole.pc
endif

EXTRA_DIST = libipmiconsole.conf

install-data-local:
	$(INSTALL) -m 755 -d $(DESTDIR)$(FREEIPMI_SYSCONFDIR)
	$(INSTALL) -m 644 $(srcdir)/libipmiconsole.conf $(DESTDIR)$(LIBIPMICONSOLE_CONFIG_FILE_DEFAULT)

uninstall-local:
	$(RM) -f $(DESTDIR)$(LIBIPMICONSOLE_CONFIG_FILE_DEFAULT)



