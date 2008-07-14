CLEANFILES = *~
DISTCLEANFILES = .deps/*.P

EXTRA_DIST = freeipmi.conf

Freeipmiconfdir     = $(sysconfdir)
Freeipmiconf_DATA   = freeipmi.conf

install-data-local:
	$(INSTALL) -m 755 -d $(DESTDIR)$(sysconfdir)
	$(INSTALL) -m 755 $(srcdir)/freeipmi.conf $(DESTDIR)$(sysconfdir)
