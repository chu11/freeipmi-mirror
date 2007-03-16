SUBDIRS = src

dist_man_MANS = ipmidetect.8 ipmidetect.conf.5 ipmidetectd.8 ipmidetectd.conf.5 libipmidetect.3

EXTRA_DIST = freeipmi-ipmidetectd.init

install-data-local: 
	$(INSTALL) -m 755 -d $(DESTDIR)$(sysconfdir)/init.d
	$(INSTALL) -m 755 $(srcdir)/freeipmi-ipmidetectd.init $(DESTDIR)$(sysconfdir)/init.d/freeipmi-ipmidetectd

uninstall-local:
	$(RM) -f $(DESTDIR)$(sysconfdir)/init.d/freeipmi-ipmidetectd

CLEANFILES = *~
DISTCLEANFILES = .deps/*.P
