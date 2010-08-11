SUBDIRS = src

MANS_CPP = \
	ipmidetect.8 \
	ipmidetect.conf.5 \
	ipmidetectd.8 \
	ipmidetectd.conf.5 \
	libipmidetect.3

$(MANS_CPP): $(MANS_CPP:%=%.pre)
	$(CPP) -C -P -I. $@.pre  $@

dist_man_MANS = $(MANS_CPP)

CLEANFILES = $(MANS_CPP) *~

DISTCLEANFILES = $(MANS_CPP) $(MANS_CPP:%=%.pre) .deps/*.P

EXTRA_DIST = ipmidetectd.init ipmidetectd.conf ipmidetect.conf

install-data-local: $(INSTALL_INIT_SCRIPTS)
	$(INSTALL) -m 644 $(srcdir)/ipmidetect.conf $(DESTDIR)$(sysconfdir)/ipmidetect.conf
	$(INSTALL) -m 644 $(srcdir)/ipmidetectd.conf $(DESTDIR)$(sysconfdir)/ipmidetectd.conf

install-init-scripts:
	$(INSTALL) -m 755 -d $(DESTDIR)$(sysconfdir)/init.d
	$(INSTALL) -m 755 $(srcdir)/ipmidetectd.init $(DESTDIR)$(sysconfdir)/init.d/ipmidetectd

uninstall-local:
	$(RM) -f $(DESTDIR)$(sysconfdir)/init.d/ipmidetectd
	$(RM) -f $(DESTDIR)$(sysconfdir)/ipmidetect.conf
	$(RM) -f $(DESTDIR)$(sysconfdir)/ipmidetectd.conf
