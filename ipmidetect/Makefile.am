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

EXTRA_DIST = freeipmi-ipmidetectd.init ipmidetectd.conf

install-data-local: $(INSTALL_INIT_SCRIPTS)
       $(INSTALL) -m 644 $(srcdir)/ipmidetectd.conf $(DESTDIR)$(sysconfdir)/ipmidetectd.conf

install-init-scripts:
	$(INSTALL) -m 755 -d $(DESTDIR)$(sysconfdir)/init.d
	$(INSTALL) -m 755 $(srcdir)/freeipmi-ipmidetectd.init $(DESTDIR)$(sysconfdir)/init.d/freeipmi-ipmidetectd

uninstall-local:
	$(RM) -f $(DESTDIR)$(sysconfdir)/init.d/freeipmi-ipmidetectd
	$(RM) -f $(DESTDIR)$(sysconfdir)/ipmidetectd.conf
