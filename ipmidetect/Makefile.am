SUBDIRS = src

MANS_CPP = \
	ipmidetect.8 \
	ipmidetect.conf.5

$(MANS_CPP): $(MANS_CPP:%=%.pre)
	$(CPP) -w -C -P -I. $@.pre  $@

dist_man_MANS = $(MANS_CPP) ipmi-detect.8

CLEANFILES = $(MANS_CPP) *~

DISTCLEANFILES = $(MANS_CPP) $(MANS_CPP:%=%.pre) .deps/*.P

EXTRA_DIST = ipmidetect.conf ipmi-detect.8

install-data-local:
	$(INSTALL) -m 755 -d $(DESTDIR)$(FREEIPMI_SYSCONFDIR)
	$(INSTALL) -m 644 $(srcdir)/ipmidetect.conf $(DESTDIR)$(IPMIDETECT_CONFIG_FILE_DEFAULT)

uninstall-local:
	$(RM) -f $(DESTDIR)$(IPMIDETECT_CONFIG_FILE_DEFAULT)
