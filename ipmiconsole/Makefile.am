SUBDIRS = src

MANS_CPP = \
	ipmiconsole.8 \
	ipmiconsole.conf.5 \
	libipmiconsole.3 \
	libipmiconsole.conf.5

$(MANS_CPP): $(MANS_CPP:%=%.pre)
	$(CPP) -C -P -I. $@.pre  $@

dist_man_MANS = $(MANS_CPP) ipmi-console.8

CLEANFILES = $(MANS_CPP) *~

DISTCLEANFILES = $(MANS_CPP) $(MANS_CPP:%=%.pre) .deps/*.P

EXTRA_DIST = ipmi-console.8 libipmiconsole.conf

install-data-local:
	$(INSTALL) -m 755 -d $(DESTDIR)$(FREEIPMI_SYSCONFDIR)
	$(INSTALL) -m 644 $(srcdir)/libipmiconsole.conf $(DESTDIR)$(LIBIPMICONSOLE_CONFIG_FILE_DEFAULT)

uninstall-local:
	$(RM) -f $(DESTDIR)$(LIBIPMICONSOLE_CONFIG_FILE_DEFAULT)



