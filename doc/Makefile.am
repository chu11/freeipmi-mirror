SUBDIRS = examples

#  build and install the .info pages
info_TEXINFOS = freeipmi-ug.texi
freeipmi_ug_TEXINFOS = authors.texi version.texi

#  install the man pages
dist_man_MANS = fish.1 bmc-config.1 bmc-info.1 fish.1 sel.1 sensors.1 \
	        bmc-watchdog.8 ipmipower.8 ipmipower.conf.5 \
		ipmiping.8 rmcpping.8

EXTRA_DIST = BUGS

docdir   = $(datadir)/doc/$(PACKAGE)

doc_DATA = $(EXTRA_DIST)

uninstall-extra:
	$(docdir)
