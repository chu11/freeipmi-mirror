SUBDIRS = examples

# Additional documentation
EXTRA_DIST = BUGS ipmi-over-ts2000.texi ipmi-network-layout.fig ipmi-network-layout.xcf freeipmi-hackers-intro.sxi

#  build and install the .info pages
info_TEXINFOS = freeipmi.texi
freeipmi_ug_TEXINFOS = authors.texi version.texi gpl.texi fdl.texi permissions.texi

#  install the man pages
dist_man_MANS = fish.1 bmc-config.1 bmc-info.1 fish.1 sel.1 sensors.1 \
	        bmc-watchdog.8 ipmipower.8 ipmipower.conf.5 \
		ipmiping.8 rmcpping.8

docdir   = $(datadir)/doc/$(PACKAGE)

doc_DATA = $(EXTRA_DIST)

uninstall-extra:
	$(docdir)

