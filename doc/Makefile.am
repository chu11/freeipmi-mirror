SUBDIRS = examples

EXTRA_DIST = BUGS freeipmi-hackers-intro.sxi ipmi-network-layout.fig ipmi-over-ts2000.texi 

#  build and install the .info pages
info_TEXINFOS = freeipmi.texi
freeipmi_TEXINFOS = authors.texi version.texi gpl.texi fdl.texi permissions.texi

#  install the man pages
dist_man_MANS = fish.1 bmc-config.1 bmc-info.1 fish.1 sel.1 sensors.1 \
	        bmc-watchdog.8 ipmipower.8 ipmipower.conf.5 \
		ipmiping.8 rmcpping.8

docdir   = $(datadir)/doc/$(PACKAGE)

export docdir

doc_DATA = $(EXTRA_DIST) freeipmi.texi authors.texi gpl.texi version.texi fdl.texi \
	   permissions.texi

ipmi-network-layout.pdf: ipmi-network-layout.fig Makefile
	fig2dev -L pdf $< ipmi-network-layout.pdf

ipmi-over-ts2000.pdf: ipmi-over-ts2000.texi ipmi-network-layout.pdf
	texi2pdf -c $<

pdf-local: ipmi-over-ts2000.pdf ipmi-network-layout.pdf

uninstall-extra:
	$(docdir)
