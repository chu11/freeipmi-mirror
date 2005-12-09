SUBDIRS = examples

EXTRA_DIST = BUGS freeipmi-hackers-intro.odp ipmi-network-layout.fig ipmi-over-ts2000.texi 

#  build and install the .info pages
info_TEXINFOS = freeipmi.texi
freeipmi_TEXINFOS = authors.texi gpl.texi fdl.texi permissions.texi

docdir   = $(datadir)/doc/$(PACKAGE)

export docdir

doc_DATA = $(EXTRA_DIST)

ipmi-network-layout.pdf: ipmi-network-layout.fig Makefile
	fig2dev -L pdf $< ipmi-network-layout.pdf

ipmi-over-ts2000.pdf: ipmi-over-ts2000.texi ipmi-network-layout.pdf
	texi2pdf -c $<

CLEANFILES = ipmi-network-layout.pdf ipmi-over-ts2000.pdf

pdf-local: ipmi-over-ts2000.pdf ipmi-network-layout.pdf

uninstall-extra:
	$(docdir)
