#  build and install the .info pages
info_TEXINFOS =  freeipmi-faq.texi

MANS_CPP = \
	freeipmi.hostmap.5 \
	freeipmi.7

$(MANS_CPP): $(MANS_CPP:%=%.pre)
	cpp -C -P -I. -x c $@.pre -o $@

dist_man_MANS = $(MANS_CPP)

CLEANFILES = $(MANS_CPP) *~
DISTCLEANFILES = $(MANS_CPP) $(MANS_CPP:%=%.pre) .deps/*.P

EXTRA_DIST = \
	freeipmi-coding.txt \
	freeipmi-hostrange.txt \
	freeipmi-libraries.txt \
	freeipmi-bugs-and-workarounds.txt

docdir = $(datadir)/doc/$(PACKAGE)

doc_DATA = $(EXTRA_DIST)



