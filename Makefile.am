SUBDIRS = doc libfreeipmi fish bmc-watchdog ipmipower ipmiping rmcpping

PACKAGE = @PACKAGE@
VERSION = @VERSION@
 
EXTRA = ChangeLog AUTHORS COPYING INSTALL NEWS README TODO COPYING.bmc-watchdog DISCLAIMER.bmc-watchdog COPYING.ipmipower DISCLAIMER.ipmipower COPYING.ipmiping DISCLAIMER.ipmiping COPYING.rmcpping DISCLAIMER.rmcpping
 
docdir   = $(datadir)/doc/$(PACKAGE)
doc_DATA = $(EXTRA)
 
CLEANFILES =
DISTCLEANFILES = .deps/*.P

uninstall-extra:
	$(docdir)

EXTRA_DIST = $(EXTRA) freeipmi.spec.in
