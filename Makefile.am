SUBDIRS = doc libfreeipmi fish bmc-watchdog

PACKAGE = @PACKAGE@
VERSION = @VERSION@
 
EXTRA = ChangeLog AUTHORS NEWS README TODO COPYING.bmc-watchdog DISCLAIMER.bmc-watchdog
 
docdir   = $(datadir)/doc/$(PACKAGE)
doc_DATA = $(EXTRA)
 
CLEANFILES =
DISTCLEANFILES = .deps/*.P

uninstall-extra:
	$(docdir)

EXTRA_DIST = $(EXTRA)
