if FISH
  MAYBE_FISH = fish
endif

SUBDIRS = doc libfreeipmi bmc-watchdog ipmipower ipmi-raw ipmi-locate bmc-info ipmiping rmcpping garpd $(MAYBE_FISH)

PACKAGE = @PACKAGE@
VERSION = @VERSION@

EXTRA = ChangeLog AUTHORS COPYING INSTALL NEWS README TODO THANKS COPYING.bmc-watchdog DISCLAIMER.bmc-watchdog COPYING.ipmipower DISCLAIMER.ipmipower COPYING.ipmiping DISCLAIMER.ipmiping COPYING.rmcpping DISCLAIMER.rmcpping

docdir   = $(datadir)/doc/$(PACKAGE)
doc_DATA = $(EXTRA)

CLEANFILES =
DISTCLEANFILES = .deps/*.P

uninstall-extra:
	$(docdir)

EXTRA_DIST = $(EXTRA) freeipmi.spec.in autogen.sh
