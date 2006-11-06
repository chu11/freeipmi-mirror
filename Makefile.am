SUBDIRS = doc libfreeipmi common bmc-watchdog ipmipower ipmi-raw ipmi-sel ipmi-sensors ipmi-locate bmc-info ipmiping rmcpping bmc-config ipmiconsole

PACKAGE = @PACKAGE@
VERSION = @VERSION@

EXTRA = ChangeLog ChangeLog.0 AUTHORS COPYING INSTALL NEWS README TODO THANKS COPYING.bmc-watchdog DISCLAIMER.bmc-watchdog COPYING.ipmipower DISCLAIMER.ipmipower COPYING.ipmiping DISCLAIMER.ipmiping COPYING.rmcpping DISCLAIMER.rmcpping COPYING.ipmiconsole DISCLAIMER.ipmiconsole

docdir   = $(datadir)/doc/$(PACKAGE)
doc_DATA = $(EXTRA)

CLEANFILES =
DISTCLEANFILES = .deps/*.P

uninstall-extra:
	$(docdir)

EXTRA_DIST = $(EXTRA) freeipmi.spec.in autogen.sh
