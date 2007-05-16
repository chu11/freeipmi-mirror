SUBDIRS = doc libfreeipmi common bmc-watchdog ipmipower ipmi-raw ipmi-sel ipmi-sensors ipmi-locate bmc-info ipmiping rmcpping bmc-config ipmi-pef ipmiconsole ipmimonitoring ipmi-chassis ipmidetect

PACKAGE = @PACKAGE@
VERSION = @VERSION@

EXTRA = ChangeLog ChangeLog.0 AUTHORS COPYING INSTALL NEWS README TODO COPYING.bmc-watchdog DISCLAIMER.bmc-watchdog COPYING.ipmipower DISCLAIMER.ipmipower COPYING.ipmiping DISCLAIMER.ipmiping COPYING.rmcpping DISCLAIMER.rmcpping COPYING.ipmiconsole DISCLAIMER.ipmiconsole COPYING.ipmimonitoring DISCLAIMER.ipmimonitoring COPYING.pstdout DISCLAIMER.pstdout COPYING.ipmidetect DISCLAIMER.ipmidetect COPYING.ZRESEARCH

docdir   = $(datadir)/doc/$(PACKAGE)
doc_DATA = $(EXTRA)

CLEANFILES =
DISTCLEANFILES = .deps/*.P

uninstall-extra:
	$(docdir)

EXTRA_DIST = $(EXTRA) freeipmi.spec freeipmi.spec.in autogen.sh
