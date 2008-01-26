SUBDIRS = doc libfreeipmi common bmc-watchdog ipmipower ipmi-fru ipmi-raw ipmi-sel ipmi-sensors ipmi-locate bmc-info ipmiping rmcpping bmc-config pef-config ipmiconsole ipmimonitoring ipmi-chassis ipmidetect ipmi-sensors-config

PACKAGE = @PACKAGE@
VERSION = @VERSION@

EXTRA = \
	ChangeLog \
	ChangeLog.0 \
	AUTHORS \
	COPYING \
	INSTALL \
	NEWS \
	README \
	TODO \
	COPYING.bmc-watchdog \
	COPYING.ipmipower \
	COPYING.ipmiping \
	COPYING.rmcpping \
	COPYING.ipmiconsole \
	COPYING.ipmimonitoring \
	COPYING.pstdout \
	COPYING.ipmidetect \
	COPYING.ipmi-fru \
	COPYING.ZRESEARCH \
	DISCLAIMER.bmc-watchdog \
	DISCLAIMER.ipmipower \
	DISCLAIMER.ipmiping \
	DISCLAIMER.rmcpping \
	DISCLAIMER.ipmiconsole \
	DISCLAIMER.ipmimonitoring \
	DISCLAIMER.pstdout \
	DISCLAIMER.ipmidetect \
	DISCLAIMER.ipmi-fru \
	DISCLAIMER.bmc-watchdog.UC \
	DISCLAIMER.ipmipower.UC \
	DISCLAIMER.ipmiping.UC \
	DISCLAIMER.rmcpping.UC \
	DISCLAIMER.ipmiconsole.UC \
	DISCLAIMER.ipmimonitoring.UC \
	DISCLAIMER.pstdout.UC \
	DISCLAIMER.ipmidetect.UC \
	DISCLAIMER.ipmi-fru.UC

docdir   = $(datadir)/doc/$(PACKAGE)
doc_DATA = $(EXTRA)

CLEANFILES =
DISTCLEANFILES = .deps/*.P

uninstall-extra:
	$(docdir)

EXTRA_DIST = $(EXTRA) freeipmi.spec freeipmi.spec.in autogen.sh
