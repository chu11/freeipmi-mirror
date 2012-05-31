
# include directory for aclocal
ACLOCAL_AMFLAGS = -I config

SUBDIRS = \
	doc \
	etc \
	common \
	libfreeipmi \
	libipmiconsole \
	libipmimonitoring \
	libipmidetect \
	man \
	bmc-config \
	bmc-info \
	bmc-device \
	bmc-watchdog \
	ipmi-chassis \
	ipmi-chassis-config \
	ipmi-dcmi \
	ipmi-fru \
	ipmi-oem \
	ipmi-pef-config \
	ipmi-pet \
	ipmi-raw \
	ipmi-sel \
	ipmi-sensors \
	ipmi-sensors-config \
	ipmi-locate \
	ipmiconsole \
	ipmidetect \
	ipmidetectd \
	ipmiping \
	ipmipower \
	rmcpping \
	contrib

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
	README.argp \
	README.build \
	README.openipmi \
	TODO \
	COPYING.bmc-watchdog \
	COPYING.ipmipower \
	COPYING.ipmiping \
	COPYING.ipmiconsole \
	COPYING.ipmimonitoring \
	COPYING.pstdout \
	COPYING.ipmidetect \
	COPYING.ipmi-fru \
	COPYING.ipmi-dcmi \
	COPYING.sunbmc \
	COPYING.ZRESEARCH \
	DISCLAIMER.bmc-watchdog \
	DISCLAIMER.ipmipower \
	DISCLAIMER.ipmiping \
	DISCLAIMER.ipmiconsole \
	DISCLAIMER.ipmimonitoring \
	DISCLAIMER.pstdout \
	DISCLAIMER.ipmidetect \
	DISCLAIMER.ipmi-fru \
	DISCLAIMER.ipmi-dcmi \
	DISCLAIMER.bmc-watchdog.UC \
	DISCLAIMER.ipmipower.UC \
	DISCLAIMER.ipmiping.UC \
	DISCLAIMER.ipmiconsole.UC \
	DISCLAIMER.ipmimonitoring.UC \
	DISCLAIMER.pstdout.UC \
	DISCLAIMER.ipmidetect.UC \
	DISCLAIMER.ipmi-fru.UC

doc_DATA = $(EXTRA)

CLEANFILES =

DISTCLEANFILES = .deps/*.P

uninstall-extra:
	$(docdir)

EXTRA_DIST = $(EXTRA) freeipmi.spec

