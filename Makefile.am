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
	bmc-watchdog \
	ipmi-chassis-config \
	ipmi-fru \
	ipmi-oem \
	ipmi-pef-config \
	ipmi-sensors \
	ipmi-sensors-config \
	ipmi-locate \
	ipmidetect \
	ipmidetectd \
	ipmipower \
	examples \
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

docdir   = $(datadir)/doc/$(PACKAGE)
doc_DATA = $(EXTRA)

uninstall-extra:
	$(docdir)

sbin_PROGRAMS = \
	bmc-config/src/bmc-config \
	bmc-device/src/bmc-device \
	bmc-info/src/bmc-info \
	ipmi-chassis/src/ipmi-chassis \
	ipmi-dcmi/src/ipmi-dcmi \
	ipmi-raw/src/ipmi-raw \
	ipmi-sel/src/ipmi-sel \
	ipmiconsole/src/ipmiconsole \
	ipmiping/src/ipmiping \
	rmcpping/src/rmcpping

TOOLCOMMON_LDADD = \
        $(top_builddir)/common/src/toolcommon/libtoolcommon.la \
        $(top_builddir)/common/src/toolhostrange/libtoolhostrange.la \
        $(top_builddir)/common/src/miscutil/libmiscutils.la \
        $(top_builddir)/common/src/parsecommon/libparsecommon.la \
        $(top_builddir)/common/src/portability/libfreeipmiportability.la \
        $(top_builddir)/libipmidetect/src/libipmidetect.la \
        $(top_builddir)/libfreeipmi/src/libfreeipmi.la

CONFIGTOOLCOMMON_LDADD = \
	$(TOOLCOMMON_LDADD) \
	$(top_builddir)/common/src/configtool/libconfigtoolcommon.la

PINGTOOLCOMMON_LDADD = \
	$(top_builddir)/common/src/pingtool/libpingtoolcommon.la \
	$(top_builddir)/common/src/debugutil/libdebugutil.la \
	$(top_builddir)/common/src/portability/libfreeipmiportability.la \
	$(top_builddir)/libfreeipmi/src/libfreeipmi.la

TOOLCOMMON_CPPFLAGS = \
        -I$(top_srcdir)/common/src/toolcommon \
        -I$(top_srcdir)/common/src/toolhostrange \
        -I$(top_srcdir)/common/src/miscutil \
        -I$(top_srcdir)/common/src/parsecommon \
        -I$(top_srcdir)/common/src/portability \
        -I$(top_srcdir)/libfreeipmi/include \
        -D_GNU_SOURCE \
        -D_REENTRANT 

CONFIGTOOLCOMMON_CPPFLAGS = \
	$(TOOLCOMMON_CPPFLAGS) \
	-I$(top_srcdir)/common/src/configtool

PINGTOOLCOMMON_CPPFLAGS = \
	-I$(top_srcdir)/common/src/pingtool \
	-I$(top_srcdir)/common/src/debugutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE

bmc_config_src_bmc_config_SOURCES = \
	bmc-config/src/bmc-config.h \
	bmc-config/src/bmc-config.c \
	bmc-config/src/bmc-config-argp.c \
	bmc-config/src/bmc-config-argp.h \
	bmc-config/src/bmc-config-channel-common.c \
	bmc-config/src/bmc-config-channel-common.h \
	bmc-config/src/bmc-config-lan-channel-section.c \
	bmc-config/src/bmc-config-lan-channel-section.h \
	bmc-config/src/bmc-config-lan-conf-section.c \
	bmc-config/src/bmc-config-lan-conf-section.h \
	bmc-config/src/bmc-config-lan-conf-auth-section.c \
	bmc-config/src/bmc-config-lan-conf-auth-section.h \
	bmc-config/src/bmc-config-lan-conf-security-keys-section.c \
	bmc-config/src/bmc-config-lan-conf-security-keys-section.h \
	bmc-config/src/bmc-config-lan-conf-misc-section.c \
	bmc-config/src/bmc-config-lan-conf-misc-section.h \
	bmc-config/src/bmc-config-lan-conf-user-security-section.c \
	bmc-config/src/bmc-config-lan-conf-user-security-section.h \
	bmc-config/src/bmc-config-map.c \
	bmc-config/src/bmc-config-map.h \
	bmc-config/src/bmc-config-misc-section.c \
	bmc-config/src/bmc-config-misc-section.h \
	bmc-config/src/bmc-config-rmcpplus-conf-privilege-section.c \
	bmc-config/src/bmc-config-rmcpplus-conf-privilege-section.h \
	bmc-config/src/bmc-config-serial-channel-section.c \
	bmc-config/src/bmc-config-serial-channel-section.h \
	bmc-config/src/bmc-config-serial-conf-section.c \
	bmc-config/src/bmc-config-serial-conf-section.h \
	bmc-config/src/bmc-config-pef-conf-section.c \
	bmc-config/src/bmc-config-pef-conf-section.h \
	bmc-config/src/bmc-config-sections.h \
	bmc-config/src/bmc-config-sections.c \
	bmc-config/src/bmc-config-sol-conf-section.c \
	bmc-config/src/bmc-config-sol-conf-section.h \
	bmc-config/src/bmc-config-user-sections.c \
	bmc-config/src/bmc-config-user-sections.h \
	bmc-config/src/bmc-config-utils.c \
	bmc-config/src/bmc-config-utils.h \
	bmc-config/src/bmc-config-validate.c \
	bmc-config/src/bmc-config-validate.h

bmc_config_src_bmc_config_LDADD = $(CONFIGTOOLCOMMON_LDADD)
bmc_config_src_bmc_config_CPPFLAGS = $(CONFIGTOOLCOMMON_CPPFLAGS)

bmc_device_src_bmc_device_SOURCES = \
        bmc-device/src/bmc-device.c \
        bmc-device/src/bmc-device.h \
        bmc-device/src/bmc-device-argp.c \
        bmc-device/src/bmc-device-argp.h

bmc_device_src_bmc_device_LDADD = $(TOOLCOMMON_LDADD)
bmc_device_src_bmc_device_CPPFLAGS = $(TOOLCOMMON_CPPFLAGS)

bmc_info_src_bmc_info_SOURCES = \
        bmc-info/src/bmc-info.c \
        bmc-info/src/bmc-info.h \
        bmc-info/src/bmc-info-argp.c \
        bmc-info/src/bmc-info-argp.h

bmc_info_src_bmc_info_LDADD = $(TOOLCOMMON_LDADD)
bmc_info_src_bmc_info_CPPFLAGS = $(TOOLCOMMON_CPPFLAGS)

ipmi_chassis_src_ipmi_chassis_SOURCES = \
        ipmi-chassis/src/ipmi-chassis.c \
        ipmi-chassis/src/ipmi-chassis.h \
        ipmi-chassis/src/ipmi-chassis-argp.c \
        ipmi-chassis/src/ipmi-chassis-argp.h

ipmi_chassis_src_ipmi_chassis_LDADD = $(TOOLCOMMON_LDADD)
ipmi_chassis_src_ipmi_chassis_CPPFLAGS = $(TOOLCOMMON_CPPFLAGS)

ipmi_dcmi_src_ipmi_dcmi_SOURCES = \
        ipmi-dcmi/src/ipmi-dcmi.c \
        ipmi-dcmi/src/ipmi-dcmi.h \
        ipmi-dcmi/src/ipmi-dcmi-argp.c \
        ipmi-dcmi/src/ipmi-dcmi-argp.h

ipmi_dcmi_src_ipmi_dcmi_LDADD = \
        $(top_builddir)/common/src/tooloem/libtooloem.la \
	$(TOOLCOMMON_LDADD)

ipmi_dcmi_src_ipmi_dcmi_CPPFLAGS = \
        -I$(top_srcdir)/common/src/tooloem \
	$(TOOLCOMMON_CPPFLAGS)

ipmi_raw_src_ipmi_raw_SOURCES = \
        ipmi-raw/src/ipmi-raw.c \
        ipmi-raw/src/ipmi-raw.h \
        ipmi-raw/src/ipmi-raw-argp.c \
        ipmi-raw/src/ipmi-raw-argp.h

ipmi_raw_src_ipmi_raw_LDADD = $(TOOLCOMMON_LDADD)
ipmi_raw_src_ipmi_raw_CPPFLAGS = $(TOOLCOMMON_CPPFLAGS)

ipmi_sel_src_ipmi_sel_SOURCES = \
        ipmi-sel/src/ipmi-sel.c \
        ipmi-sel/src/ipmi-sel.h \
        ipmi-sel/src/ipmi-sel-argp.c \
        ipmi-sel/src/ipmi-sel-argp.h

ipmi_sel_src_ipmi_sel_LDADD = \
        $(top_builddir)/common/src/tooloem/libtooloem.la \
        $(top_builddir)/common/src/toolsdr/libtoolsdr.la \
        $(top_builddir)/common/src/toolsensor/libtoolsensor.la \
	$(TOOLCOMMON_LDADD)

ipmi_sel_src_ipmi_sel_CPPFLAGS = \
        -I$(top_srcdir)/common/src/tooloem \
        -I$(top_srcdir)/common/src/toolsdr \
        -I$(top_srcdir)/common/src/toolsensor \
	$(TOOLCOMMON_CPPFLAGS)

ipmiconsole_src_ipmiconsole_SOURCES = \
        ipmiconsole/src/ipmiconsole.c \
        ipmiconsole/src/ipmiconsole._h \
        ipmiconsole/src/ipmiconsole-argp.c \
        ipmiconsole/src/ipmiconsole-argp.h

ipmiconsole_src_ipmiconsole_LDADD = \
        $(top_builddir)/common/src/toolcommon/libtoolcommon.la \
        $(top_builddir)/common/src/miscutil/libmiscutils.la \
        $(top_builddir)/common/src/parsecommon/libparsecommon.la \
        $(top_builddir)/common/src/portability/libfreeipmiportability.la \
        $(top_builddir)/libipmiconsole/src/libipmiconsole.la

ipmiconsole_src_ipmiconsole_CPPFLAGS = \
        -I$(top_srcdir)/common/src/toolcommon \
        -I$(top_srcdir)/common/src/miscutil \
        -I$(top_srcdir)/common/src/parsecommon \
        -I$(top_srcdir)/common/src/portability \
        -I$(top_srcdir)/libfreeipmi/include \
        -I$(top_srcdir)/libipmiconsole/src/ \
        -D_GNU_SOURCE \
        -D_REENTRANT

ipmiping_src_ipmiping_SOURCES = ipmiping/src/ipmiping.c
ipmiping_src_ipmiping_LDADD = $(PINGTOOLCOMMON_LDADD)
ipmiping_src_ipmiping_CPPFLAGS = $(PINGTOOLCOMMON_CPPFLAGS)

rmcpping_src_rmcpping_SOURCES = rmcpping/src/rmcpping.c
rmcpping_src_rmcpping_LDADD = $(PINGTOOLCOMMON_LDADD)
rmcpping_src_rmcpping_CPPFLAGS = $(PINGTOOLCOMMON_CPPFLAGS)

MANS_CPP = \
	bmc-config/bmc-config.8 \
	bmc-config/bmc-config.conf.5 \
	bmc-device/bmc-device.8 \
	bmc-info/bmc-info.8 \
	ipmi-chassis/ipmi-chassis.8 \
	ipmi-dcmi/ipmi-dcmi.8 \
	ipmi-raw/ipmi-raw.8 \
	ipmi-sel/ipmi-sel.8 \
	ipmiconsole/ipmiconsole.8 \
	ipmiping/ipmiping.8 \
	rmcpping/rmcpping.8

$(MANS_CPP): $(MANS_CPP:%=%.pre)
	$(CPP) -C -P -I. $@.pre  $@

dist_man_MANS = \
	$(MANS_CPP) \
	ipmiconsole/ipmi-console.8 \
	ipmiconsole/ipmiconsole.conf.5 \
	ipmiping/ipmi-ping.8 \
	rmcpping/rmcp-ping.8

CLEANFILES = $(MANS_CPP) *~

DISTCLEANFILES = $(MANS_CPP) $(MANS_CPP:%=%.pre) .deps/*.P

EXTRA_DIST = \
	$(EXTRA) \
	$(MANS_CPP) \
	freeipmi.spec
