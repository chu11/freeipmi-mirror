# include directory for aclocal
ACLOCAL_AMFLAGS = -I config

PACKAGE = @PACKAGE@
VERSION = @VERSION@

SUBDIRS = \
	doc \
	libfreeipmi \
	examples \
	contrib

FREEIPMI_DOCS = \
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
doc_DATA = $(FREEIPMI_DOCS)

uninstall-extra:
	$(docdir)

sbin_PROGRAMS = \
	bmc-config/src/bmc-config \
	bmc-device/src/bmc-device \
	bmc-info/src/bmc-info \
	bmc-watchdog/src/bmc-watchdog \
	ipmi-chassis/src/ipmi-chassis \
	ipmi-chassis-config/src/ipmi-chassis-config \
	ipmi-dcmi/src/ipmi-dcmi \
	ipmi-fru/src/ipmi-fru \
	ipmi-locate/src/ipmi-locate \
	ipmi-oem/src/ipmi-oem \
	ipmi-pef-config/src/ipmi-pef-config \
	ipmipower/src/ipmipower \
	ipmi-raw/src/ipmi-raw \
	ipmi-sel/src/ipmi-sel \
	ipmi-sensors/src/ipmi-sensors \
	ipmi-sensors-config/src/ipmi-sensors-config \
	ipmiconsole/src/ipmiconsole \
	ipmidetect/src/ipmidetect \
	ipmidetectd/src/ipmidetectd \
	ipmiping/src/ipmiping \
	rmcpping/src/rmcpping

BASE_CPPFLAGS = \
        -I$(top_srcdir)/common/src/miscutil \
        -I$(top_srcdir)/common/src/portability \
        -I$(top_srcdir)/libfreeipmi/include \
        -D_GNU_SOURCE \
        -D_REENTRANT 

TOOLBASE_CPPFLAGS = \
        -I$(top_srcdir)/common/src/toolcommon \
        -I$(top_srcdir)/common/src/parsecommon \
	$(BASE_CPPFLAGS)

TOOLCOMMON_CPPFLAGS = \
	-I$(top_srcdir)/common/src/toolhostrange \
	$(TOOLBASE_CPPFLAGS)

CONFIGTOOLCOMMON_CPPFLAGS = \
	-I$(top_srcdir)/common/src/configtool \
	$(TOOLCOMMON_CPPFLAGS)

PINGTOOLCOMMON_CPPFLAGS = \
	-I$(top_srcdir)/common/src/pingtool \
	-I$(top_srcdir)/common/src/debugutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE

BASE_LDADD = \
	$(top_builddir)/common/src/miscutil/libmiscutil.la \
	$(top_builddir)/common/src/parsecommon/libparsecommon.la \
	$(top_builddir)/common/src/portability/libportability.la \
	$(top_builddir)/libfreeipmi/src/libfreeipmi.la

TOOLBASE_LDADD = \
	$(top_builddir)/common/src/toolcommon/libtoolcommon.la \
	$(BASE_LDADD)

TOOLCOMMON_LDADD = \
	$(top_builddir)/common/src/toolhostrange/libtoolhostrange.la \
	$(top_builddir)/libipmidetect/src/libipmidetect.la \
	$(TOOLBASE_LDADD)

CONFIGTOOLCOMMON_LDADD = \
	$(top_builddir)/common/src/configtool/libconfigtoolcommon.la \
	$(TOOLCOMMON_LDADD)

PINGTOOLCOMMON_LDADD = \
	$(top_builddir)/common/src/pingtool/libpingtoolcommon.la \
	$(top_builddir)/common/src/debugutil/libdebugutil.la \
	$(top_builddir)/common/src/portability/libportability.la \
	$(top_builddir)/libfreeipmi/src/libfreeipmi.la

bmc_config_src_bmc_config_CPPFLAGS = \
	$(CONFIGTOOLCOMMON_CPPFLAGS)

bmc_config_src_bmc_config_LDADD = \
	$(CONFIGTOOLCOMMON_LDADD)

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

bmc_device_src_bmc_device_CPPFLAGS = \
	$(TOOLCOMMON_CPPFLAGS)

bmc_device_src_bmc_device_LDADD = \
	$(TOOLCOMMON_LDADD)

bmc_device_src_bmc_device_SOURCES = \
	bmc-device/src/bmc-device.c \
	bmc-device/src/bmc-device.h \
	bmc-device/src/bmc-device-argp.c \
	bmc-device/src/bmc-device-argp.h

bmc_info_src_bmc_info_CPPFLAGS = \
	$(TOOLCOMMON_CPPFLAGS)

bmc_info_src_bmc_info_LDADD = \
	$(TOOLCOMMON_LDADD)

bmc_info_src_bmc_info_SOURCES = \
	bmc-info/src/bmc-info.c \
	bmc-info/src/bmc-info.h \
	bmc-info/src/bmc-info-argp.c \
	bmc-info/src/bmc-info-argp.h

bmc_watchdog_src_bmc_watchdog_CPPFLAGS = \
	-I$(top_srcdir)/common/src/debugutil \
	-DBMC_WATCHDOG_LOCALSTATEDIR='"$(localstatedir)"' \
	$(TOOLBASE_CPPFLAGS)

bmc_watchdog_src_bmc_watchdog_LDADD = \
	$(top_builddir)/common/src/debugutil/libdebugutil.la \
	$(TOOLBASE_LDADD)

bmc_watchdog_src_bmc_watchdog_SOURCES = \
	bmc-watchdog/src/bmc-watchdog.c \
	bmc-watchdog/src/bmc-watchdog.h \
	bmc-watchdog/src/bmc-watchdog-argp.c \
	bmc-watchdog/src/bmc-watchdog-argp.h

ipmi_chassis_src_ipmi_chassis_CPPFLAGS = \
	$(TOOLCOMMON_CPPFLAGS)

ipmi_chassis_src_ipmi_chassis_LDADD = \
	$(TOOLCOMMON_LDADD)

ipmi_chassis_src_ipmi_chassis_SOURCES = \
	ipmi-chassis/src/ipmi-chassis.c \
	ipmi-chassis/src/ipmi-chassis.h \
	ipmi-chassis/src/ipmi-chassis-argp.c \
	ipmi-chassis/src/ipmi-chassis-argp.h

ipmi_chassis_config_src_ipmi_chassis_config_CPPFLAGS = \
	$(CONFIGTOOLCOMMON_CPPFLAGS)

ipmi_chassis_config_src_ipmi_chassis_config_LDADD = \
	$(CONFIGTOOLCOMMON_LDADD)

ipmi_chassis_config_src_ipmi_chassis_config_SOURCES = \
	ipmi-chassis-config/src/ipmi-chassis-config.c \
	ipmi-chassis-config/src/ipmi-chassis-config.h \
	ipmi-chassis-config/src/ipmi-chassis-config-argp.c \
	ipmi-chassis-config/src/ipmi-chassis-config-argp.h \
	ipmi-chassis-config/src/ipmi-chassis-config-boot-flags.c \
	ipmi-chassis-config/src/ipmi-chassis-config-boot-flags.h \
	ipmi-chassis-config/src/ipmi-chassis-config-front-panel-buttons.c \
	ipmi-chassis-config/src/ipmi-chassis-config-front-panel-buttons.h \
	ipmi-chassis-config/src/ipmi-chassis-config-map.c \
	ipmi-chassis-config/src/ipmi-chassis-config-map.h \
	ipmi-chassis-config/src/ipmi-chassis-config-power-conf.c \
	ipmi-chassis-config/src/ipmi-chassis-config-power-conf.h \
	ipmi-chassis-config/src/ipmi-chassis-config-sections.c \
	ipmi-chassis-config/src/ipmi-chassis-config-sections.h \
	ipmi-chassis-config/src/ipmi-chassis-config-validate.c \
	ipmi-chassis-config/src/ipmi-chassis-config-validate.h

ipmi_dcmi_src_ipmi_dcmi_CPPFLAGS = \
	-I$(top_srcdir)/common/src/tooloem \
	$(TOOLCOMMON_CPPFLAGS)

ipmi_dcmi_src_ipmi_dcmi_LDADD = \
	$(top_builddir)/common/src/tooloem/libtooloem.la \
	$(TOOLCOMMON_LDADD)

ipmi_dcmi_src_ipmi_dcmi_SOURCES = \
	ipmi-dcmi/src/ipmi-dcmi.c \
	ipmi-dcmi/src/ipmi-dcmi.h \
	ipmi-dcmi/src/ipmi-dcmi-argp.c \
	ipmi-dcmi/src/ipmi-dcmi-argp.h

ipmi_fru_src_ipmi_fru_CPPFLAGS = \
	-I$(top_srcdir)/common/src/tooloem \
	-I$(top_srcdir)/common/src/toolsdr \
	$(TOOLCOMMON_CPPFLAGS)

ipmi_fru_src_ipmi_fru_LDADD = \
	$(top_builddir)/common/src/tooloem/libtooloem.la \
	$(top_builddir)/common/src/toolsdr/libtoolsdr.la \
	$(TOOLCOMMON_LDADD)

ipmi_fru_src_ipmi_fru_SOURCES = \
	ipmi-fru/src/ipmi-fru.c \
	ipmi-fru/src/ipmi-fru.h \
	ipmi-fru/src/ipmi-fru-argp.c \
	ipmi-fru/src/ipmi-fru-argp.h \
	ipmi-fru/src/ipmi-fru-output.c \
	ipmi-fru/src/ipmi-fru-output.h

ipmi_locate_src_ipmi_locate_CPPFLAGS = \
	$(TOOLBASE_CPPFLAGS)

ipmi_locate_src_ipmi_locate_LDADD = \
	$(TOOLBASE_LDADD)

ipmi_locate_src_ipmi_locate_SOURCES = \
	ipmi-locate/src/ipmi-locate.c \
	ipmi-locate/src/ipmi-locate.h \
	ipmi-locate/src/ipmi-locate-argp.c \
	ipmi-locate/src/ipmi-locate-argp.h

ipmi_oem_src_ipmi_oem_CPPFLAGS = \
	-I$(top_srcdir)/common/src/tooloem \
	-I$(top_srcdir)/common/src/toolsdr \
	-I$(top_srcdir)/common/src/toolsensor \
	$(TOOLCOMMON_CPPFLAGS)

ipmi_oem_src_ipmi_oem_LDADD = \
	$(top_builddir)/common/src/tooloem/libtooloem.la \
	$(top_builddir)/common/src/toolsdr/libtoolsdr.la \
	$(top_builddir)/common/src/toolsensor/libtoolsensor.la \
	$(TOOLCOMMON_LDADD)

ipmi_oem_src_ipmi_oem_SOURCES = \
	ipmi-oem/src/ipmi-oem.c \
	ipmi-oem/src/ipmi-oem.h \
	ipmi-oem/src/ipmi-oem-argp.c \
	ipmi-oem/src/ipmi-oem-argp.h \
	ipmi-oem/src/ipmi-oem-common.c \
	ipmi-oem/src/ipmi-oem-common.h \
	ipmi-oem/src/ipmi-oem-dell.c \
	ipmi-oem/src/ipmi-oem-dell.h \
	ipmi-oem/src/ipmi-oem-fujitsu.c \
	ipmi-oem/src/ipmi-oem-fujitsu.h \
	ipmi-oem/src/ipmi-oem-ibm.c \
	ipmi-oem/src/ipmi-oem-ibm.h \
	ipmi-oem/src/ipmi-oem-intel.c \
	ipmi-oem/src/ipmi-oem-intel.h \
	ipmi-oem/src/ipmi-oem-intelnm.c \
	ipmi-oem/src/ipmi-oem-intelnm.h \
	ipmi-oem/src/ipmi-oem-inventec.c \
	ipmi-oem/src/ipmi-oem-inventec.h \
	ipmi-oem/src/ipmi-oem-sun.c \
	ipmi-oem/src/ipmi-oem-sun.h \
	ipmi-oem/src/ipmi-oem-quanta.c \
	ipmi-oem/src/ipmi-oem-quanta.h \
	ipmi-oem/src/ipmi-oem-supermicro.c \
	ipmi-oem/src/ipmi-oem-supermicro.h

ipmi_pef_config_src_ipmi_pef_config_CPPFLAGS = \
	$(CONFIGTOOLCOMMON_CPPFLAGS)

ipmi_pef_config_src_ipmi_pef_config_LDADD = \
	$(CONFIGTOOLCOMMON_LDADD)

ipmi_pef_config_src_ipmi_pef_config_SOURCES = \
	ipmi-pef-config/src/ipmi-pef-config.c \
	ipmi-pef-config/src/ipmi-pef-config.h \
	ipmi-pef-config/src/ipmi-pef-config-alert-policy-table.c \
	ipmi-pef-config/src/ipmi-pef-config-alert-policy-table.h \
	ipmi-pef-config/src/ipmi-pef-config-alert-string.c \
	ipmi-pef-config/src/ipmi-pef-config-alert-string.h \
	ipmi-pef-config/src/ipmi-pef-config-argp.c \
	ipmi-pef-config/src/ipmi-pef-config-argp.h \
	ipmi-pef-config/src/ipmi-pef-config-community-string.c \
	ipmi-pef-config/src/ipmi-pef-config-community-string.h \
	ipmi-pef-config/src/ipmi-pef-config-event-filter-table.c \
	ipmi-pef-config/src/ipmi-pef-config-event-filter-table.h \
	ipmi-pef-config/src/ipmi-pef-config-info.c \
	ipmi-pef-config/src/ipmi-pef-config-info.h \
	ipmi-pef-config/src/ipmi-pef-config-lan-alert-destination.c \
	ipmi-pef-config/src/ipmi-pef-config-lan-alert-destination.h \
	ipmi-pef-config/src/ipmi-pef-config-map.c \
	ipmi-pef-config/src/ipmi-pef-config-map.h \
	ipmi-pef-config/src/ipmi-pef-config-pef-conf-section.c \
	ipmi-pef-config/src/ipmi-pef-config-pef-conf-section.h \
	ipmi-pef-config/src/ipmi-pef-config-sections.c \
	ipmi-pef-config/src/ipmi-pef-config-sections.h \
	ipmi-pef-config/src/ipmi-pef-config-validate.c \
	ipmi-pef-config/src/ipmi-pef-config-validate.h \
	ipmi-pef-config/src/ipmi-pef-config-utils.c \
	ipmi-pef-config/src/ipmi-pef-config-utils.h

ipmi_raw_src_ipmi_raw_CPPFLAGS = \
	$(TOOLCOMMON_CPPFLAGS)

ipmi_raw_src_ipmi_raw_LDADD = \
	$(TOOLCOMMON_LDADD)

ipmi_raw_src_ipmi_raw_SOURCES = \
	ipmi-raw/src/ipmi-raw.c \
	ipmi-raw/src/ipmi-raw.h \
	ipmi-raw/src/ipmi-raw-argp.c \
	ipmi-raw/src/ipmi-raw-argp.h

ipmi_sel_src_ipmi_sel_CPPFLAGS = \
	-I$(top_srcdir)/common/src/tooloem \
	-I$(top_srcdir)/common/src/toolsdr \
	-I$(top_srcdir)/common/src/toolsensor \
	$(TOOLCOMMON_CPPFLAGS)

ipmi_sel_src_ipmi_sel_LDADD = \
	$(top_builddir)/common/src/tooloem/libtooloem.la \
	$(top_builddir)/common/src/toolsdr/libtoolsdr.la \
	$(top_builddir)/common/src/toolsensor/libtoolsensor.la \
	$(TOOLCOMMON_LDADD)

ipmi_sel_src_ipmi_sel_SOURCES = \
	ipmi-sel/src/ipmi-sel.c \
	ipmi-sel/src/ipmi-sel.h \
	ipmi-sel/src/ipmi-sel-argp.c \
	ipmi-sel/src/ipmi-sel-argp.h

ipmi_sensors_src_ipmi_sensors_CPPFLAGS = \
	-I$(top_srcdir)/common/src/tooloem \
	-I$(top_srcdir)/common/src/toolsdr \
	-I$(top_srcdir)/common/src/toolsensor \
	$(TOOLCOMMON_CPPFLAGS)

ipmi_sensors_src_ipmi_sensors_LDADD = \
	$(top_builddir)/common/src/tooloem/libtooloem.la \
	$(top_builddir)/common/src/toolsdr/libtoolsdr.la \
	$(top_builddir)/common/src/toolsensor/libtoolsensor.la \
	$(TOOLCOMMON_LDADD)

ipmi_sensors_src_ipmi_sensors_SOURCES = \
	ipmi-sensors/src/ipmi-sensors.c \
	ipmi-sensors/src/ipmi-sensors.h \
	ipmi-sensors/src/ipmi-sensors-argp.c \
	ipmi-sensors/src/ipmi-sensors-argp.h \
	ipmi-sensors/src/ipmi-sensors-detailed-output.c \
	ipmi-sensors/src/ipmi-sensors-detailed-output.h \
	ipmi-sensors/src/ipmi-sensors-oem-intel-node-manager.c \
	ipmi-sensors/src/ipmi-sensors-oem-intel-node-manager.h \
	ipmi-sensors/src/ipmi-sensors-output-common.c \
	ipmi-sensors/src/ipmi-sensors-output-common.h \
	ipmi-sensors/src/ipmi-sensors-simple-output.c \
	ipmi-sensors/src/ipmi-sensors-simple-output.h

ipmi_sensors_config_src_ipmi_sensors_config_CPPFLAGS = \
	-I$(top_srcdir)/common/src/toolsdr \
	$(CONFIGTOOLCOMMON_CPPFLAGS)

ipmi_sensors_config_src_ipmi_sensors_config_LDADD = \
	$(top_builddir)/common/src/toolsdr/libtoolsdr.la \
	$(CONFIGTOOLCOMMON_LDADD)

ipmi_sensors_config_src_ipmi_sensors_config_SOURCES = \
	ipmi-sensors-config/src/ipmi-sensors-config.c \
	ipmi-sensors-config/src/ipmi-sensors-config.h \
	ipmi-sensors-config/src/ipmi-sensors-config-argp.c \
	ipmi-sensors-config/src/ipmi-sensors-config-argp.h \
	ipmi-sensors-config/src/ipmi-sensors-config-discrete-section.c \
	ipmi-sensors-config/src/ipmi-sensors-config-discrete-section.h \
	ipmi-sensors-config/src/ipmi-sensors-config-threshold-section.c \
	ipmi-sensors-config/src/ipmi-sensors-config-threshold-section.h \
	ipmi-sensors-config/src/ipmi-sensors-config-sections.c \
	ipmi-sensors-config/src/ipmi-sensors-config-sections.h \
	ipmi-sensors-config/src/ipmi-sensors-config-sensor-event-enable-common.c \
	ipmi-sensors-config/src/ipmi-sensors-config-sensor-event-enable-common.h \
	ipmi-sensors-config/src/ipmi-sensors-config-utils.c \
	ipmi-sensors-config/src/ipmi-sensors-config-utils.h

ipmiconsole_src_ipmiconsole_CPPFLAGS = \
	-I$(top_srcdir)/libipmiconsole/src/ \
	$(TOOLBASE_CPPFLAGS)

ipmiconsole_src_ipmiconsole_LDADD = \
	$(top_builddir)/libipmiconsole/src/libipmiconsole.la \
	$(TOOLBASE_LDADD)

ipmiconsole_src_ipmiconsole_SOURCES = \
	ipmiconsole/src/ipmiconsole.c \
	ipmiconsole/src/ipmiconsole_.h \
	ipmiconsole/src/ipmiconsole-argp.c \
	ipmiconsole/src/ipmiconsole-argp.h

ipmidetect_src_ipmidetect_CPPFLAGS = \
	-I$(top_srcdir)/libipmidetect/src/ \
	$(BASE_CPPFLAGS)

ipmidetect_src_ipmidetect_LDADD = \
	$(top_builddir)/libipmidetect/src/libipmidetect.la \
	$(BASE_LDADD)

ipmidetect_src_ipmidetect_SOURCES = \
	ipmidetect/src/ipmidetect.c

ipmidetectd_src_ipmidetectd_CPPFLAGS = \
	$(BASE_CPPFLAGS)

ipmidetectd_src_ipmidetectd_LDADD = \
	$(BASE_LDADD)

ipmidetectd_src_ipmidetectd_SOURCES = \
	ipmidetectd/src/debug.c \
	ipmidetectd/src/debug.h \
	ipmidetectd/src/ipmidetectd.c \
	ipmidetectd/src/ipmidetectd.h \
	ipmidetectd/src/ipmidetectd_config.c \
	ipmidetectd/src/ipmidetectd_config.h \
	ipmidetectd/src/ipmidetectd_loop.c \
	ipmidetectd/src/ipmidetectd_loop.h

ipmiping_src_ipmiping_CPPFLAGS = \
	$(PINGTOOLCOMMON_CPPFLAGS)

ipmiping_src_ipmiping_LDADD = \
	$(PINGTOOLCOMMON_LDADD)

ipmiping_src_ipmiping_SOURCES = \
	ipmiping/src/ipmiping.c

ipmipower_src_ipmipower_CPPFLAGS = \
	-I$(top_srcdir)/common/src/debugutil \
	$(TOOLCOMMON_CPPFLAGS)

ipmipower_src_ipmipower_LDADD = \
	$(top_builddir)/common/src/debugutil/libdebugutil.la \
	$(TOOLCOMMON_LDADD)

ipmipower_src_ipmipower_SOURCES = \
	ipmipower/src/argv.c \
	ipmipower/src/argv.h \
	ipmipower/src/ipmipower.c \
	ipmipower/src/ipmipower.h \
	ipmipower/src/ipmipower_argp.c \
	ipmipower/src/ipmipower_argp.h \
	ipmipower/src/ipmipower_check.c \
	ipmipower/src/ipmipower_check.h \
	ipmipower/src/ipmipower_connection.c \
	ipmipower/src/ipmipower_connection.h \
	ipmipower/src/ipmipower_error.c \
	ipmipower/src/ipmipower_error.h \
	ipmipower/src/ipmipower_output.c \
	ipmipower/src/ipmipower_output.h \
	ipmipower/src/ipmipower_packet.c \
	ipmipower/src/ipmipower_packet.h \
	ipmipower/src/ipmipower_ping.c \
	ipmipower/src/ipmipower_ping.h \
	ipmipower/src/ipmipower_powercmd.c \
	ipmipower/src/ipmipower_powercmd.h \
	ipmipower/src/ipmipower_prompt.c \
	ipmipower/src/ipmipower_prompt.h \
	ipmipower/src/ipmipower_util.c \
	ipmipower/src/ipmipower_util.h
 
rmcpping_src_rmcpping_CPPFLAGS = \
	$(PINGTOOLCOMMON_CPPFLAGS)

rmcpping_src_rmcpping_LDADD = \
	$(PINGTOOLCOMMON_LDADD)

rmcpping_src_rmcpping_SOURCES = \
	rmcpping/src/rmcpping.c

sbin_SCRIPTS = \
	ipmi-sensors/src/ipmimonitoring

ipmi-sensors/src/ipmimonitoring: ipmi-sensors/src/ipmimonitoring.in
	sed -e 's:@IPMIMONITORINGSBINDIR@:$(sbindir):' $< > $@
	chmod +x ipmi-sensors/src/ipmimonitoring

if WITH_GNU_LD
LIBIPMICONSOLE_VERSION_SCRIPT = $(top_srcdir)/libipmiconsole/src/ipmiconsole.map
LIBIPMICONSOLE_OTHER_FLAGS = -Wl,--version-script=$(LIBIPMICONSOLE_VERSION_SCRIPT)
LIBIPMIDETECT_VERSION_SCRIPT = $(top_srcdir)/libipmidetect/src/ipmidetect.map
LIBIPMIDETECT_OTHER_FLAGS = -Wl,--version-script=$(LIBIPMIDETECT_VERSION_SCRIPT)
LIBIPMIMONITORING_VERSION_SCRIPT = $(top_srcdir)/libipmimonitoring/src/ipmimonitoring.map
LIBIPMIMONITORING_OTHER_FLAGS = -Wl,--version-script=$(LIBIPMIMONITORING_VERSION_SCRIPT)
endif

if WITH_PKG_CONFIG
pkgconfig_DATA = \
	libipmiconsole/libipmiconsole.pc \
	libipmidetect/libipmidetect.pc \
	libipmimonitoring/libipmimonitoring.pc
endif

include_HEADERS = \
	libipmiconsole/src/ipmiconsole.h \
	libipmidetect/src/ipmidetect.h \
	libipmimonitoring/src/ipmi_monitoring.h \
	libipmimonitoring/src/ipmi_monitoring_bitmasks.h \
	libipmimonitoring/src/ipmi_monitoring_offsets.h

lib_LTLIBRARIES = \
	libipmiconsole/src/libipmiconsole.la \
	libipmidetect/src/libipmidetect.la \
	libipmimonitoring/src/libipmimonitoring.la

libipmiconsole_src_libipmiconsole_la_CFLAGS = \
	$(PTHREAD_CFLAGS)

libipmiconsole_src_libipmiconsole_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/debugutil \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/parsecommon \
	-I$(top_srcdir)/common/src/portability \
        -I$(top_srcdir)/libfreeipmi/include \
	-DWITH_PTHREADS=1 \
	-D_GNU_SOURCE \
	-D_REENTRANT

libipmiconsole_src_libipmiconsole_la_LDFLAGS = \
	$(PTHREAD_LIBS) \
	-version-info @LIBIPMICONSOLE_VERSION_INFO@ \
	$(LIBIPMICONSOLE_OTHER_FLAGS)

libipmiconsole_src_libipmiconsole_la_LIBADD = \
	$(top_builddir)/common/src/debugutil/libdebugutil.la \
	$(top_builddir)/common/src/miscutil/libmiscutil.la \
	$(top_builddir)/common/src/parsecommon/libparsecommon.la \
	$(top_builddir)/common/src/portability/libportability.la \
	$(top_builddir)/libfreeipmi/src/libfreeipmi.la

libipmiconsole_src_libipmiconsole_la_SOURCES = \
	libipmiconsole/src/ipmiconsole.c \
	libipmiconsole/src/ipmiconsole_checks.c \
	libipmiconsole/src/ipmiconsole_checks.h \
	libipmiconsole/src/ipmiconsole_ctx.c \
	libipmiconsole/src/ipmiconsole_ctx.h \
	libipmiconsole/src/ipmiconsole_debug.c \
	libipmiconsole/src/ipmiconsole_debug.h \
	libipmiconsole/src/ipmiconsole_defs.h \
	libipmiconsole/src/ipmiconsole_garbage_collector.c \
	libipmiconsole/src/ipmiconsole_garbage_collector.h \
	libipmiconsole/src/ipmiconsole_engine.c \
	libipmiconsole/src/ipmiconsole_engine.h \
	libipmiconsole/src/ipmiconsole_packet.c \
	libipmiconsole/src/ipmiconsole_packet.h \
	libipmiconsole/src/ipmiconsole_processing.c \
	libipmiconsole/src/ipmiconsole_processing.h \
	libipmiconsole/src/ipmiconsole_util.c \
	libipmiconsole/src/ipmiconsole_util.h \
	libipmiconsole/src/scbuf.c \
	libipmiconsole/src/scbuf.h

libipmidetect_src_libipmidetect_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-D_GNU_SOURCE \
	-D_REENTRANT

libipmidetect_src_libipmidetect_la_LDFLAGS = \
	-version-info @LIBIPMIDETECT_VERSION_INFO@ \
	$(LIBIPMIDETECT_OTHER_FLAGS)

libipmidetect_src_libipmidetect_la_LIBADD = \
	$(top_builddir)/common/src/miscutil/libmiscutil.la \
	$(top_builddir)/common/src/portability/libportability.la

libipmidetect_src_libipmidetect_la_SOURCES = \
	libipmidetect/src/ipmidetect.c

libipmimonitoring_src_libipmimonitoring_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

libipmimonitoring_src_libipmimonitoring_la_LDFLAGS = \
	-version-info @LIBIPMIMONITORING_VERSION_INFO@ \
	$(OTHER_FLAGS)

libipmimonitoring_src_libipmimonitoring_la_LIBADD = \
	$(top_builddir)/common/src/miscutil/libmiscutil.la \
	$(top_builddir)/common/src/portability/libportability.la \
	$(top_builddir)/libfreeipmi/src/libfreeipmi.la

libipmimonitoring_src_libipmimonitoring_la_SOURCES = \
	libipmimonitoring/src/ipmi_monitoring.c \
	libipmimonitoring/src/ipmi_monitoring_debug.c \
	libipmimonitoring/src/ipmi_monitoring_debug.h \
	libipmimonitoring/src/ipmi_monitoring_defs.h \
	libipmimonitoring/src/ipmi_monitoring_ipmi_communication.c \
	libipmimonitoring/src/ipmi_monitoring_ipmi_communication.h \
	libipmimonitoring/src/ipmi_monitoring_parse_common.c \
	libipmimonitoring/src/ipmi_monitoring_parse_common.h \
	libipmimonitoring/src/ipmi_monitoring_sdr_cache.c \
	libipmimonitoring/src/ipmi_monitoring_sdr_cache.h \
	libipmimonitoring/src/ipmi_monitoring_sel.c \
	libipmimonitoring/src/ipmi_monitoring_sel.h \
	libipmimonitoring/src/ipmi_monitoring_sensor_reading.c \
	libipmimonitoring/src/ipmi_monitoring_sensor_reading.h

noinst_LTLIBRARIES = \
	common/src/configtool/libconfigtoolcommon.la \
	common/src/debugutil/libdebugutil.la \
	common/src/miscutil/libmiscutil.la \
	common/src/parsecommon/libparsecommon.la \
	common/src/pingtool/libpingtoolcommon.la \
	common/src/portability/libportability.la \
	common/src/toolcommon/libtoolcommon.la \
	common/src/toolhostrange/libtoolhostrange.la \
	common/src/tooloem/libtooloem.la \
	common/src/toolsdr/libtoolsdr.la \
	common/src/toolsensor/libtoolsensor.la

common_src_configtool_libconfigtoolcommon_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/parsecommon \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/common/src/toolcommon \
	-I$(top_srcdir)/common/src/toolhostrange \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

common_src_configtool_libconfigtoolcommon_la_SOURCES = \
	common/src/configtool/config-tool-argp.c \
	common/src/configtool/config-tool-argp.h \
	common/src/configtool/config-tool-comment.c \
	common/src/configtool/config-tool-comment.h \
	common/src/configtool/config-tool-commit.c \
	common/src/configtool/config-tool-commit.h \
	common/src/configtool/config-tool-common.h \
	common/src/configtool/config-tool-checkout.c \
	common/src/configtool/config-tool-checkout.h \
	common/src/configtool/config-tool-diff.c \
	common/src/configtool/config-tool-diff.h \
	common/src/configtool/config-tool-parse.c \
	common/src/configtool/config-tool-parse.h \
	common/src/configtool/config-tool-pef-conf-section.c \
	common/src/configtool/config-tool-pef-conf-section.h \
	common/src/configtool/config-tool-section.c \
	common/src/configtool/config-tool-section.h \
	common/src/configtool/config-tool-utils.c \
	common/src/configtool/config-tool-utils.h \
	common/src/configtool/config-tool-validate.c \
	common/src/configtool/config-tool-validate.h

common_src_debugutil_libdebugutil_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

common_src_debugutil_libdebugutil_la_SOURCES = \
        common/src/debugutil/debug-util.c \
        common/src/debugutil/debug-util.h

common_src_miscutil_libmiscutil_la_CPPFLAGS = \
	-D_GNU_SOURCE \
	-D_REENTRANT

common_src_miscutil_libmiscutil_la_SOURCES = \
	common/src/miscutil/cbuf.c \
	common/src/miscutil/cbuf.h \
	common/src/miscutil/conffile.c \
	common/src/miscutil/conffile.h \
	common/src/miscutil/error.c \
	common/src/miscutil/error.h \
	common/src/miscutil/fd.c \
	common/src/miscutil/fd.h \
	common/src/miscutil/hash.c \
	common/src/miscutil/hash.h \
	common/src/miscutil/hostlist.c \
	common/src/miscutil/hostlist.h \
	common/src/miscutil/list.c \
	common/src/miscutil/list.h \
	common/src/miscutil/secure.c \
	common/src/miscutil/secure.h \
	common/src/miscutil/thread.c \
	common/src/miscutil/thread.h \
	common/src/miscutil/timeval.c \
	common/src/miscutil/timeval.h

common_src_parsecommon_libparsecommon_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

common_src_parsecommon_libparsecommon_la_SOURCES = \
	common/src/parsecommon/parse-common.c \
	common/src/parsecommon/parse-common.h

common_src_pingtool_libpingtoolcommon_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/common/src/toolcommon \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE

common_src_pingtool_libpingtoolcommon_la_SOURCES = \
	common/src/pingtool/ping-tool-common.c \
	common/src/pingtool/ping-tool-common.h

common_src_portability_libportability_la_CPPFLAGS = \
	-D_GNU_SOURCE \
	-D_REENTRANT

if WITH_FREEIPMI_ARGP
FREEIPMI_ARG_SOURCES = \
	common/src/portability/freeipmi-argp.h \
	common/src/portability/freeipmi-argp-ba.c \
	common/src/portability/freeipmi-argp-eexst.c \
	common/src/portability/freeipmi-argp-fmtstream.c \
	common/src/portability/freeipmi-argp-fmtstream.h \
	common/src/portability/freeipmi-argp-namefrob.h \
	common/src/portability/freeipmi-argp-help.c \
	common/src/portability/freeipmi-argp-parse.c \
	common/src/portability/freeipmi-argp-pv.c \
	common/src/portability/freeipmi-argp-pvh.c
endif

common_src_portability_libportability_la_SOURCES = \
	common/src/portability/freeipmi-portability.c \
	common/src/portability/freeipmi-portability.h \
	$(FREEIPMI_ARG_SOURCES)

common_src_toolcommon_libtoolcommon_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/parsecommon \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/common/src/toolhostrange \
	-I$(top_srcdir)/common/src/toolsdr \
	-I$(top_srcdir)/common/src/toolsensor \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

common_src_toolcommon_libtoolcommon_la_SOURCES = \
	common/src/toolcommon/tool-common.c \
	common/src/toolcommon/tool-common.h \
	common/src/toolcommon/tool-config-file-common.c \
	common/src/toolcommon/tool-config-file-common.h \
	common/src/toolcommon/tool-cmdline-common.c \
	common/src/toolcommon/tool-cmdline-common.h

common_src_toolhostrange_libtoolhostrange_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libipmidetect/src \
	-D_GNU_SOURCE \
	-D_REENTRANT

common_src_toolhostrange_libtoolhostrange_la_LDFLAGS = \
	$(PTHREAD_LIBS)

common_src_toolhostrange_libtoolhostrange_la_SOURCES = \
	common/src/toolhostrange/pstdout.c \
	common/src/toolhostrange/pstdout.h \
	common/src/toolhostrange/tool-hostrange-common.c \
	common/src/toolhostrange/tool-hostrange-common.h

common_src_tooloem_libtooloem_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/common/src/toolhostrange \
	-I$(top_srcdir)/libfreeipmi/include  \
	-D_GNU_SOURCE \
	-D_REENTRANT

common_src_tooloem_libtooloem_la_SOURCES = \
	common/src/tooloem/tool-oem-common.c \
	common/src/tooloem/tool-oem-common.h

common_src_toolsdr_libtoolsdr_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/common/src/toolhostrange \
	-I$(top_srcdir)/libfreeipmi/include  \
	-D_GNU_SOURCE \
	-D_REENTRANT

common_src_toolsdr_libtoolsdr_la_SOURCES = \
	common/src/toolsdr/tool-sdr-cache-common.c \
	common/src/toolsdr/tool-sdr-cache-common.h

common_src_toolsensor_libtoolsensor_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/common/src/toolhostrange \
	-I$(top_srcdir)/common/src/toolsdr \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

common_src_toolsensor_libtoolsensor_la_SOURCES = \
	common/src/toolsensor/tool-sensor-common.c \
	common/src/toolsensor/tool-sensor-common.h

MANS_CPP = \
	bmc-config/bmc-config.8 \
	bmc-config/bmc-config.conf.5 \
	bmc-device/bmc-device.8 \
	bmc-info/bmc-info.8 \
	bmc-watchdog/bmc-watchdog.8 \
	ipmi-chassis/ipmi-chassis.8 \
	ipmi-chassis-config/ipmi-chassis-config.8 \
	ipmi-dcmi/ipmi-dcmi.8 \
	ipmi-fru/ipmi-fru.8 \
	ipmi-locate/ipmi-locate.8 \
	ipmi-oem/ipmi-oem.8 \
	ipmi-pef-config/ipmi-pef-config.8 \
	ipmi-raw/ipmi-raw.8 \
	ipmi-sel/ipmi-sel.8 \
	ipmi-sensors/ipmi-sensors.8 \
	ipmi-sensors-config/ipmi-sensors-config.8 \
	ipmiconsole/ipmiconsole.8 \
	ipmidetect/ipmidetect.8 \
	ipmidetect/ipmidetect.conf.5 \
	ipmidetectd/ipmidetectd.8 \
	ipmidetectd/ipmidetectd.conf.5 \
	ipmiping/ipmiping.8 \
	ipmipower/ipmipower.8 \
	man/freeipmi.7 \
	man/freeipmi.conf.5 \
	rmcpping/rmcpping.8 \
	libipmiconsole/libipmiconsole.3 \
	libipmiconsole/libipmiconsole.conf.5 \
	libipmidetect/libipmidetect.3 \
	libipmimonitoring/libipmimonitoring.3

MANS_NOCPP = \
	ipmi-pef-config/pef-config.8 \
	ipmi-sensors/ipmimonitoring.8 \
	ipmiconsole/ipmi-console.8 \
	ipmiconsole/ipmiconsole.conf.5 \
	ipmidetect/ipmi-detect.8 \
	ipmiping/ipmi-ping.8 \
	ipmipower/ipmi-power.8 \
	ipmipower/ipmipower.conf.5 \
	rmcpping/rmcp-ping.8 \
	libipmimonitoring/ipmi_monitoring_sensors.conf.5 \
	libipmimonitoring/ipmimonitoring_sensors.conf.5 \
	libipmimonitoring/ipmimonitoring.conf.5 \
	libipmimonitoring/libipmimonitoring.conf.5

MANS_COMMON = \
	man/manpage-common-general-options-header.man \
	man/manpage-common-driver.man \
	man/manpage-common-inband.man \
	man/manpage-common-outofband-hostname.man \
	man/manpage-common-outofband-hostname-hostranged.man \
	man/manpage-common-outofband-username-user.man \
	man/manpage-common-outofband-username-operator.man \
	man/manpage-common-outofband-username-admin.man \
	man/manpage-common-outofband-password.man \
	man/manpage-common-outofband-k-g.man \
	man/manpage-common-outofband-session-timeout.man \
	man/manpage-common-outofband-retransmission-timeout.man \
	man/manpage-common-authentication-type.man \
	man/manpage-common-cipher-suite-id-main.man \
	man/manpage-common-cipher-suite-id-details.man \
	man/manpage-common-privilege-level-user.man \
	man/manpage-common-privilege-level-operator.man \
	man/manpage-common-privilege-level-admin.man \
	man/manpage-common-config-file.man \
	man/manpage-common-workaround-flags.man \
	man/manpage-common-workaround-heading-text.man \
	man/manpage-common-workaround-inband-text.man \
	man/manpage-common-workaround-outofband-text.man \
	man/manpage-common-workaround-sol-only-text.man \
	man/manpage-common-workaround-config-tool.man \
	man/manpage-common-debug.man \
	man/manpage-common-misc.man \
	man/manpage-common-hostranged-options-header.man \
	man/manpage-common-hostranged-buffer.man \
	man/manpage-common-hostranged-consolidate.man \
	man/manpage-common-hostranged-fanout.man \
	man/manpage-common-hostranged-eliminate.man \
	man/manpage-common-hostranged-always-prefix.man \
	man/manpage-common-hostranged-text-main.man \
	man/manpage-common-hostranged-text-options.man \
	man/manpage-common-hostranged-text-localhost.man \
	man/manpage-common-hostranged-text-threads.man \
	man/manpage-common-sdr-options.man \
	man/manpage-common-ignore-sdr-cache.man \
	man/manpage-common-config-tool-options.man \
	man/manpage-common-config-tool-options-verbose.man \
	man/manpage-common-config-tool-options-very-verbose.man \
	man/manpage-common-config-tool-options-lan-channel-number.man \
	man/manpage-common-config-tool-options-serial-channel-number.man \
	man/manpage-common-config-tool-options-sol-channel-number.man \
	man/manpage-common-config-tool-general-use.man \
	man/manpage-common-troubleshooting.man \
	man/manpage-common-troubleshooting-lan-only.man \
	man/manpage-common-known-issues.man \
	man/manpage-common-known-issues-ping.man \
	man/manpage-common-reporting-bugs.man \
	man/manpage-common-gpl-library-text.man \
	man/manpage-common-gpl-program-text.man \
	man/manpage-common-gpl-freeipmi-text.man \
	man/manpage-common-ping-options.man \
	man/manpage-common-ping-origin.man \
	man/manpage-common-table-of-contents.man \
	man/manpage-common-homepage.man \
	man/manpage-common-interpret-oem-data.man \
	man/manpage-common-oem-interpretation.man \
	man/manpage-common-entity-sensor-names.man \
	man/manpage-common-no-sensor-type-output.man \
	man/manpage-common-comma-separated-output.man \
	man/manpage-common-no-header-output.man \
	man/manpage-common-non-abbreviated-units.man \
	man/manpage-common-legacy-output.man

$(MANS_CPP): $(MANS_CPP:%=%.pre)
	$(CPP) -C -P -I$(top_srcdir)/common/man $@.pre  $@

dist_man_MANS = \
	$(MANS_CPP) \
	$(MANS_NOCPP)

CLEANFILES = \
	ipmi-sensors/src/ipmimonitoring \
	$(MANS_CPP) *~

DISTCLEANFILES = \
	$(MANS_CPP) \
	$(MANS_CPP:%=%.pre) \
	.deps/*.P

EXTRA_DIST = \
	$(FREEIPMI_DOCS) \
	$(MANS_NOCPP) \
	$(MANS_COMMON) \
	bmc-watchdog/bmc-watchdog.init \
	bmc-watchdog/bmc-watchdog.logrotate \
	bmc-watchdog/bmc-watchdog.sysconfig \
	ipmidetect/ipmidetect.conf \
	ipmidetectd/ipmidetectd.init \
	ipmidetectd/ipmidetectd.conf \
	ipmi-sensors/src/ipmimonitoring.in \
	freeipmi.spec \
	libipmiconsole/libipmiconsole.conf \
	libipmiconsole/src/ipmiconsole.map \
	libipmidetect/src/ipmidetect.map \
	libipmimonitoring/src/ipmimonitoring.map

install-data-local: $(INSTALL_INIT_SCRIPTS) $(INSTALL_LOGROTATE_CONFIG) $(INSTALL_LOGDIR)
	$(INSTALL) -m 755 -d $(DESTDIR)$(FREEIPMI_SYSCONFDIR)
	$(INSTALL) -m 755 -d $(DESTDIR)$(IPMI_MONITORING_SDR_CACHE_DIR)
	$(INSTALL) -m 644 $(top_srcdir)/etc/freeipmi.conf $(DESTDIR)$(FREEIPMI_CONFIG_FILE_DEFAULT)
	$(INSTALL) -m 644 $(top_srcdir)/ipmidetect/ipmidetect.conf $(DESTDIR)$(IPMIDETECT_CONFIG_FILE_DEFAULT)
	$(INSTALL) -m 644 $(top_srcdir)/ipmidetectd/ipmidetectd.conf $(DESTDIR)$(IPMIDETECTD_CONFIG_FILE_DEFAULT)
	$(INSTALL) -m 644 $(top_srcdir)/libipmiconsole/libipmiconsole.conf $(DESTDIR)$(LIBIPMICONSOLE_CONFIG_FILE_DEFAULT)

install-init-scripts:
	$(INSTALL) -m 755 -d $(DESTDIR)$(sysconfdir)/init.d
	$(INSTALL) -m 755 $(top_srcdir)/bmc-watchdog//bmc-watchdog.init $(DESTDIR)$(sysconfdir)/init.d/bmc-watchdog
	$(INSTALL) -m 755 $(top_srcdir)/ipmidetectd/ipmidetectd.init $(DESTDIR)$(sysconfdir)/init.d/ipmidetectd
	$(INSTALL) -m 755 -d $(DESTDIR)/$(sysconfdir)/sysconfig
	$(INSTALL) -m 444 $(top_srcdir)/bmc-watchdog//bmc-watchdog.sysconfig $(DESTDIR)$(sysconfdir)/sysconfig/bmc-watchdog

install-logrotate-config:
	$(INSTALL) -m 755 -d $(DESTDIR)$(sysconfdir)/logrotate.d
	$(INSTALL) -m 444 $(top_srcdir)/bmc-watchdog/bmc-watchdog.logrotate $(DESTDIR)$(sysconfdir)/logrotate.d/bmc-watchdog

install-logdir:
	$(INSTALL) -m 755 -d $(DESTDIR)$(localstatedir)/log/$(PACKAGE)
	$(INSTALL) -m 755 -d $(DESTDIR)$(localstatedir)/log/ipmiconsole

uninstall-local:
	$(RM) -f $(DESTDIR)$(FREEIPMI_CONFIG_FILE_DEFAULT)
	$(RM) -f $(DESTDIR)$(IPMIDETECT_CONFIG_FILE_DEFAULT)
	$(RM) -f $(DESTDIR)$(IPMIDETECTD_CONFIG_FILE_DEFAULT)
	$(RM) -f $(DESTDIR)$(LIBIPMICONSOLE_CONFIG_FILE_DEFAULT)
	$(RM) -f $(DESTDIR)$(sysconfdir)/init.d/bmc-watchdog
	$(RM) -f $(DESTDIR)$(sysconfdir)/init.d/ipmidetectd
	$(RM) -f $(DESTDIR)$(sysconfdir)/logrotate.d/bmc-watchdog
	$(RM) -f $(DESTDIR)$(sysconfdir)/sysconfig/bmc-watchdog

install-exec-hook:
	$(install_sh) -d -m 755 $(DESTDIR)$(sbindir)
	$(LN_S) -f $(sbindir)/ipmi-pef-config $(DESTDIR)$(sbindir)/pef-config
	$(LN_S) -f $(sbindir)/ipmiconsole $(DESTDIR)$(sbindir)/ipmi-console
	$(LN_S) -f $(sbindir)/ipmidetect $(DESTDIR)$(sbindir)/ipmi-detect
	$(LN_S) -f $(sbindir)/ipmiping $(DESTDIR)$(sbindir)/ipmi-ping
	$(LN_S) -f $(sbindir)/ipmipower $(DESTDIR)$(sbindir)/ipmi-power
	$(LN_S) -f $(sbindir)/rmcpping $(DESTDIR)$(sbindir)/rmcp-ping

uninstall-hook:
	$(RM) $(DESTDIR)$(sbindir)/pef-config
	$(RM) $(DESTDIR)$(sbindir)/ipmi-console
	$(RM) $(DESTDIR)$(sbindir)/ipmi-detect
	$(RM) $(DESTDIR)$(sbindir)/ipmi-ping
	$(RM) $(DESTDIR)$(sbindir)/ipmi-power
	$(RM) $(DESTDIR)$(sbindir)/rmcp-ping
