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

sbin_SCRIPTS = \
	ipmi-sensors/src/ipmimonitoring

BASE_CPPFLAGS = \
        -I$(top_builddir)/common/src/miscutil \
        -I$(top_builddir)/common/src/portability \
        -I$(top_builddir)/libfreeipmi/include \
        -D_GNU_SOURCE \
        -D_REENTRANT 

TOOLBASE_CPPFLAGS = \
        -I$(top_builddir)/common/src/toolcommon \
        -I$(top_builddir)/common/src/parsecommon \
	$(BASE_CPPFLAGS)

TOOLCOMMON_CPPFLAGS = \
	-I$(top_builddir)/common/src/toolhostrange \
	$(TOOLBASE_CPPFLAGS)

CONFIGTOOLCOMMON_CPPFLAGS = \
	-I$(top_builddir)/common/src/configtool \
	$(TOOLCOMMON_CPPFLAGS)

PINGTOOLCOMMON_CPPFLAGS = \
	-I$(top_builddir)/common/src/pingtool \
	-I$(top_builddir)/common/src/debugutil \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE

BASE_LDADD = \
	$(top_builddir)/common/src/miscutil/libmiscutils.la \
	$(top_builddir)/common/src/parsecommon/libparsecommon.la \
	$(top_builddir)/common/src/portability/libfreeipmiportability.la \
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
	$(top_builddir)/common/src/portability/libfreeipmiportability.la \
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
	-I$(top_builddir)/common/src/debugutil \
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
	-I$(top_builddir)/common/src/tooloem \
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
	-I$(top_builddir)/common/src/tooloem \
	-I$(top_builddir)/common/src/toolsdr \
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
	-I$(top_builddir)/common/src/tooloem \
	-I$(top_builddir)/common/src/toolsdr \
	-I$(top_builddir)/common/src/toolsensor \
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
	-I$(top_builddir)/common/src/tooloem \
	-I$(top_builddir)/common/src/toolsdr \
	-I$(top_builddir)/common/src/toolsensor \
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
	-I$(top_builddir)/common/src/toolsdr \
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
	$(TOOLBASE_CPPFLAGS) \
	-I$(top_builddir)/libipmiconsole/src/

ipmiconsole_src_ipmiconsole_LDADD = \
	$(TOOLBASE_LDADD) \
	$(top_builddir)/libipmiconsole/src/libipmiconsole.la

ipmiconsole_src_ipmiconsole_SOURCES = \
	ipmiconsole/src/ipmiconsole.c \
	ipmiconsole/src/ipmiconsole._h \
	ipmiconsole/src/ipmiconsole-argp.c \
	ipmiconsole/src/ipmiconsole-argp.h

ipmidetect_src_ipmidetect_CPPFLAGS = \
	$(BASE_CPPFLAGS) \
	-I$(top_builddir)/libipmidetect/src/

ipmidetect_src_ipmidetect_LDADD = \
	$(BASE_LDADD) \
	$(top_builddir)/libipmidetect/src/libipmidetect.la

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
	$(TOOLCOMMON_CPPFLAGS) \
	-I$(top_builddir)/common/src/debugutil

ipmipower_src_ipmipower_LDADD = \
	$(TOOLCOMMON_LDADD) \
	$(top_builddir)/common/src/debugutil/libdebugutil.la

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
	rmcpping/rmcpping.8

$(MANS_CPP): $(MANS_CPP:%=%.pre)
	$(CPP) -C -P -I. $@.pre  $@

MANS_NOCPP = \
	ipmi-pef-config/pef-config.8 \
	ipmi-sensors/ipmimonitoring.8 \
	ipmiconsole/ipmi-console.8 \
	ipmiconsole/ipmiconsole.conf.5 \
	ipmidetect/ipmi-detect.8 \
	ipmiping/ipmi-ping.8 \
	ipmipower/ipmi-power.8 \
	ipmipower/ipmipower.conf.5 \
	rmcpping/rmcp-ping.8

dist_man_MANS = \
	$(MANS_CPP) \
	$(MANS_NOCPP)

CLEANFILES = $(MANS_CPP) *~

DISTCLEANFILES = $(MANS_CPP) $(MANS_CPP:%=%.pre) .deps/*.P

EXTRA_DIST = \
	$(EXTRA) \
	$(MANS_NOCPP) \
	bmc-watchdog/bmc-watchdog.init \
	bmc-watchdog/bmc-watchdog.logrotate \
	bmc-watchdog/bmc-watchdog.sysconfig \
	ipmidetect/ipmidetect.conf \
	ipmidetectd/ipmidetectd.init \
	ipmidetectd/ipmidetectd.conf \
	ipmi-sensors/src/ipmimonitoring \
	freeipmi.spec

install-data-local: $(INSTALL_INIT_SCRIPTS) $(INSTALL_LOGROTATE_CONFIG) $(INSTALL_LOGDIR)
	$(INSTALL) -m 755 -d $(DESTDIR)$(FREEIPMI_SYSCONFDIR)
	$(INSTALL) -m 644 ipmidetct/ipmidetect.conf $(DESTDIR)$(IPMIDETECT_CONFIG_FILE_DEFAULT)
	$(INSTALL) -m 755 -d $(DESTDIR)$(FREEIPMI_SYSCONFDIR)
	$(INSTALL) -m 644 ipmidetectd/ipmidetectd.conf $(DESTDIR)$(IPMIDETECTD_CONFIG_FILE_DEFAULT)

install-init-scripts:
	$(INSTALL) -m 755 -d $(DESTDIR)$(sysconfdir)/init.d
	$(INSTALL) -m 755 bmc-watchdog//bmc-watchdog.init $(DESTDIR)$(sysconfdir)/init.d/bmc-watchdog
	$(INSTALL) -m 755 ipmidetectd/ipmidetectd.init $(DESTDIR)$(sysconfdir)/init.d/ipmidetectd
	$(INSTALL) -m 755 -d $(DESTDIR)/$(sysconfdir)/sysconfig
	$(INSTALL) -m 444 bmc-watchdog//bmc-watchdog.sysconfig $(DESTDIR)$(sysconfdir)/sysconfig/bmc-watchdog

install-logrotate-config:
	$(INSTALL) -m 755 -d $(DESTDIR)$(sysconfdir)/logrotate.d
	$(INSTALL) -m 444 bmc-watchdog//bmc-watchdog.logrotate $(DESTDIR)$(sysconfdir)/logrotate.d/bmc-watchdog

install-logdir:
	$(INSTALL) -m 755 -d $(DESTDIR)$(localstatedir)/log/$(PACKAGE)

uninstall-local:
	$(RM) -f $(DESTDIR)$(sysconfdir)/init.d/bmc-watchdog
	$(RM) -f $(DESTDIR)$(sysconfdir)/logrotate.d/bmc-watchdog
	$(RM) -f $(DESTDIR)$(sysconfdir)/sysconfig/bmc-watchdog
	$(RM) -f $(DESTDIR)$(sysconfdir)/init.d/ipmidetectd
	$(RM) -f $(DESTDIR)$(IPMIDETECT_CONFIG_FILE_DEFAULT)
	$(RM) -f $(DESTDIR)$(IPMIDETECTD_CONFIG_FILE_DEFAULT)

install-exec-hook:
	$(install_sh) -d -m 755 $(DESTDIR)$(sbindir)
	$(LN_S) -f $(sbindir)/ipmi-pef-config $(DESTDIR)$(sbindir)/pef-config
	$(LN_S) -f $(sbindir)/ipmiconsole $(DESTDIR)$(sbindir)/ipmi-console
	$(LN_S) -f $(sbindir)/ipmipower $(DESTDIR)$(sbindir)/ipmi-power
	$(LN_S) -f $(sbindir)/ipmiping $(DESTDIR)$(sbindir)/ipmi-ping
	$(LN_S) -f $(sbindir)/rmcpping $(DESTDIR)$(sbindir)/rmcp-ping

uninstall-hook:
	$(RM) $(DESTDIR)$(sbindir)/pef-config
	$(RM) $(DESTDIR)$(sbindir)/ipmi-console
	$(RM) $(DESTDIR)$(sbindir)/ipmi-ping
	$(RM) $(DESTDIR)$(sbindir)/ipmi-power
	$(RM) $(DESTDIR)$(sbindir)/rmcp-ping
