SUBDIRS = include

if WITH_PKG_CONFIG
pkgconfig_DATA = libfreeipmi.pc
endif

if WITH_GNU_LD
VERSION_SCRIPT = $(srcdir)/freeipmi.map
VERSION_FLAGS = -Wl,--version-script=$(VERSION_SCRIPT)
endif

lib_LTLIBRARIES = libfreeipmi.la

# achu
#
# IPMI_DEBUG_IPCKEY is so we can do inband development without the
# real ipckey being installed.

libfreeipmi_la_CPPFLAGS = \
	-I$(top_srcdir)/common/debugutil \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/portability \
	-I$(top_builddir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi \
	-DIPMI_IPCKEY=\"$(localstatedir)/lib/$(PACKAGE_NAME)/ipckey\" \
	-DIPMI_DEBUG_IPCKEY=\"$(top_builddir)/libfreeipmi/driver/ipmi-semaphores.h\" \
	-D_GNU_SOURCE \
	-D_REENTRANT

libfreeipmi_la_LDFLAGS = \
	-version-info @LIBFREEIPMI_VERSION_INFO@ $(VERSION_FLAGS) \
	$(OTHER_FLAGS) \
        $(PTHREAD_LIBS) \
	@GCRYPT_LIBS@ \
	-lm 

libfreeipmi_la_LIBADD = \
	$(top_builddir)/common/debugutil/libdebugutil.la \
	$(top_builddir)/common/miscutil/libmiscutil.la \
	$(top_builddir)/common/portability/libportability.la

libfreeipmi_la_SOURCES = \
	api/ipmi-api.c \
	api/ipmi-api-defs.h \
	api/ipmi-api-trace.h \
	api/ipmi-api-util.c \
	api/ipmi-api-util.h \
	api/ipmi-chassis-cmds-api.c \
	api/ipmi-dcmi-cmds-api.c \
	api/ipmi-device-global-cmds-api.c \
	api/ipmi-event-cmds-api.c \
	api/ipmi-firmware-firewall-command-discovery-cmds-api.c \
	api/ipmi-fru-inventory-device-cmds-api.c \
	api/ipmi-inteldcmi-driver-api.c \
	api/ipmi-inteldcmi-driver-api.h \
	api/ipmi-kcs-driver-api.c \
	api/ipmi-kcs-driver-api.h \
	api/ipmi-lan-cmds-api.c \
	api/ipmi-lan-interface-api.c \
	api/ipmi-lan-interface-api.h \
	api/ipmi-lan-session-common.c \
	api/ipmi-lan-session-common.h \
	api/ipmi-messaging-support-cmds-api.c \
	api/ipmi-oem-intel-node-manager-cmds-api.c \
	api/ipmi-openipmi-driver-api.c \
	api/ipmi-openipmi-driver-api.h \
	api/ipmi-pef-and-alerting-cmds-api.c \
	api/ipmi-rmcpplus-support-and-payload-cmds-api.c \
	api/ipmi-sel-cmds-api.c \
	api/ipmi-sdr-repository-cmds-api.c \
	api/ipmi-sensor-cmds-api.c \
	api/ipmi-serial-modem-cmds-api.c \
	api/ipmi-sol-cmds-api.c \
	api/ipmi-ssif-driver-api.c \
	api/ipmi-ssif-driver-api.h \
	api/ipmi-sunbmc-driver-api.c \
	api/ipmi-sunbmc-driver-api.h \
	cmds/ipmi-bmc-watchdog-timer-cmds.c \
	cmds/ipmi-chassis-cmds.c \
	cmds/ipmi-dcmi-cmds.c \
	cmds/ipmi-device-global-cmds.c \
	cmds/ipmi-event-cmds.c \
	cmds/ipmi-firmware-firewall-command-discovery-cmds.c \
	cmds/ipmi-fru-inventory-device-cmds.c \
	cmds/ipmi-lan-cmds.c \
	cmds/ipmi-messaging-support-cmds.c \
	cmds/ipmi-oem-intel-node-manager-cmds.c \
	cmds/ipmi-pef-and-alerting-cmds.c \
	cmds/ipmi-rmcpplus-support-and-payload-cmds.c \
	cmds/ipmi-sdr-repository-cmds.c \
	cmds/ipmi-sel-cmds.c \
	cmds/ipmi-sensor-cmds.c \
	cmds/ipmi-serial-modem-cmds.c \
	cmds/ipmi-sol-cmds.c \
	cmds/rmcp-cmds.c \
	debug/ipmi-debug.c \
	debug/ipmi-debug-common.c \
	debug/ipmi-debug-common.h \
	debug/ipmi-debug-inband.c \
	debug/ipmi-debug-lan.c \
	debug/ipmi-debug-rmcp.c \
	debug/ipmi-debug-rmcpplus.c \
	debug/ipmi-debug-sdr.c \
	driver/freeipmi_bmc_intf.h \
	driver/ipmi-driver-trace.h \
	driver/ipmi-semaphores.c \
	driver/ipmi-semaphores.h \
	driver/ipmi-inteldcmi-driver.c \
	driver/ipmi-kcs-driver.c \
	driver/ipmi-openipmi-driver.c \
	driver/ipmi-sunbmc-driver.c \
	driver/ipmi-ssif-driver.c \
	fiid/fiid.c \
	fru/ipmi-fru.c \
	fru/ipmi-fru-common.c \
	fru/ipmi-fru-common.h \
	fru/ipmi-fru-data.c \
	fru/ipmi-fru-defs.h \
	fru/ipmi-fru-trace.h \
	fru/ipmi-fru-util.c \
	fru/ipmi-fru-util.h \
	interface/ipmi-ipmb-interface.c \
	interface/ipmi-kcs-interface.c \
	interface/ipmi-lan-interface.c \
	interface/ipmi-network.c \
	interface/ipmi-network.h \
	interface/ipmi-rmcpplus-interface.c \
	interface/rmcp-interface.c \
	interpret/ipmi-interpret.c \
	interpret/ipmi-interpret-config-common.c \
	interpret/ipmi-interpret-config-common.h \
	interpret/ipmi-interpret-config-sel.c \
	interpret/ipmi-interpret-config-sel.h \
	interpret/ipmi-interpret-config-sensor.c \
	interpret/ipmi-interpret-config-sensor.h \
	interpret/ipmi-interpret-defs.h \
	interpret/ipmi-interpret-util.c \
	interpret/ipmi-interpret-util.h \
	interpret/ipmi-interpret-trace.h \
	libcommon/ipmi-bit-ops.c \
	libcommon/ipmi-bit-ops.h \
	libcommon/ipmi-crypt.c \
	libcommon/ipmi-crypt.h \
	libcommon/ipmi-fiid-util.c \
	libcommon/ipmi-fiid-util.h \
	libcommon/ipmi-fill-util.h \
	libcommon/ipmi-md2.c \
	libcommon/ipmi-md2.h \
	libcommon/ipmi-md5.c \
	libcommon/ipmi-md5.h \
	libcommon/ipmi-trace.h \
	locate/ipmi-locate.c \
	locate/ipmi-locate-acpi-spmi.c \
	locate/ipmi-locate-defaults.c \
	locate/ipmi-locate-defs.h \
	locate/ipmi-locate-dmidecode.c \
	locate/ipmi-locate-pci.c \
	locate/ipmi-locate-smbios.c \
	locate/ipmi-locate-trace.h \
	locate/ipmi-locate-util.c \
	locate/ipmi-locate-util.h \
	payload/ipmi-sol-payload.c \
	record-format/ipmi-cipher-suite-record-format.c \
	record-format/ipmi-fru-dimmspd-record-format.c \
	record-format/ipmi-fru-information-record-format.c \
	record-format/ipmi-sdr-record-format.c \
	record-format/ipmi-sdr-oem-record-format.c \
	record-format/ipmi-sel-record-format.c \
	sdr/ipmi-sdr.c \
	sdr/ipmi-sdr-common.c \
	sdr/ipmi-sdr-common.h \
	sdr/ipmi-sdr-cache-create.c \
	sdr/ipmi-sdr-defs.h \
	sdr/ipmi-sdr-cache-delete.c \
	sdr/ipmi-sdr-cache-read.c \
	sdr/ipmi-sdr-oem.c \
	sdr/ipmi-sdr-parse.c \
	sdr/ipmi-sdr-parse-util.c \
	sdr/ipmi-sdr-stats.c \
	sdr/ipmi-sdr-trace.h \
	sdr/ipmi-sdr-util.c \
	sdr/ipmi-sdr-util.h \
	sel/ipmi-sel.c \
	sel/ipmi-sel-common.c \
	sel/ipmi-sel-common.h \
	sel/ipmi-sel-defs.h \
	sel/ipmi-sel-string.c \
	sel/ipmi-sel-string.h \
	sel/ipmi-sel-string-dell.c \
	sel/ipmi-sel-string-dell.h \
	sel/ipmi-sel-string-fujitsu.c \
	sel/ipmi-sel-string-fujitsu.h \
	sel/ipmi-sel-string-intel.c \
	sel/ipmi-sel-string-intel.h \
	sel/ipmi-sel-string-intel-node-manager.c \
	sel/ipmi-sel-string-intel-node-manager.h \
	sel/ipmi-sel-string-inventec.c \
	sel/ipmi-sel-string-inventec.h \
	sel/ipmi-sel-string-quanta.c \
	sel/ipmi-sel-string-quanta.h \
	sel/ipmi-sel-string-sun.c \
	sel/ipmi-sel-string-sun.h \
	sel/ipmi-sel-string-supermicro.c \
	sel/ipmi-sel-string-supermicro.h \
	sel/ipmi-sel-string-wistron.c \
	sel/ipmi-sel-string-wistron.h \
	sel/ipmi-sel-trace.h \
	sel/ipmi-sel-util.c \
	sel/ipmi-sel-util.h \
	sensor-read/ipmi-sensor-read.c \
	sensor-read/ipmi-sensor-read-defs.h \
	sensor-read/ipmi-sensor-read-trace.h \
	sensor-read/ipmi-sensor-read-util.c \
	sensor-read/ipmi-sensor-read-util.h \
	spec/ipmi-device-types-spec.c \
	spec/ipmi-entity-ids-spec.c \
	spec/ipmi-fru-chassis-types-spec.c \
	spec/ipmi-fru-language-codes-spec.c \
	spec/ipmi-iana-enterprise-numbers-spec.c \
	spec/ipmi-jedec-manufacturer-identification-code-spec.c \
	spec/ipmi-sensor-and-event-code-tables-spec.c \
	spec/ipmi-sensor-and-event-code-tables-oem-spec.c \
	spec/ipmi-sensor-types-spec.c \
	spec/ipmi-sensor-units-spec.c \
	util/ipmi-channel-util.c \
	util/ipmi-cipher-suite-util.c \
	util/ipmi-dcmi-util.c \
	util/ipmi-device-types-util.c \
	util/ipmi-entity-ids-util.c \
	util/ipmi-error-dcmi-util.c \
	util/ipmi-error-util.c \
	util/ipmi-iana-enterprise-numbers-util.c \
	util/ipmi-ipmb-util.c \
	util/ipmi-jedec-manufacturer-identification-code-util.c \
	util/ipmi-lan-util.c \
	util/ipmi-outofband-util.c \
	util/ipmi-rmcpplus-util.c \
	util/ipmi-sensor-and-event-code-tables-util.c \
	util/ipmi-sensor-util.c \
	util/ipmi-timestamp-util.c \
	util/ipmi-util.c \
	util/rmcp-util.c

$(top_builddir)/common/debugutil/libdebugutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/miscutil/libmiscutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/portability/libportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:

EXTRA_DIST = freeipmi.map ipckey

install-data-local:
	$(INSTALL) -m 755 -d $(DESTDIR)$(localstatedir)/lib/$(PACKAGE_NAME)
	$(INSTALL) -m 644 $(srcdir)/ipckey $(DESTDIR)$(localstatedir)/lib/$(PACKAGE_NAME)/ipckey

uninstall-local:
	$(RM) -f $(DESTDIR)$(localstatedir)/lib/$(PACKAGE_NAME)/ipckey



