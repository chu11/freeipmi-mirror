if WITH_PKG_CONFIG
pkgconfig_DATA = libipmimonitoring.pc
endif

if WITH_GNU_LD
VERSION_SCRIPT = $(srcdir)/ipmimonitoring.map
OTHER_FLAGS = -Wl,--version-script=$(VERSION_SCRIPT)
endif

noinst_HEADERS = \
	ipmi_monitoring_debug.h \
	ipmi_monitoring_defs.h \
	ipmi_monitoring_ipmi_communication.h \
	ipmi_monitoring_parse_common.h \
	ipmi_monitoring_sdr_cache.h \
	ipmi_monitoring_sel.h \
	ipmi_monitoring_sensor_reading.h

nodist_include_HEADERS = \
	ipmi_monitoring.h

include_HEADERS = \
	ipmi_monitoring_bitmasks.h \
	ipmi_monitoring_offsets.h

lib_LTLIBRARIES = libipmimonitoring.la

libipmimonitoring_la_CPPFLAGS = \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/portability \
	-I$(top_builddir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

libipmimonitoring_la_LDFLAGS = \
	-version-info @LIBIPMIMONITORING_VERSION_INFO@ \
	$(OTHER_FLAGS)

libipmimonitoring_la_LIBADD = \
	$(top_builddir)/common/miscutil/libmiscutil.la \
	$(top_builddir)/common/portability/libportability.la \
	$(top_builddir)/libfreeipmi/libfreeipmi.la

libipmimonitoring_la_SOURCES = \
	ipmi_monitoring.c \
	ipmi_monitoring_debug.c \
	ipmi_monitoring_ipmi_communication.c \
	ipmi_monitoring_parse_common.c \
	ipmi_monitoring_sdr_cache.c \
	ipmi_monitoring_sel.c \
	ipmi_monitoring_sensor_reading.c

$(top_builddir)/common/miscutil/libmiscutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/portability/libportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:

IPMISDRCACHEdir = $(IPMI_MONITORING_SDR_CACHE_DIR)

install-data-local:
	$(INSTALL) -m 755 -d $(DESTDIR)$(IPMISDRCACHEdir)

EXTRA_DIST = ipmimonitoring.map

