##*****************************************************************************
## $Id: Makefile.am,v 1.6.2.1 2007-12-22 20:20:50 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

include_HEADERS = ipmi_monitoring.h

noinst_HEADERS = \
	ipmi_monitoring_debug.h \
	ipmi_monitoring_defs.h \
	ipmi_monitoring_fiid_wrappers.h \
	ipmi_monitoring_ipmi_communication.h \
	ipmi_monitoring_sdr_cache.h \
	ipmi_monitoring_sensor_reading.h \
	ipmi_monitoring_sensor_config.h

lib_LTLIBRARIES = libipmimonitoring.la

libipmimonitoring_la_SOURCES = \
	ipmi_monitoring.c \
	ipmi_monitoring_debug.c \
	ipmi_monitoring_fiid_wrappers.c \
	ipmi_monitoring_ipmi_communication.c \
	ipmi_monitoring_sdr_cache.c \
	ipmi_monitoring_sensor_reading.c \
	ipmi_monitoring_sensor_config.c

libipmimonitoring_la_CPPFLAGS = -D_REENTRANT \
	-I$(srcdir)/../../../common/src \
	-I$(srcdir)/../../../libfreeipmi/include

libipmimonitoring_la_LDFLAGS = \
	-version-info @LIBIPMIMONITORING_VERSION_INFO@ \
	$(OTHER_FLAGS)

libipmimonitoring_la_LIBADD = \
	../../../common/src/libmiscutils.la \
	../../../libfreeipmi/src/libfreeipmi.la \
	../../../common/src/libfreeipmiportability.la

../../../common/src/libmiscutils.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../common/src/libfreeipmiportability.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:

IPMISDRCACHEdir = $(IPMI_MONITORING_SDR_CACHE_DIR)

install-data-local:
	$(INSTALL) -m 755 -d $(DESTDIR)$(IPMISDRCACHEdir)

EXTRA_DIST = ipmimonitoring.map

