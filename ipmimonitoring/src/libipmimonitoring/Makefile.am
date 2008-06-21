##*****************************************************************************
## $Id: Makefile.am,v 1.11 2008-06-21 05:46:33 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	ipmi_monitoring_debug.h \
	ipmi_monitoring_defs.h \
	ipmi_monitoring_fiid_wrappers.h \
	ipmi_monitoring_ipmi_communication.h \
	ipmi_monitoring_sdr_cache.h \
	ipmi_monitoring_sensor_reading.h \
	ipmi_monitoring_sensor_config.h

include_HEADERS = ipmi_monitoring.h

lib_LTLIBRARIES = libipmimonitoring.la

libipmimonitoring_la_CPPFLAGS = \
	-I$(top_builddir)/common/src/miscutil \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/libfreeipmi/include \
	-D_REENTRANT

libipmimonitoring_la_LDFLAGS = \
	-version-info @LIBIPMIMONITORING_VERSION_INFO@ \
	$(OTHER_FLAGS)

libipmimonitoring_la_LIBADD = \
	$(top_builddir)/common/src/miscutil/libmiscutils.la \
	$(top_builddir)/common/src/portability/libfreeipmiportability.la \
	$(top_builddir)/libfreeipmi/src/libfreeipmi.la

libipmimonitoring_la_SOURCES = \
	ipmi_monitoring.c \
	ipmi_monitoring_debug.c \
	ipmi_monitoring_fiid_wrappers.c \
	ipmi_monitoring_ipmi_communication.c \
	ipmi_monitoring_sdr_cache.c \
	ipmi_monitoring_sensor_reading.c \
	ipmi_monitoring_sensor_config.c

$(top_builddir)/common/src/miscutil/libmiscutils.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/common/src/portability/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/libfreeipmi/src/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:

IPMISDRCACHEdir = $(IPMI_MONITORING_SDR_CACHE_DIR)

install-data-local:
	$(INSTALL) -m 755 -d $(DESTDIR)$(IPMISDRCACHEdir)

EXTRA_DIST = ipmimonitoring.map

