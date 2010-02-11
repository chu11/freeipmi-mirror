##*****************************************************************************
## $Id: Makefile.am,v 1.23.12.4 2010-02-11 19:45:10 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	ipmimonitoring-sensors.h \
	ipmimonitoring-sensors-argp.h

AM_CPPFLAGS = \
	-I$(top_srcdir)/common/src/toolcommon \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/ipmimonitoring/src/libipmimonitoring \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

noinst_PROGRAMS = ipmimonitoring-sensors

ipmimonitoring_sensors_LDADD = \
	$(top_builddir)/common/src/toolcommon/libtoolcommon.la \
	$(top_builddir)/common/src/miscutil/libmiscutils.la \
	$(top_builddir)/common/src/portability/libfreeipmiportability.la \
	$(top_builddir)/ipmidetect/src/libipmidetect/libipmidetect.la \
	$(top_builddir)/ipmimonitoring/src/libipmimonitoring/libipmimonitoring.la

ipmimonitoring_sensors_SOURCES = \
	ipmimonitoring-sensors.c \
	ipmimonitoring-sensors-argp.c

$(top_builddir)/common/src/toolcommon/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/miscutil/libmiscutils.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/portability/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/ipmidetect/src/libipmidetect/libipmidetect.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/ipmimonitoring/src/libipmimonitoring/libipmimonitoring.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:
