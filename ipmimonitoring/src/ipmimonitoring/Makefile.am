##*****************************************************************************
## $Id: Makefile.am,v 1.23.12.6 2010-02-11 23:44:59 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

AM_CPPFLAGS = \
	-I$(top_srcdir)/ipmimonitoring/src/libipmimonitoring \
	-D_GNU_SOURCE \
	-D_REENTRANT

noinst_PROGRAMS = ipmimonitoring-sensors

ipmimonitoring_sensors_LDADD = \
	$(top_builddir)/ipmimonitoring/src/libipmimonitoring/libipmimonitoring.la

ipmimonitoring_sensors_SOURCES = \
	ipmimonitoring-sensors.c

$(top_builddir)/ipmimonitoring/src/libipmimonitoring/libipmimonitoring.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:
