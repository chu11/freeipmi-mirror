##*****************************************************************************
## $Id: Makefile.am,v 1.7 2008-06-21 05:46:35 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	ipmi-sdr-cache-common.h \
	ipmi-sdr-cache-defs.h

noinst_LTLIBRARIES = libsdrcache.la 

libsdrcache_la_LIBADD = \
	$(top_builddir)/common/src/portability/libfreeipmiportability.la

libsdrcache_la_CPPFLAGS = \
	-I$(top_builddir)/common/src/debugutil \
	-I$(top_builddir)/common/src/miscutil \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/libfreeipmi/include \
	-I$(top_builddir)/libfreeipmi/src \
	-D_GNU_SOURCE \
	-D_REENTRANT

libsdrcache_la_SOURCES = \
	ipmi-sdr-cache.c \
	ipmi-sdr-cache-common.c \
	ipmi-sdr-cache-create.c \
	ipmi-sdr-cache-delete.c \
	ipmi-sdr-cache-read.c

$(top_builddir)/common/src/portability/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
