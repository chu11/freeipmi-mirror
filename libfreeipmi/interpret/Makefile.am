##*****************************************************************************
## $Id: Makefile.am,v 1.4 2010-05-21 22:04:20 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	ipmi-interpret-defs.h \
	ipmi-interpret-config-common.h \
	ipmi-interpret-config-sel.h \
	ipmi-interpret-config-sensor.h \
	ipmi-interpret-trace.h \
	ipmi-interpret-util.h

noinst_LTLIBRARIES = libinterpret.la 

libinterpret_la_CPPFLAGS = \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi \
	-D_GNU_SOURCE \
	-D_REENTRANT

libinterpret_la_SOURCES = \
	ipmi-interpret.c \
	ipmi-interpret-config-common.c \
	ipmi-interpret-config-sel.c \
	ipmi-interpret-config-sensor.c \
	ipmi-interpret-util.c
