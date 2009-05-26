##*****************************************************************************
## $Id: Makefile.am,v 1.1 2009-05-26 23:35:57 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_LTLIBRARIES = libdcmi.la 

libdcmi_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/debugutil \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/src \
	-D_GNU_SOURCE \
	-D_REENTRANT

libdcmi_la_SOURCES = \
	ipmi-dcmi.c
