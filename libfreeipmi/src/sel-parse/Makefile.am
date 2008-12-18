##*****************************************************************************
## $Id: Makefile.am,v 1.1.2.1 2008-12-18 00:27:30 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_LTLIBRARIES = libselparse.la 

libselparse_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/debugutil \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/src \
	-D_GNU_SOURCE \
	-D_REENTRANT

libselparse_la_SOURCES = \
	ipmi-sel-parse.c
