##*****************************************************************************
## $Id: Makefile.am,v 1.4 2008-04-12 00:05:21 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	debug.h \
	ipmidetectd.h \
	ipmidetectd_config.h \
	ipmidetectd_loop.h

sbin_PROGRAMS = ipmidetectd

ipmidetectd_SOURCES = \
	debug.c \
	ipmidetectd.c \
	ipmidetectd_config.c \
	ipmidetectd_loop.c

ipmidetectd_CPPFLAGS = \
	-I$(srcdir)/../../../common/src \
	-I$(srcdir)/../../../libfreeipmi/include

ipmidetectd_LDADD = \
	../../../common/src/libmiscutils.la \
	../../../libfreeipmi/src/libfreeipmi.la

../../../common/src/libmiscutils.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
