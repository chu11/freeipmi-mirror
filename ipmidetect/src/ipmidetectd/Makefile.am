##*****************************************************************************
## $Id: Makefile.am,v 1.2 2007-10-15 16:07:00 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	debug.h \
	error.h \
	hash.h \
	thread.h \
	ipmidetectd.h \
	ipmidetectd_config.h \
	ipmidetectd_loop.h

sbin_PROGRAMS = ipmidetectd

ipmidetectd_SOURCES = \
	debug.c \
	error.c \
	hash.c \
	thread.c \
	ipmidetectd.c \
	ipmidetectd_config.c \
	ipmidetectd_loop.c

ipmidetectd_CPPFLAGS = \
	-I$(srcdir)/../../../common/src \
	-I$(srcdir)/../../../libfreeipmi/include

ipmidetectd_LDADD = \
	../../../common/src/libllnlcommon.la \
	../../../libfreeipmi/src/libfreeipmi.la

../../../common/src/libllnlcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
