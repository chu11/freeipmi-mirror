##*****************************************************************************
## $Id: Makefile.am,v 1.1 2007-01-30 21:52:57 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

sbin_PROGRAMS = ipmimonitoring

noinst_HEADERS = \
	error.h

ipmimonitoring_SOURCES = \
	ipmimonitoring.c \
	error.c

ipmimonitoring_CFLAGS = \
	-I$(srcdir)/../libipmimonitoring \
	-I$(srcdir)/../../../common/src \
	-I$(srcdir)/../../../libfreeipmi/include

ipmimonitoring_LDADD = \
	../../../common/src/libllnlcommon.la \
	../libipmimonitoring/libipmimonitoring.la

../../../common/src/libllnlcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../libipmimonitoring/libipmimonitoring.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
