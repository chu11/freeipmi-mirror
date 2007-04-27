##*****************************************************************************
## $Id: Makefile.am,v 1.2 2007-04-27 03:08:29 chu11 Exp $
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
	../libipmimonitoring/libipmimonitoring.la \
	../../../common/src/libpstdout.la \
	../../../ipmidetect/src/libipmidetect/libipmidetect.la \
	../../../common/src/libeliminate.la \
	../../../common/src/libcbuf.la

../../../common/src/libllnlcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../libipmimonitoring/libipmimonitoring.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../common/src/libpstdout.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../common/src/libeliminate.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../ipmidetect/src/libipmidetect/libipmidetect.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../common/src/libcbuf.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
