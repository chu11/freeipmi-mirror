##*****************************************************************************
## $Id: Makefile.am,v 1.7 2007-12-29 17:20:31 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

sbin_PROGRAMS = ipmimonitoring

noinst_HEADERS = \
	ipmimonitoring.h \
	ipmimonitoring-argp.h

ipmimonitoring_SOURCES = \
	ipmimonitoring.c \
	ipmimonitoring-argp.c

ipmimonitoring_CPPFLAGS = \
	-I$(srcdir)/../libipmimonitoring \
	-I$(srcdir)/../../../common/src \
	-I$(srcdir)/../../../libfreeipmi/include

ipmimonitoring_LDADD = \
	../../../common/src/libmiscutils.la \
	../libipmimonitoring/libipmimonitoring.la \
	../../../ipmidetect/src/libipmidetect/libipmidetect.la \
	../../../common/src/libhostrange.la \
	../../../common/src/libtoolcommon.la

../../../common/src/libmiscutils.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../libipmimonitoring/libipmimonitoring.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../common/src/libhostrange.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../ipmidetect/src/libipmidetect/libipmidetect.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../common/src/libtoolcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
