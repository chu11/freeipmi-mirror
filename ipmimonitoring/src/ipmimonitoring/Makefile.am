##*****************************************************************************
## $Id: Makefile.am,v 1.5.2.1 2007-12-12 18:30:20 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

sbin_PROGRAMS = ipmimonitoring

noinst_HEADERS = \
	error.h

ipmimonitoring_SOURCES = \
	ipmimonitoring.c \
	error.c

ipmimonitoring_CPPFLAGS = \
	-I$(srcdir)/../libipmimonitoring \
	-I$(srcdir)/../../../common/src \
	-I$(srcdir)/../../../libfreeipmi/include

ipmimonitoring_LDADD = \
	../../../common/src/libcmdlineparsecommon.la \
	../../../common/src/libllnlcommon.la \
	../libipmimonitoring/libipmimonitoring.la \
	../../../common/src/libpstdout.la \
	../../../ipmidetect/src/libipmidetect/libipmidetect.la \
	../../../common/src/libhostrange.la \
	../../../common/src/libtoolcommon.la

../../../common/src/libcmdlineparsecommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../common/src/libllnlcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../libipmimonitoring/libipmimonitoring.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../common/src/libpstdout.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../common/src/libhostrange.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../ipmidetect/src/libipmidetect/libipmidetect.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../common/src/libtoolcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
