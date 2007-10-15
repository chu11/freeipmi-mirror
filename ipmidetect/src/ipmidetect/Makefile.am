##*****************************************************************************
## $Id: Makefile.am,v 1.2 2007-10-15 16:07:00 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

sbin_PROGRAMS       = ipmidetect

noinst_HEADERS      = error.h 

ipmidetect_SOURCES  = ipmidetect.c error.c

ipmidetect_CPPFLAGS   = -I $(srcdir)/../libipmidetect \
		      -I $(srcdir)/../../../common/src

ipmidetect_LDADD    = ../../../common/src/libllnlcommon.la \
		      ../libipmidetect/libipmidetect.la

../../../common/src/libllnlcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../libipmidetect/libipmidetect.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
