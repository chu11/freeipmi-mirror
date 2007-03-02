##*****************************************************************************
## $Id: Makefile.am,v 1.1 2007-03-02 00:56:26 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

sbin_PROGRAMS       = ipmidetect

noinst_HEADERS      = error.h 

ipmidetect_SOURCES  = ipmidetect.c error.c

ipmidetect_CFLAGS   = -I $(srcdir)/../libipmidetect \
		      -I $(srcdir)/../../../common/src

ipmidetect_LDADD    = ../../../common/src/libllnlcommon.la \
		      ../libipmidetect/libipmidetect.la

../../../common/src/libllnlcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../libipmidetect/libipmidetect.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
