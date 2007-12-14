##*****************************************************************************
## $Id: Makefile.am,v 1.3 2007-12-14 19:16:25 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

sbin_PROGRAMS       = ipmidetect

noinst_HEADERS      = error.h 

ipmidetect_SOURCES  = ipmidetect.c error.c

ipmidetect_CPPFLAGS   = -I $(srcdir)/../libipmidetect \
		      -I $(srcdir)/../../../common/src

ipmidetect_LDADD    = ../../../common/src/libmiscutils.la \
		      ../libipmidetect/libipmidetect.la

../../../common/src/libmiscutils.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../libipmidetect/libipmidetect.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
