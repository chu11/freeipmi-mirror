##*****************************************************************************
## $Id: Makefile.am,v 1.4 2008-04-12 00:05:21 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

sbin_PROGRAMS       = ipmidetect

ipmidetect_SOURCES  = ipmidetect.c

ipmidetect_CPPFLAGS   = -I $(srcdir)/../libipmidetect \
		      -I $(srcdir)/../../../common/src

ipmidetect_LDADD    = ../../../common/src/libmiscutils.la \
		      ../libipmidetect/libipmidetect.la

../../../common/src/libmiscutils.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../libipmidetect/libipmidetect.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
