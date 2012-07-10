##*****************************************************************************
## $Id: Makefile.am,v 1.11 2008-09-17 22:56:04 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

sbin_PROGRAMS = ipmidetect

ipmidetect_CPPFLAGS = \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/portability \
	-I$(top_builddir)/libipmidetect/ \
	-D_GNU_SOURCE \
	-D_REENTRANT

ipmidetect_LDADD = \
	$(top_builddir)/common/miscutil/libmiscutil.la \
	$(top_builddir)/common/portability/libportability.la \
	$(top_builddir)/libipmidetect/libipmidetect.la

ipmidetect_SOURCES = \
	ipmidetect.c \
	ipmidetect_.h \
	ipmidetect-argp.c \
	ipmidetect-argp.h

$(top_builddir)/common/miscutil/libmiscutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/portability/libportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libipmidetect/libipmidetect.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:

install-exec-hook:
	$(install_sh) -d -m 755 $(DESTDIR)$(sbindir)
	$(LN_S) -f $(sbindir)/ipmidetect $(DESTDIR)$(sbindir)/ipmi-detect

uninstall-hook:
	$(RM) $(DESTDIR)$(sbindir)/ipmi-detect
