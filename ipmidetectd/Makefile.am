##*****************************************************************************
## $Id: Makefile.am,v 1.11 2008-09-17 22:56:04 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

sbin_PROGRAMS = ipmidetectd

ipmidetectd_CPPFLAGS = \
	-I$(top_srcdir)/common/daemonutil \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT \
	-DIPMIDETECTD_LOCALSTATEDIR='"$(localstatedir)"'

ipmidetectd_LDADD = \
	$(top_builddir)/common/daemonutil/libdaemonutil.la \
	$(top_builddir)/common/miscutil/libmiscutil.la \
	$(top_builddir)/common/portability/libportability.la \
	$(top_builddir)/libfreeipmi/libfreeipmi.la

ipmidetectd_SOURCES = \
	ipmidetectd.c \
	ipmidetectd.h \
	ipmidetectd_argp.c \
	ipmidetectd_argp.h \
	ipmidetectd_config.c \
	ipmidetectd_config.h

$(top_builddir)/common/daemonutil/libdaemonutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/miscutil/libmiscutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/portability/libportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:

