##*****************************************************************************
## $Id: Makefile.am,v 1.23 2008-09-24 04:00:30 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

noinst_HEADERS = \
	ipmiconsole_.h \
	ipmiconsole-argp.h

AM_CPPFLAGS = \
	-I$(top_srcdir)/common/src/toolcommon \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/parsecommon \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libipmiconsole/src/ \
	-D_GNU_SOURCE \
	-D_REENTRANT

sbin_PROGRAMS = ipmiconsole

ipmiconsole_LDADD = \
	$(top_builddir)/common/src/toolcommon/libtoolcommon.la \
	$(top_builddir)/common/src/miscutil/libmiscutils.la \
	$(top_builddir)/common/src/parsecommon/libparsecommon.la \
	$(top_builddir)/common/src/portability/libfreeipmiportability.la \
	$(top_builddir)/libipmiconsole/src/libipmiconsole.la

ipmiconsole_SOURCES = \
	ipmiconsole.c \
	ipmiconsole-argp.c

$(top_builddir)/common/src/toolcommon/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/miscutil/libmiscutils.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/parsecommon/libparsecommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/portability/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libipmiconsole/src/libipmiconsole.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:

install-exec-hook:
	$(install_sh) -d -m 755 $(DESTDIR)$(sbindir)
	$(LN_S) $(sbindir)/ipmiconsole $(DESTDIR)$(sbindir)/ipmi-console

uninstall-hook:
	$(RM) $(DESTDIR)$(sbindir)/ipmi-console
