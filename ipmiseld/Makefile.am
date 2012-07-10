##*****************************************************************************
## $Id: Makefile.am,v 1.11 2008-09-17 22:56:04 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

sbin_PROGRAMS = ipmiseld

ipmiseld_CPPFLAGS = \
	-I$(top_srcdir)/common/toolcommon \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/parsecommon \
	-I$(top_srcdir)/common/portability \
	-I$(top_builddir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT \
	-DIPMISELD_LOCALSTATEDIR='"$(localstatedir)"'

ipmiseld_LDADD = \
	$(top_builddir)/common/toolcommon/libtoolcommon.la \
	$(top_builddir)/common/miscutil/libmiscutil.la \
	$(top_builddir)/common/parsecommon/libparsecommon.la \
	$(top_builddir)/common/portability/libportability.la \
	$(top_builddir)/libfreeipmi/libfreeipmi.la

ipmiseld_SOURCES = \
	ipmiseld.c \
	ipmiseld.h \
	ipmiseld-argp.c \
	ipmiseld-argp.h \
	ipmiseld-cache.c \
	ipmiseld-cache.h \
	ipmiseld-common.c \
	ipmiseld-common.h \
	ipmiseld-debug.c \
	ipmiseld-debug.h \
	ipmiseld-ipmi-communication.c \
	ipmiseld-ipmi-communication.h \
	ipmiseld-threadpool.c \
	ipmiseld-threadpool.h

$(top_builddir)/common/toolcommon/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/miscutil/libmiscutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/parsecommon/libparsecommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/portability/libportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:

IPMISELDCACHEdir = $(IPMISELD_CACHE_DIRECTORY)

install-data-local:
	$(INSTALL) -m 755 -d $(DESTDIR)$(IPMISELDCACHEdir)

