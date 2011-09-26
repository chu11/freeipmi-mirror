##*****************************************************************************
## $Id: Makefile.am,v 1.21 2009-06-05 21:50:41 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

if WITH_GNU_LD
VERSION_SCRIPT = $(srcdir)/ipmiconsole.map
OTHER_FLAGS = -Wl,--version-script=$(VERSION_SCRIPT)
endif

noinst_HEADERS = \
	scbuf.h \
	ipmiconsole_checks.h \
	ipmiconsole_ctx.h \
	ipmiconsole_defs.h \
	ipmiconsole_debug.h \
	ipmiconsole_engine.h \
	ipmiconsole_garbage_collector.h \
	ipmiconsole_packet.h \
	ipmiconsole_processing.h \
	ipmiconsole_util.h
		 
include_HEADERS = ipmiconsole.h

lib_LTLIBRARIES = libipmiconsole.la
		 
libipmiconsole_la_CFLAGS = $(PTHREAD_CFLAGS)

libipmiconsole_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/debugutil \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/parsecommon \
	-I$(top_srcdir)/common/src/portability \
        -I$(top_srcdir)/libfreeipmi/include \
	-DWITH_PTHREADS=1 \
	-D_GNU_SOURCE \
	-D_REENTRANT

libipmiconsole_la_LDFLAGS = \
	$(PTHREAD_LIBS) \
	-version-info @LIBIPMICONSOLE_VERSION_INFO@ \
	$(OTHER_FLAGS)

libipmiconsole_la_LIBADD = \
	$(top_builddir)/common/src/debugutil/libdebugutil.la \
	$(top_builddir)/common/src/miscutil/libmiscutils.la \
	$(top_builddir)/common/src/parsecommon/libparsecommon.la \
	$(top_builddir)/common/src/portability/libfreeipmiportability.la \
	$(top_builddir)/libfreeipmi/src/libfreeipmi.la

libipmiconsole_la_SOURCES = \
	scbuf.c \
	ipmiconsole.c \
	ipmiconsole_checks.c \
	ipmiconsole_ctx.c \
	ipmiconsole_debug.c \
	ipmiconsole_engine.c \
	ipmiconsole_garbage_collector.c \
	ipmiconsole_packet.c \
	ipmiconsole_processing.c \
	ipmiconsole_util.c

$(top_builddir)/common/src/debugutil/libdebugutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/miscutil/libmiscutils.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/portability/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/src/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:

install-data-hook: $(INSTALL_LOGDIR)

install-logdir:
	$(INSTALL) -m 755 -d $(DESTDIR)$(localstatedir)/log/ipmiconsole

EXTRA_DIST = ipmiconsole.map
