if WITH_PKG_CONFIG
pkgconfig_DATA = libipmiconsole.pc
endif

if WITH_GNU_LD
VERSION_SCRIPT = $(srcdir)/ipmiconsole.map
OTHER_FLAGS = -Wl,--version-script=$(VERSION_SCRIPT)
endif

nodist_include_HEADERS = ipmiconsole.h

lib_LTLIBRARIES = libipmiconsole.la

libipmiconsole_la_CFLAGS = $(PTHREAD_CFLAGS)

libipmiconsole_la_CPPFLAGS = \
	-I$(top_srcdir)/common/debugutil \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/parsecommon \
	-I$(top_srcdir)/common/portability \
        -I$(top_builddir)/libfreeipmi/include \
        -I$(top_srcdir)/libfreeipmi/include \
	-DWITH_PTHREADS=1 \
	-D_GNU_SOURCE \
	-D_REENTRANT

libipmiconsole_la_LDFLAGS = \
	$(PTHREAD_LIBS) \
	-version-info @LIBIPMICONSOLE_VERSION_INFO@ \
	$(OTHER_FLAGS)

libipmiconsole_la_LIBADD = \
	$(top_builddir)/common/debugutil/libdebugutil.la \
	$(top_builddir)/common/miscutil/libmiscutil.la \
	$(top_builddir)/common/parsecommon/libparsecommon.la \
	$(top_builddir)/common/portability/libportability.la \
	$(top_builddir)/libfreeipmi/libfreeipmi.la

libipmiconsole_la_SOURCES = \
	ipmiconsole.c \
	ipmiconsole_checks.c \
	ipmiconsole_checks.h \
	ipmiconsole_ctx.c \
	ipmiconsole_ctx.h \
	ipmiconsole_debug.c \
	ipmiconsole_debug.h \
	ipmiconsole_defs.h \
	ipmiconsole_engine.c \
	ipmiconsole_engine.h \
	ipmiconsole_garbage_collector.c \
	ipmiconsole_garbage_collector.h \
	ipmiconsole_packet.c \
	ipmiconsole_packet.h \
	ipmiconsole_processing.c \
	ipmiconsole_processing.h \
	ipmiconsole_util.c \
	ipmiconsole_util.h \
	scbuf.c \
	scbuf.h

$(top_builddir)/common/debugutil/libdebugutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/miscutil/libmiscutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/portability/libportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:

install-data-hook: $(INSTALL_LOGDIR)

install-logdir:
	$(INSTALL) -m 755 -d $(DESTDIR)$(localstatedir)/log/ipmiconsole

EXTRA_DIST = ipmiconsole.map
