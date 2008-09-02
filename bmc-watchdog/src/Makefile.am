noinst_HEADERS = \
	bmc-watchdog.h \
	bmc-watchdog-argp.h

AM_CPPFLAGS = \
	-I$(top_builddir)/common/src/debugutil \
	-I$(top_builddir)/common/src/miscutil \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/common/src/toolcommon \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

sbin_PROGRAMS = bmc-watchdog

bmc_watchdog_LDADD = \
	$(top_builddir)/common/src/debugutil/libdebugutil.la \
	$(top_builddir)/common/src/toolcommon/libtoolcommon.la

bmc_watchdog_SOURCES = \
	bmc-watchdog.c \
	bmc-watchdog-argp.c

$(top_builddir)/common/src/debugutil/libdebugutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/toolcommon/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:
