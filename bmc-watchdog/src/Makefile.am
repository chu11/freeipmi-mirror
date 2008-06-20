noinst_HEADERS = \
	bmc-watchdog.h \
	bmc-watchdog-argp.h

AM_CPPFLAGS = \
	-I$(top_builddir)/common/src \
	-I$(top_builddir)/common/src/debugutil \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

sbin_PROGRAMS = bmc-watchdog

bmc_watchdog_LDADD = \
	$(top_builddir)/common/src/libtoolcommon.la \
	$(top_builddir)/common/src/debugutil/libdebugutil.la

bmc_watchdog_SOURCES = \
	bmc-watchdog.c \
	bmc-watchdog-argp.c

$(top_builddir)/common/src/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/common/src/debugutil/libdebugutil.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
