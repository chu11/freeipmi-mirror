noinst_HEADERS = \
	bmc-device.h \
	bmc-device-argp.h

AM_CPPFLAGS = \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/common/src/toolcommon \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT 

sbin_PROGRAMS = bmc-device

bmc_device_LDADD = \
	$(top_builddir)/common/src/toolcommon/libtoolcommon.la

bmc_device_SOURCES = \
	bmc-device.c \
	bmc-device-argp.c 

$(top_builddir)/common/src/toolcommon/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:
