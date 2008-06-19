noinst_HEADERS = \
	bmc-device.h \
	bmc-device-argp.h

AM_CPPFLAGS = \
	-I$(top_builddir)/common/src \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT 

sbin_PROGRAMS = bmc-device

bmc_device_LDADD = \
	$(top_builddir)/common/src/libtoolcommon.la

bmc_device_SOURCES = \
	bmc-device.c \
	bmc-device-argp.c 

$(top_builddir)/common/src/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
