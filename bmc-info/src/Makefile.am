noinst_HEADERS = \
	bmc-info.h \
	bmc-info-argp.h 

AM_CPPFLAGS = \
	-I$(top_builddir)/common/src/miscutil \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/common/src/toolcommon \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT 

sbin_PROGRAMS = bmc-info

bmc_info_LDADD = \
	$(top_builddir)/common/src/toolcommon/libtoolcommon.la
 
bmc_info_SOURCES = \
	bmc-info.c \
	bmc-info-argp.c 

$(top_builddir)/common/src/toolcommon/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:
