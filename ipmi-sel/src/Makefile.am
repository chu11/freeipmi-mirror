noinst_HEADERS = \
	ipmi-sel.h \
	ipmi-sel-argp.h \
	ipmi-sel-entry.h 

AM_CPPFLAGS = \
	-I$(top_builddir)/common/src/debugutil \
	-I$(top_builddir)/common/src/miscutil \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/common/src/toolcommon \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

sbin_PROGRAMS = ipmi-sel

ipmi_sel_LDADD = \
	$(top_builddir)/common/src/debugutil/libdebugutil.la \
	$(top_builddir)/common/src/toolcommon/libtoolcommon.la
 
ipmi_sel_SOURCES = \
	ipmi-sel.c \
	ipmi-sel-argp.c \
	ipmi-sel-entry.c
 
$(top_builddir)/common/src/toolcommon/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:
