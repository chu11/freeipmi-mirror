noinst_HEADERS = \
	ipmi-sel.h \
	ipmi-sel-argp.h \
	ipmi-sel-entry.h 

AM_CPPFLAGS = \
	-I$(top_builddir)/common/src \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

sbin_PROGRAMS = ipmi-sel

ipmi_sel_LDADD = \
	$(top_builddir)/common/src/libtoolcommon.la
 
ipmi_sel_SOURCES = \
	ipmi-sel.c \
	ipmi-sel-argp.c \
	ipmi-sel-entry.c
 
$(top_builddir)/common/src/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
