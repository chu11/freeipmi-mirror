noinst_HEADERS = \
	ipmi-fru.h \
	ipmi-fru-argp.h \
	ipmi-fru-info-area.h \
	ipmi-fru-multirecord-area.h \
	ipmi-fru-util.h

AM_CPPFLAGS = \
	-I$(top_builddir)/common/src/miscutil \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/common/src/toolcommon \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT 

sbin_PROGRAMS = ipmi-fru

ipmi_fru_LDADD = \
	$(top_builddir)/common/src/toolcommon/libtoolcommon.la

ipmi_fru_SOURCES = \
	ipmi-fru.c \
	ipmi-fru-argp.c \
	ipmi-fru-info-area.c \
	ipmi-fru-multirecord-area.c \
	ipmi-fru-util.c

$(top_builddir)/common/src/toolcommon/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
 
