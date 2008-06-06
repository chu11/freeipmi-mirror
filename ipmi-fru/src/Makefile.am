noinst_HEADERS = \
	ipmi-fru.h \
	ipmi-fru-argp.h \
	ipmi-fru-info-area.h \
	ipmi-fru-multirecord-area.h \
	ipmi-fru-util.h

AM_CPPFLAGS = \
	-I$(top_builddir)/common/src \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT 

sbin_PROGRAMS = ipmi-fru

ipmi_fru_LDADD = \
	$(top_builddir)/common/src/libtoolcommon.la \
	$(top_builddir)/common/src/libhostrange.la \
	$(top_builddir)/common/src/libmiscutils.la \
	$(top_builddir)/common/src/libfreeipmiportability.la \
	$(top_builddir)/ipmidetect/src/libipmidetect/libipmidetect.la \
	$(top_builddir)/libfreeipmi/src/libfreeipmi.la

ipmi_fru_SOURCES = \
	ipmi-fru.c \
	ipmi-fru-argp.c \
	ipmi-fru-info-area.c \
	ipmi-fru-multirecord-area.c \
	ipmi-fru-util.c

$(top_builddir)/common/src/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/common/src/libhostrange.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/common/src/libmiscutils.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/common/src/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/ipmidetect/src/libipmidetect/libipmidetect.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/libfreeipmi/src/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
 
