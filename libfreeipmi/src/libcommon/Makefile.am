noinst_HEADERS = \
	ipmi-bit-ops.h \
	ipmi-crypt.h \
	ipmi-err-wrappers.h \
	ipmi-fiid-wrappers.h \
	ipmi-md2.h \
	ipmi-md5.h

noinst_LTLIBRARIES = libcommon.la

libcommon_la_LIBADD = \
	$(top_builddir)/common/src/portability/libfreeipmiportability.la

libcommon_la_LDFLAGS = -lgcrypt

libcommon_la_CPPFLAGS = \
	-I$(top_builddir)/libfreeipmi/src \
	-I$(top_builddir)/libfreeipmi/include \
	-I$(top_builddir)/common/src/portability \
	-D_GNU_SOURCE \
	-D_REENTRANT

libcommon_la_SOURCES = \
	ipmi-bit-ops.c \
	ipmi-crypt.c \
	ipmi-md2.c \
	ipmi-md5.c

$(top_builddir)/common/src/portability/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
