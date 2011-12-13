noinst_HEADERS = \
	ipmi-bit-ops.h \
	ipmi-crypt.h \
	ipmi-fiid-util.h \
	ipmi-fill-util.h \
	ipmi-md2.h \
	ipmi-md5.h \
	ipmi-trace.h

noinst_LTLIBRARIES = libcommon.la

libcommon_la_LDFLAGS = @GCRYPT_LIBS@

libcommon_la_CPPFLAGS = \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi \
	-D_GNU_SOURCE \
	-D_REENTRANT

libcommon_la_SOURCES = \
	ipmi-bit-ops.c \
	ipmi-crypt.c \
	ipmi-fiid-util.c \
	ipmi-md2.c \
	ipmi-md5.c
