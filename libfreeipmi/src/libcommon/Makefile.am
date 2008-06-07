noinst_HEADERS = \
	ipmi-bit-ops.h \
	ipmi-crypt.h \
	ipmi-err-wrappers.h \
	ipmi-fiid-wrappers.h \
	ipmi-md2.h \
	ipmi-md5.h

noinst_LTLIBRARIES = libcommon.la

libcommon_la_LDFLAGS = -lgcrypt

libcommon_la_CPPFLAGS = \
	-I$(top_builddir)/libfreeipmi/src \
	-I$(top_builddir)/libfreeipmi/include \
	-I$(top_builddir)/common/src \
	-D_GNU_SOURCE \
	-D_REENTRANT

libcommon_la_SOURCES = \
	ipmi-bit-ops.c \
	ipmi-crypt.c \
	ipmi-md2.c \
	ipmi-md5.c
