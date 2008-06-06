noinst_LTLIBRARIES = libcommon.la

libcommon_la_SOURCES = \
	ipmi-bit-ops.c \
	ipmi-crypt.c \
	ipmi-md2.c \
	ipmi-md5.c

noinst_HEADERS = \
	ipmi-bit-ops.h \
	ipmi-crypt.h \
	ipmi-err-wrappers.h \
	ipmi-fiid-wrappers.h \
	ipmi-md2.h \
	ipmi-md5.h

libcommon_la_LDFLAGS = -lgcrypt
libcommon_la_CPPFLAGS = \
	-I$(srcdir)/.. \
	-I$(srcdir)/../../include \
	-I$(srcdir)/../../../common/src \
	-D_GNU_SOURCE \
	-D_REENTRANT

