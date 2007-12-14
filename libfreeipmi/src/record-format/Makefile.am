noinst_LTLIBRARIES = librecordformat.la

librecordformat_la_SOURCES = \
	ipmi-cipher-suite-record-format.c \
	ipmi-sdr-record-format.c \
	ipmi-fru-information-record-format.c \
	ipmi-sel-record-format.c

librecordformat_la_CFLAGS = -Wall
librecordformat_la_CPPFLAGS = \
	-I$(srcdir)/.. \
	-I$(srcdir)/../../include \
	-I$(srcdir)/../../../common/src \
	-D_GNU_SOURCE \
	-D_REENTRANT
