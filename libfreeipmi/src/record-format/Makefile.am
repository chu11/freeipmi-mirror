noinst_LTLIBRARIES = librecordformat.la

librecordformat_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/src \
	-D_GNU_SOURCE \
	-D_REENTRANT

librecordformat_la_SOURCES = \
	ipmi-cipher-suite-record-format.c \
	ipmi-sdr-record-format.c \
	ipmi-fru-information-record-format.c \
	ipmi-sel-record-format.c
