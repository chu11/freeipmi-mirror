noinst_LTLIBRARIES = librecordformat.la

librecordformat_la_LIBADD = \
	$(top_builddir)/common/src/portability/libfreeipmiportability.la

librecordformat_la_CPPFLAGS = \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/libfreeipmi/include \
	-I$(top_builddir)/libfreeipmi/src \
	-D_GNU_SOURCE \
	-D_REENTRANT

librecordformat_la_SOURCES = \
	ipmi-cipher-suite-record-format.c \
	ipmi-sdr-record-format.c \
	ipmi-fru-information-record-format.c \
	ipmi-sel-record-format.c

$(top_builddir)/common/src/portability/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
