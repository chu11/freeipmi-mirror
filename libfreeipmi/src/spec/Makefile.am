noinst_LTLIBRARIES = libspec.la

libspec_la_CPPFLAGS = \
	-I$(top_builddir)/libfreeipmi/src \
	-I$(top_builddir)/libfreeipmi/include \
	-I$(top_builddir)/common/src \
	-I$(top_builddir)/common/src/portability \
	-D_GNU_SOURCE \
	-D_REENTRANT

libspec_la_SOURCES = \
	ipmi-fru-chassis-types-spec.c \
	ipmi-fru-language-codes-spec.c \
	ipmi-sensor-types-spec.c \
	ipmi-sensor-units-spec.c
