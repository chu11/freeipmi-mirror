noinst_LTLIBRARIES = libspec.la

libspec_la_SOURCES = \
	ipmi-fru-chassis-types-spec.c \
	ipmi-fru-language-codes-spec.c \
	ipmi-sensor-types-spec.c \
	ipmi-sensor-units-spec.c

libspec_la_CFLAGS = -Wall
libspec_la_CPPFLAGS = \
	-I$(srcdir)/.. \
	-I$(srcdir)/../../include \
	-I$(srcdir)/../../../common/src \
	-D_GNU_SOURCE \
	-D_REENTRANT
