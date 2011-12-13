noinst_LTLIBRARIES = libspec.la

libspec_la_CPPFLAGS = \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi \
	-D_GNU_SOURCE \
	-D_REENTRANT

libspec_la_SOURCES = \
	ipmi-device-types-spec.c \
	ipmi-entity-ids-spec.c \
	ipmi-fru-chassis-types-spec.c \
	ipmi-fru-language-codes-spec.c \
	ipmi-iana-enterprise-numbers-spec.c \
	ipmi-sensor-and-event-code-tables-spec.c \
	ipmi-sensor-and-event-code-tables-oem-spec.c \
	ipmi-sensor-types-spec.c \
	ipmi-sensor-units-spec.c
