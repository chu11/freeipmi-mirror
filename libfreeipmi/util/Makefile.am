noinst_LTLIBRARIES = libutil.la

libutil_la_CPPFLAGS = \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi \
	-D_GNU_SOURCE \
	-D_REENTRANT

libutil_la_SOURCES = \
	ipmi-channel-util.c \
	ipmi-cipher-suite-util.c \
	ipmi-device-types-util.c \
	ipmi-entity-ids-util.c \
	ipmi-error-util.c \
	ipmi-error-dcmi-util.c \
	ipmi-iana-enterprise-numbers-util.c \
	ipmi-ipmb-util.c \
	ipmi-lan-util.c \
	ipmi-outofband-util.c \
	ipmi-rmcpplus-util.c \
	ipmi-sensor-and-event-code-tables-util.c \
	ipmi-sensor-util.c \
	ipmi-sensor-units-util.c \
	ipmi-util.c \
	ipmi-dcmi-util.c \
	rmcp-util.c
