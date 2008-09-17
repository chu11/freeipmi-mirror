noinst_HEADERS = \
	ipmi-err-wrappers-api.h \
	ipmi-fiid-wrappers-api.h \
	ipmi-ctx.h \
	ipmi-lan-interface-api.h \
	ipmi-lan-session-common.h \
	ipmi-kcs-driver-api.h \
	ipmi-openipmi-driver-api.h \
	ipmi-ssif-driver-api.h \
	ipmi-sunbmc-driver-api.h

noinst_LTLIBRARIES = libapi.la

libapi_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/debugutil \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/src \
	-D_GNU_SOURCE \
	-D_REENTRANT

libapi_la_SOURCES = \
	ipmi-api.c \
	ipmi-kcs-driver-api.c \
	ipmi-openipmi-driver-api.c \
	ipmi-ssif-driver-api.c \
	ipmi-sunbmc-driver-api.c \
	ipmi-chassis-cmds-api.c \
	ipmi-device-global-cmds-api.c \
	ipmi-event-cmds-api.c \
	ipmi-fru-inventory-device-cmds-api.c \
	ipmi-lan-cmds-api.c \
	ipmi-lan-interface-api.c \
	ipmi-lan-session-common.c \
	ipmi-messaging-support-cmds-api.c \
	ipmi-pef-and-alerting-cmds-api.c \
	ipmi-rmcpplus-support-and-payload-cmds-api.c \
	ipmi-sel-cmds-api.c \
	ipmi-sdr-repository-cmds-api.c \
	ipmi-sensor-cmds-api.c \
	ipmi-serial-modem-cmds-api.c \
	ipmi-sol-cmds-api.c

