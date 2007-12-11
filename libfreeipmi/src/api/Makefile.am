noinst_LTLIBRARIES = libapi.la

libapi_la_SOURCES = \
	ipmi-api.c \
	ipmi-kcs-driver-api.c \
	ipmi-openipmi-driver-api.c \
	ipmi-ssif-driver-api.c \
	ipmi-chassis-cmds-api.c \
	ipmi-device-global-cmds-api.c \
	ipmi-fru-inventory-device-cmds-api.c \
	ipmi-lan-cmds-api.c \
	ipmi-lan-interface-api.c \
	ipmi-lan-session-util.c \
	ipmi-messaging-support-cmds-api.c \
	ipmi-pef-and-alerting-cmds-api.c \
	ipmi-rmcpplus-support-and-payload-cmds-api.c \
	ipmi-sel-cmds-api.c \
	ipmi-sdr-repository-cmds-api.c \
	ipmi-sensor-cmds-api.c \
	ipmi-serial-modem-cmds-api.c \
	ipmi-sol-cmds-api.c

noinst_HEADERS = \
	ipmi-err-wrappers-api.h \
	ipmi-fiid-wrappers-api.h \
	ipmi-ctx.h \
	ipmi-lan-interface-api.h \
	ipmi-lan-session-util.h \
	ipmi-kcs-driver-api.h \
	ipmi-openipmi-driver-api.h \
	ipmi-ssif-driver-api.h

libapi_la_CFLAGS = -Wall
libapi_la_CPPFLAGS = \
	-I$(srcdir)/.. \
	-I$(srcdir)/../../include \
	-I$(srcdir)/../../../common/src \
	-D_GNU_SOURCE \
	-D_REENTRANT
