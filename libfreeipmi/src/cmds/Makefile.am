noinst_LTLIBRARIES = libcmds.la

libcmds_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/src \
	-D_GNU_SOURCE \
	-D_REENTRANT

libcmds_la_SOURCES = \
	ipmi-bmc-watchdog-timer-cmds.c \
	ipmi-chassis-cmds.c \
	ipmi-device-global-cmds.c \
	ipmi-event-cmds.c \
	ipmi-fru-inventory-device-cmds.c \
	ipmi-lan-cmds.c \
	ipmi-messaging-support-cmds.c \
	ipmi-pef-and-alerting-cmds.c \
	ipmi-rmcpplus-support-and-payload-cmds.c \
	ipmi-sdr-repository-cmds.c \
	ipmi-sel-cmds.c \
	ipmi-sensor-cmds.c \
	ipmi-serial-modem-cmds.c \
	ipmi-sol-cmds.c \
	rmcp-cmds.c

