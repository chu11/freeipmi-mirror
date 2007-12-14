noinst_LTLIBRARIES = liblocate.la

liblocate_la_SOURCES = \
	ipmi-locate.c \
	ipmi-locate-acpi-spmi.c \
	ipmi-locate-defaults.c \
	ipmi-locate-dmidecode.c \
	ipmi-locate-pci.c \
	ipmi-locate-smbios.c

liblocate_la_CFLAGS = -Wall
liblocate_la_CPPFLAGS = \
	-I$(srcdir)/.. \
	-I$(srcdir)/../../include \
	-I$(srcdir)/../../../common/src \
	-D_GNU_SOURCE \
	-D_REENTRANT
