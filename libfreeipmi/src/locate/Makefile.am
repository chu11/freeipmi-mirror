noinst_LTLIBRARIES = liblocate.la

liblocate_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/src \
	-D_GNU_SOURCE \
	-D_REENTRANT

liblocate_la_SOURCES = \
	ipmi-locate.c \
	ipmi-locate-acpi-spmi.c \
	ipmi-locate-defaults.c \
	ipmi-locate-dmidecode.c \
	ipmi-locate-pci.c \
	ipmi-locate-smbios.c
