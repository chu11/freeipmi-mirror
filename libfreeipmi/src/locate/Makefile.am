noinst_LTLIBRARIES = liblocate.la

liblocate_la_CPPFLAGS = \
	-I$(top_builddir)/libfreeipmi/src \
	-I$(top_builddir)/libfreeipmi/include \
	-I$(top_builddir)/common/src \
	-D_GNU_SOURCE \
	-D_REENTRANT

liblocate_la_SOURCES = \
	ipmi-locate.c \
	ipmi-locate-acpi-spmi.c \
	ipmi-locate-defaults.c \
	ipmi-locate-dmidecode.c \
	ipmi-locate-pci.c \
	ipmi-locate-smbios.c
