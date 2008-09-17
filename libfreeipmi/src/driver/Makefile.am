noinst_HEADERS = \
	ipmi-semaphores.h

noinst_LTLIBRARIES = libdriver.la

# achu
#
# IPMI_DEBUG_IPCKEY is so we can do inband development without the
# real ipckey being installed.

libdriver_la_CPPFLAGS = \
	-I$(top_srcdir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/src \
	-I$(top_srcdir)/common/src/portability \
	-DIPMI_IPCKEY=\"$(localstatedir)/lib/$(PACKAGE_NAME)/ipckey\" \
	-DIPMI_DEBUG_IPCKEY=\"$(top_builddir)/libfreeipmi/src/ipmi-semaphores.h\" \
	-D_GNU_SOURCE \
	-D_REENTRANT

libdriver_la_SOURCES = \
	ipmi-semaphores.c \
	ipmi-kcs-driver.c \
	ipmi-openipmi-driver.c \
	ipmi-sunbmc-driver.c \
	ipmi-ssif-driver.c
