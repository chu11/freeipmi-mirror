noinst_LTLIBRARIES = libdriver.la

libdriver_la_SOURCES = \
	ipmi-semaphores.c \
	ipmi-kcs-driver.c \
	ipmi-openipmi-driver.c \
	ipmi-ssif-driver.c

noinst_HEADERS = \
	ipmi-inband.h \
	ipmi-semaphores.h

# achu
#
# IPMI_DEBUG_IPCKEY is so we can do inband development without the real ipckey being
# installed.

libdriver_la_CPPFLAGS = \
	-I$(srcdir)/.. \
	-I$(srcdir)/../../include \
	-I$(srcdir)/../../../common/src \
	-DIPMI_IPCKEY=\"$(localstatedir)/lib/$(PACKAGE_NAME)/ipckey\" \
	-DIPMI_DEBUG_IPCKEY=\"$(top_builddir)/libfreeipmi/src/ipmi-semaphores.h\" \
	-D_GNU_SOURCE \
	-D_REENTRANT
