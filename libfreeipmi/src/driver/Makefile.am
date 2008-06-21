noinst_HEADERS = \
	ipmi-semaphores.h

noinst_LTLIBRARIES = libdriver.la

# achu
#
# IPMI_DEBUG_IPCKEY is so we can do inband development without the
# real ipckey being installed.

libdriver_la_LIBADD = \
	$(top_builddir)/common/src/portability/libfreeipmiportability.la

libdriver_la_CPPFLAGS = \
	-I$(top_builddir)/libfreeipmi/include \
	-I$(top_builddir)/libfreeipmi/src \
	-I$(top_builddir)/common/src/portability \
	-DIPMI_IPCKEY=\"$(localstatedir)/lib/$(PACKAGE_NAME)/ipckey\" \
	-DIPMI_DEBUG_IPCKEY=\"$(top_builddir)/libfreeipmi/src/ipmi-semaphores.h\" \
	-D_GNU_SOURCE \
	-D_REENTRANT

libdriver_la_SOURCES = \
	ipmi-semaphores.c \
	ipmi-kcs-driver.c \
	ipmi-openipmi-driver.c \
	ipmi-ssif-driver.c

$(top_builddir)/common/src/portability/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
