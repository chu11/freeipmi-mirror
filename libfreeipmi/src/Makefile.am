SUBDIRS = \
	api \
	cmds \
	debug \
	driver \
	fiid \
	interface \
	libcommon \
	locate \
	record-format \
	sdr-cache \
	spec \
	util

if WITH_GNU_LD
VERSION_SCRIPT = $(srcdir)/freeipmi.map
VERSION_FLAGS = -Wl,--version-script=$(VERSION_SCRIPT)
endif

lib_LTLIBRARIES = libfreeipmi.la

libfreeipmi_la_LDFLAGS = \
	-module \
	-version-info @LIBFREEIPMI_VERSION_INFO@ $(VERSION_FLAGS) \
	$(OTHER_FLAGS) \
	-lm 

libfreeipmi_la_CPPFLAGS = \
	-I$(top_builddir)/libfreeipmi/include \
	-I$(top_builddir)/common/src \
	-D_GNU_SOURCE \
	-D_REENTRANT

libfreeipmi_la_LIBADD = \
	api/libapi.la \
	cmds/libcmds.la \
	debug/libdebug.la \
	driver/libdriver.la \
	fiid/libfiid.la \
	interface/libinterface.la \
	libcommon/libcommon.la \
	locate/liblocate.la \
	record-format/librecordformat.la \
	sdr-cache/libsdrcache.la \
	spec/libspec.la \
	util/libutil.la \
	$(top_builddir)/common/src/libmiscutils.la \
	$(top_builddir)/common/src/libfreeipmiportability.la \
	$(top_builddir)/common/src/libdebugcommon.la

$(top_builddir)/common/src/libmiscutils.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/common/src/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/common/src/libdebugcommon.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:

libfreeipmi_la_SOURCES = 

EXTRA_DIST = freeipmi.map
