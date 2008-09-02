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
	-D_GNU_SOURCE \
	-D_REENTRANT

libfreeipmi_la_LIBADD = \
	$(top_builddir)/common/src/debugutil/libdebugutil.la \
	$(top_builddir)/common/src/miscutil/libmiscutils.la \
	$(top_builddir)/common/src/portability/libfreeipmiportability.la \
	$(top_builddir)/libfreeipmi/src/api/libapi.la \
	$(top_builddir)/libfreeipmi/src/cmds/libcmds.la \
	$(top_builddir)/libfreeipmi/src/debug/libdebug.la \
	$(top_builddir)/libfreeipmi/src/driver/libdriver.la \
	$(top_builddir)/libfreeipmi/src/fiid/libfiid.la \
	$(top_builddir)/libfreeipmi/src/interface/libinterface.la \
	$(top_builddir)/libfreeipmi/src/libcommon/libcommon.la \
	$(top_builddir)/libfreeipmi/src/locate/liblocate.la \
	$(top_builddir)/libfreeipmi/src/record-format/librecordformat.la \
	$(top_builddir)/libfreeipmi/src/sdr-cache/libsdrcache.la \
	$(top_builddir)/libfreeipmi/src/spec/libspec.la \
	$(top_builddir)/libfreeipmi/src/util/libutil.la

$(top_builddir)/common/src/debugutil/libdebugutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/miscutil/libmiscutils.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/portability/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/src/api/libapi.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/src/cmds/libcmds.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/src/debug/libdebug.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/src/driver/libdriver.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/src/fiid/libfiid.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/src/interface/libinterface.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/src/libcommon/libcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/src/locate/liblocate.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/src/record-format/librecordformat.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/src/sdr-cache/libsdrcache.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/src/spec/libspec.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/src/util/libutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:

libfreeipmi_la_SOURCES = 

EXTRA_DIST = freeipmi.map
