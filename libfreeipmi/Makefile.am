SUBDIRS = \
	api \
	cmds \
	debug \
	driver \
	fiid \
	fru-parse \
	include \
	interface \
	interpret \
	libcommon \
	locate \
	payload \
	record-format \
	sdr-cache \
	sdr-parse \
	sel-parse \
	sensor-read \
	spec \
	util

if WITH_PKG_CONFIG
pkgconfig_DATA = libfreeipmi.pc
endif

if WITH_GNU_LD
VERSION_SCRIPT = $(srcdir)/freeipmi.map
VERSION_FLAGS = -Wl,--version-script=$(VERSION_SCRIPT)
endif

lib_LTLIBRARIES = libfreeipmi.la

libfreeipmi_la_LDFLAGS = \
	-version-info @LIBFREEIPMI_VERSION_INFO@ $(VERSION_FLAGS) \
	$(OTHER_FLAGS) \
	-lm 

libfreeipmi_la_CPPFLAGS = \
	-D_GNU_SOURCE \
	-D_REENTRANT

libfreeipmi_la_LIBADD = \
	$(top_builddir)/common/debugutil/libdebugutil.la \
	$(top_builddir)/common/miscutil/libmiscutils.la \
	$(top_builddir)/common/portability/libfreeipmiportability.la \
	$(top_builddir)/libfreeipmi/api/libapi.la \
	$(top_builddir)/libfreeipmi/cmds/libcmds.la \
	$(top_builddir)/libfreeipmi/debug/libdebug.la \
	$(top_builddir)/libfreeipmi/driver/libdriver.la \
	$(top_builddir)/libfreeipmi/fiid/libfiid.la \
	$(top_builddir)/libfreeipmi/fru-parse/libfruparse.la \
	$(top_builddir)/libfreeipmi/interface/libinterface.la \
	$(top_builddir)/libfreeipmi/interpret/libinterpret.la \
	$(top_builddir)/libfreeipmi/libcommon/libcommon.la \
	$(top_builddir)/libfreeipmi/locate/liblocate.la \
	$(top_builddir)/libfreeipmi/payload/libpayload.la \
	$(top_builddir)/libfreeipmi/record-format/librecordformat.la \
	$(top_builddir)/libfreeipmi/sdr-cache/libsdrcache.la \
	$(top_builddir)/libfreeipmi/sdr-parse/libsdrparse.la \
	$(top_builddir)/libfreeipmi/sel-parse/libselparse.la \
	$(top_builddir)/libfreeipmi/sensor-read/libsensorread.la \
	$(top_builddir)/libfreeipmi/spec/libspec.la \
	$(top_builddir)/libfreeipmi/util/libutil.la

libfreeipmi_la_SOURCES = 

$(top_builddir)/common/debugutil/libdebugutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/miscutil/libmiscutils.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/portability/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/api/libapi.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/cmds/libcmds.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/debug/libdebug.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/driver/libdriver.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/fiid/libfiid.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/fru-parse/libfruparse.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/interface/libinterface.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/interpret/libinterpret.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/libcommon/libcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/locate/liblocate.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/payload/libpayload.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/record-format/librecordformat.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/sdr-cache/libsdrcache.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/sdr-parse/libsdrparse.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/sel-parse/libselparse.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/sensor-read/libsensorread.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/spec/libspec.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/util/libutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:

EXTRA_DIST = freeipmi.map ipckey

install-data-local:
	$(INSTALL) -m 755 -d $(DESTDIR)$(localstatedir)/lib/$(PACKAGE_NAME)
	$(INSTALL) -m 644 $(srcdir)/ipckey $(DESTDIR)$(localstatedir)/lib/$(PACKAGE_NAME)/ipckey

uninstall-local:
	$(RM) -f $(DESTDIR)$(localstatedir)/lib/$(PACKAGE_NAME)/ipckey



