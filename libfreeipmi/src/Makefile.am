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

libfreeipmi_la_LDFLAGS = -module -version-info @LIBFREEIPMI_VERSION_INFO@ $(VERSION_FLAGS) -lm 
# libfreeipmi_la_CPPFLAGS = 

libfreeipmi_la_SOURCES = 

libfreeipmi_la_CPPFLAGS = \
	-I$(srcdir)/../include \
	-I$(srcdir)/../../common/src \
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
	../../common/src/libfreeipmiportability.la \
	../../common/src/libmiscutils.la \
	../../common/src/libdebugcommon.la

api/libapi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

cmds/libcmds.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

debug/libdebug.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

driver/libdriver.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

fiid/libfiid.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

interface/libinterface.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

libcommon/libcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

locate/liblocate.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

record-format/librecordformat.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

sdr-cache/libsdrcache.la : force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

spec/libspec.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

util/libutil.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libfreeipmiportability.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libmiscutils.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libdebugcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

EXTRA_DIST = freeipmi.map

force-dependency-check:
