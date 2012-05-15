sbin_PROGRAMS = ipmi-sel

ipmi_sel_CPPFLAGS = \
	-I$(top_srcdir)/common/toolcommon \
	-I$(top_srcdir)/common/toolevent \
	-I$(top_srcdir)/common/toolhostrange \
	-I$(top_srcdir)/common/tooloem \
	-I$(top_srcdir)/common/toolsdr \
	-I$(top_srcdir)/common/toolsensor \
	-I$(top_srcdir)/common/debugutil \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/parsecommon \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

ipmi_sel_LDADD = \
	$(top_builddir)/common/toolcommon/libtoolcommon.la \
	$(top_builddir)/common/toolhostrange/libtoolhostrange.la \
	$(top_builddir)/common/tooloem/libtooloem.la \
	$(top_builddir)/common/toolsdr/libtoolsdr.la \
	$(top_builddir)/common/toolsensor/libtoolsensor.la \
	$(top_builddir)/common/toolevent/libtoolevent.la \
	$(top_builddir)/common/debugutil/libdebugutil.la \
	$(top_builddir)/common/miscutil/libmiscutil.la \
	$(top_builddir)/common/parsecommon/libparsecommon.la \
	$(top_builddir)/common/portability/libportability.la \
	$(top_builddir)/libipmidetect/libipmidetect.la \
	$(top_builddir)/libfreeipmi/libfreeipmi.la
 
ipmi_sel_SOURCES = \
	ipmi-sel.c \
	ipmi-sel_.h \
	ipmi-sel-argp.c \
	ipmi-sel-argp.h
 
$(top_builddir)/common/toolcommon/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/toolhostrange/libtoolhostrange.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/toolevent/libtoolevent.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/tooloem/libtooloem.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/toolsdr/libtoolsdr.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/toolsensor/libtoolsensor.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/debugutil/libdebugutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/miscutil/libmiscutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/parsecommon/libparsecommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/portability/libportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libipmidetect/libipmidetect.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:
