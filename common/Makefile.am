noinst_LTLIBRARIES = \
	src/configtool/libconfigtoolcommon.la \
	src/debugutil/libdebugutil.la \
	src/miscutil/libmiscutil.la \
	src/parsecommon/libparsecommon.la \
	src/pingtool/libpingtoolcommon.la \
	src/portability/libportability.la \
	src/toolcommon/libtoolcommon.la \
	src/toolhostrange/libtoolhostrange.la \
	src/tooloem/libtooloem.la \
	src/toolsdr/libtoolsdr.la \
	src/toolsensor/libtoolsensor.la

src_configtool_libconfigtoolcommon_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/parsecommon \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/common/src/toolcommon \
	-I$(top_srcdir)/common/src/toolhostrange \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

src_configtool_libconfigtoolcommon_la_SOURCES = \
	src/configtool/config-tool-argp.c \
	src/configtool/config-tool-argp.h \
	src/configtool/config-tool-comment.c \
	src/configtool/config-tool-comment.h \
	src/configtool/config-tool-commit.c \
	src/configtool/config-tool-commit.h \
	src/configtool/config-tool-common.h \
	src/configtool/config-tool-checkout.c \
	src/configtool/config-tool-checkout.h \
	src/configtool/config-tool-diff.c \
	src/configtool/config-tool-diff.h \
	src/configtool/config-tool-parse.c \
	src/configtool/config-tool-parse.h \
	src/configtool/config-tool-pef-conf-section.c \
	src/configtool/config-tool-pef-conf-section.h \
	src/configtool/config-tool-section.c \
	src/configtool/config-tool-section.h \
	src/configtool/config-tool-utils.c \
	src/configtool/config-tool-utils.h \
	src/configtool/config-tool-validate.c \
	src/configtool/config-tool-validate.h

src_debugutil_libdebugutil_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

src_debugutil_libdebugutil_la_SOURCES = \
        src/debugutil/debug-util.c \
        src/debugutil/debug-util.h

src_miscutil_libmiscutil_la_CPPFLAGS = \
	-D_GNU_SOURCE \
	-D_REENTRANT

src_miscutil_libmiscutil_la_SOURCES = \
	src/miscutil/cbuf.c \
	src/miscutil/cbuf.h \
	src/miscutil/conffile.c \
	src/miscutil/conffile.h \
	src/miscutil/error.c \
	src/miscutil/error.h \
	src/miscutil/fd.c \
	src/miscutil/fd.h \
	src/miscutil/hash.c \
	src/miscutil/hash.h \
	src/miscutil/hostlist.c \
	src/miscutil/hostlist.h \
	src/miscutil/list.c \
	src/miscutil/list.h \
	src/miscutil/secure.c \
	src/miscutil/secure.h \
	src/miscutil/thread.c \
	src/miscutil/thread.h \
	src/miscutil/timeval.c \
	src/miscutil/timeval.h

src_parsecommon_libparsecommon_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

src_parsecommon_libparsecommon_la_SOURCES = \
	src/parsecommon/parse-common.c \
	src/parsecommon/parse-common.h

src_pingtool_libpingtoolcommon_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/common/src/toolcommon \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE

src_pingtool_libpingtoolcommon_la_SOURCES = \
	src/pingtool/ping-tool-common.c \
	src/pingtool/ping-tool-common.h

src_portability_libportability_la_CPPFLAGS = \
	-D_GNU_SOURCE \
	-D_REENTRANT

if WITH_FREEIPMI_ARGP
FREEIPMI_ARG_SOURCES = \
	src/portability/freeipmi-argp.h \
	src/portability/freeipmi-argp-ba.c \
	src/portability/freeipmi-argp-eexst.c \
	src/portability/freeipmi-argp-fmtstream.c \
	src/portability/freeipmi-argp-fmtstream.h \
	src/portability/freeipmi-argp-namefrob.h \
	src/portability/freeipmi-argp-help.c \
	src/portability/freeipmi-argp-parse.c \
	src/portability/freeipmi-argp-pv.c \
	src/portability/freeipmi-argp-pvh.c
endif

src_portability_libportability_la_SOURCES = \
	src/portability/freeipmi-portability.c \
	src/portability/freeipmi-portability.h \
	$(FREEIPMI_ARG_SOURCES)

src_toolcommon_libtoolcommon_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/parsecommon \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/common/src/toolhostrange \
	-I$(top_srcdir)/common/src/toolsdr \
	-I$(top_srcdir)/common/src/toolsensor \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

src_toolcommon_libtoolcommon_la_SOURCES = \
	src/toolcommon/tool-common.c \
	src/toolcommon/tool-common.h \
	src/toolcommon/tool-config-file-common.c \
	src/toolcommon/tool-config-file-common.h \
	src/toolcommon/tool-cmdline-common.c \
	src/toolcommon/tool-cmdline-common.h

src_toolhostrange_libtoolhostrange_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libipmidetect/src \
	-D_GNU_SOURCE \
	-D_REENTRANT

src_toolhostrange_libtoolhostrange_la_LDFLAGS = \
	$(PTHREAD_LIBS)

src_toolhostrange_libtoolhostrange_la_SOURCES = \
	src/toolhostrange/pstdout.c \
	src/toolhostrange/pstdout.h \
	src/toolhostrange/tool-hostrange-common.c \
	src/toolhostrange/tool-hostrange-common.h

src_tooloem_libtooloem_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/common/src/toolhostrange \
	-I$(top_srcdir)/libfreeipmi/include  \
	-D_GNU_SOURCE \
	-D_REENTRANT

src_tooloem_libtooloem_la_SOURCES = \
	src/tooloem/tool-oem-common.c \
	src/tooloem/tool-oem-common.h

src_toolsdr_libtoolsdr_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/common/src/toolhostrange \
	-I$(top_srcdir)/libfreeipmi/include  \
	-D_GNU_SOURCE \
	-D_REENTRANT

src_toolsdr_libtoolsdr_la_SOURCES = \
	src/toolsdr/tool-sdr-cache-common.c \
	src/toolsdr/tool-sdr-cache-common.h

src_toolsensor_libtoolsensor_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/common/src/toolhostrange \
	-I$(top_srcdir)/common/src/toolsdr \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

src_toolsensor_libtoolsensor_la_SOURCES = \
	src/toolsensor/tool-sensor-common.c \
	src/toolsensor/tool-sensor-common.h

EXTRA_DIST = \
	man/manpage-common-general-options-header.man \
	man/manpage-common-driver.man \
	man/manpage-common-inband.man \
	man/manpage-common-outofband-hostname.man \
	man/manpage-common-outofband-hostname-hostranged.man \
	man/manpage-common-outofband-username-user.man \
	man/manpage-common-outofband-username-operator.man \
	man/manpage-common-outofband-username-admin.man \
	man/manpage-common-outofband-password.man \
	man/manpage-common-outofband-k-g.man \
	man/manpage-common-outofband-session-timeout.man \
	man/manpage-common-outofband-retransmission-timeout.man \
	man/manpage-common-authentication-type.man \
	man/manpage-common-cipher-suite-id-main.man \
	man/manpage-common-cipher-suite-id-details.man \
	man/manpage-common-privilege-level-user.man \
	man/manpage-common-privilege-level-operator.man \
	man/manpage-common-privilege-level-admin.man \
	man/manpage-common-config-file.man \
	man/manpage-common-workaround-flags.man \
	man/manpage-common-workaround-heading-text.man \
	man/manpage-common-workaround-inband-text.man \
	man/manpage-common-workaround-outofband-text.man \
	man/manpage-common-workaround-sol-only-text.man \
	man/manpage-common-workaround-config-tool.man \
	man/manpage-common-debug.man \
	man/manpage-common-misc.man \
	man/manpage-common-hostranged-options-header.man \
	man/manpage-common-hostranged-buffer.man \
	man/manpage-common-hostranged-consolidate.man \
	man/manpage-common-hostranged-fanout.man \
	man/manpage-common-hostranged-eliminate.man \
	man/manpage-common-hostranged-always-prefix.man \
	man/manpage-common-hostranged-text-main.man \
	man/manpage-common-hostranged-text-options.man \
	man/manpage-common-hostranged-text-localhost.man \
	man/manpage-common-hostranged-text-threads.man \
	man/manpage-common-sdr-options.man \
	man/manpage-common-ignore-sdr-cache.man \
	man/manpage-common-config-tool-options.man \
	man/manpage-common-config-tool-options-verbose.man \
	man/manpage-common-config-tool-options-very-verbose.man \
	man/manpage-common-config-tool-options-lan-channel-number.man \
	man/manpage-common-config-tool-options-serial-channel-number.man \
	man/manpage-common-config-tool-options-sol-channel-number.man \
	man/manpage-common-config-tool-general-use.man \
	man/manpage-common-troubleshooting.man \
	man/manpage-common-troubleshooting-lan-only.man \
	man/manpage-common-known-issues.man \
	man/manpage-common-known-issues-ping.man \
	man/manpage-common-reporting-bugs.man \
	man/manpage-common-gpl-library-text.man \
	man/manpage-common-gpl-program-text.man \
	man/manpage-common-gpl-freeipmi-text.man \
	man/manpage-common-ping-options.man \
	man/manpage-common-ping-origin.man \
	man/manpage-common-table-of-contents.man \
	man/manpage-common-homepage.man \
	man/manpage-common-interpret-oem-data.man \
	man/manpage-common-oem-interpretation.man \
	man/manpage-common-entity-sensor-names.man \
	man/manpage-common-no-sensor-type-output.man \
	man/manpage-common-comma-separated-output.man \
	man/manpage-common-no-header-output.man \
	man/manpage-common-non-abbreviated-units.man \
	man/manpage-common-legacy-output.man
