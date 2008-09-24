noinst_HEADERS = \
	config-tool-argp.h \
	config-tool-common.h \
	config-tool-comment.h \
	config-tool-checkout.h \
	config-tool-commit.h \
	config-tool-diff.h \
	config-tool-parse.h \
	config-tool-pef-conf-section.h \
	config-tool-section.h \
	config-tool-utils.h \
	config-tool-validate.h

noinst_LTLIBRARIES = \
	libconfigtoolcommon.la

libconfigtoolcommon_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/common/src/toolcommon \
	-I$(top_srcdir)/common/src/toolhostrange \
	-I$(top_srcdir)/libfreeipmi/include

libconfigtoolcommon_la_SOURCES = \
	config-tool-argp.c \
	config-tool-comment.c \
	config-tool-checkout.c \
	config-tool-commit.c \
	config-tool-diff.c \
	config-tool-parse.c \
	config-tool-pef-conf-section.c \
	config-tool-section.c \
	config-tool-utils.c \
	config-tool-validate.c
