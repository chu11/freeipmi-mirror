noinst_LTLIBRARIES = \
	libconfigtool.la

libconfigtool_la_CPPFLAGS = \
	-I$(top_srcdir)/common/parsecommon \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/common/toolcommon \
	-I$(top_srcdir)/common/toolhostrange \
	-I$(top_builddir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

libconfigtool_la_SOURCES = \
	config-tool-argp.c \
	config-tool-argp.h \
	config-tool-common.h \
	config-tool-comment.c \
	config-tool-comment.h \
	config-tool-checkout.c \
	config-tool-checkout.h \
	config-tool-commit.c \
	config-tool-commit.h \
	config-tool-diff.c \
	config-tool-diff.h \
	config-tool-parse.c \
	config-tool-parse.h \
	config-tool-pef-conf-section.c \
	config-tool-pef-conf-section.h \
	config-tool-section.c \
	config-tool-section.h \
	config-tool-utils.c \
	config-tool-utils.h \
	config-tool-validate.c \
	config-tool-validate.h
