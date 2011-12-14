if WITH_FREEIPMI_ARGP
FREEIPMI_ARG_SOURCES = \
	freeipmi-argp.h \
	freeipmi-argp-ba.c \
	freeipmi-argp-eexst.c \
	freeipmi-argp-fmtstream.c \
	freeipmi-argp-fmtstream.h \
	freeipmi-argp-help.c \
	freeipmi-argp-namefrob.h \
	freeipmi-argp-parse.c \
	freeipmi-argp-pv.c \
	freeipmi-argp-pvh.c
endif

noinst_LTLIBRARIES = \
	libportability.la

libportability_la_CPPFLAGS = \
	-D_GNU_SOURCE \
	-D_REENTRANT

libportability_la_SOURCES = \
	freeipmi-portability.c \
	freeipmi-portability.h \
	$(FREEIPMI_ARG_SOURCES)
