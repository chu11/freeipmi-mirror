noinst_HEADERS = \
	freeipmi-portability.h \
	freeipmi-argp-fmtstream.h \
	freeipmi-argp-namefrob.h \
	freeipmi-argp.h

if WITH_FREEIPMI_ARGP
FREEIPMI_ARG_SOURCES = \
	freeipmi-argp-ba.c \
	freeipmi-argp-eexst.c \
	freeipmi-argp-fmtstream.c \
	freeipmi-argp-help.c \
	freeipmi-argp-parse.c \
	freeipmi-argp-pv.c \
	freeipmi-argp-pvh.c
endif

noinst_LTLIBRARIES = \
	libfreeipmiportability.la

libfreeipmiportability_la_SOURCES = \
	freeipmi-portability.c \
	$(FREEIPMI_ARG_SOURCES)
