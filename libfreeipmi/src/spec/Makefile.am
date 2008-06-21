noinst_LTLIBRARIES = libspec.la

libspec_la_LIBADD = \
	$(top_builddir)/common/src/portability/libfreeipmiportability.la

libspec_la_CPPFLAGS = \
	-I$(top_builddir)/libfreeipmi/src \
	-I$(top_builddir)/libfreeipmi/include \
	-I$(top_builddir)/common/src/portability \
	-D_GNU_SOURCE \
	-D_REENTRANT

libspec_la_SOURCES = \
	ipmi-fru-chassis-types-spec.c \
	ipmi-fru-language-codes-spec.c \
	ipmi-sensor-types-spec.c \
	ipmi-sensor-units-spec.c

$(top_builddir)/common/src/portability/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
