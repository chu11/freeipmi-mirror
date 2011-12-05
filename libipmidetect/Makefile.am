SUBDIRS = src

MANS_CPP = \
	libipmidetect.3

$(MANS_CPP): $(MANS_CPP:%=%.pre)
	$(CPP) -w -C -P -I. $@.pre  $@

dist_man_MANS = $(MANS_CPP)

if WITH_PKG_CONFIG
pkgconfig_DATA = libipmidetect.pc
endif

CLEANFILES = $(MANS_CPP) *~

DISTCLEANFILES = $(MANS_CPP) $(MANS_CPP:%=%.pre) .deps/*.P
