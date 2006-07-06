SUBDIRS = src

MANS_CPP=bmc-config.8

$(MANS_CPP): $(MANS_CPP:%=%.pre)
	cpp -P -I. -x c $@.pre -o $@

dist_man_MANS = $(MANS_CPP) bmc-config.conf.5

CLEANFILES = $(MANS_CPP) *~
DISTCLEANFILES = $(MANS_CPP) $(MANS_CPP:%=%.pre) .deps/*.P
