SUBDIRS = src

MANS_CPP = bmc-device.8

$(MANS_CPP): $(MANS_CPP:%=%.pre)
	cpp -C -P -I. -x c $@.pre -o $@

dist_man_MANS = $(MANS_CPP)

CLEANFILES = $(MANS_CPP) *~
DISTCLEANFILES = $(MANS_CPP) $(MANS_CPP:%=%.pre) .deps/*.P
