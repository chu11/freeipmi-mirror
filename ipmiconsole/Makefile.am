SUBDIRS = src

MANS_CPP = \
	ipmiconsole.8 \
	ipmiconsole.conf.5 \
	libipmiconsole.3

$(MANS_CPP): $(MANS_CPP:%=%.pre)
	$(CPP) -C -P -I. $@.pre  $@

dist_man_MANS = $(MANS_CPP)

CLEANFILES = $(MANS_CPP) *~

DISTCLEANFILES = $(MANS_CPP) $(MANS_CPP:%=%.pre) .deps/*.P
