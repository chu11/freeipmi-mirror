SUBDIRS = src

MANS_CPP = rmcpping.8

$(MANS_CPP): $(MANS_CPP:%=%.pre)
	$(CPP) -w -C -P -I. $@.pre  $@

dist_man_MANS = $(MANS_CPP) rmcp-ping.8

CLEANFILES = $(MANS_CPP) *~

DISTCLEANFILES = $(MANS_CPP) $(MANS_CPP:%=%.pre) .deps/*.P

EXTRA_DIST = rmcp-ping.8
