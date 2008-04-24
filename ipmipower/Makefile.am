SUBDIRS = src

MANS_CPP = ipmipower.8 ipmipower.conf.5

$(MANS_CPP): $(MANS_CPP:%=%.pre)
	cpp -C -P -I. -x c $@.pre -o $@

dist_man_MANS = $(MANS_CPP)

CLEANFILES = $(MANS_CPP) *~
DISTCLEANFILES = $(MANS_CPP) $(MANS_CPP:%=%.pre) .deps/*.P

#  if you write a self-test script named `chk', uncomment the
#  following and add `chk' to the EXTRA_DIST list
#TESTS=chk
