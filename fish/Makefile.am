SUBDIRS = src extensions scripts

MANS_CPP = fish.8 ipmi-sensors.8

$(MANS_CPP): $(MANS_CPP:%=%.pre)
	cpp -P -I. -x c $@.pre -o $@

dist_man_MANS = $(MANS_CPP)

CLEANFILES = $(MANS_CPP)
DISTCLEANFILES = $(MANS_CPP) $(MANS_CPP:%=%.pre) .deps/*.P

#  if you write a self-test script named `chk', uncomment the
#  following and add `chk' to the EXTRA_DIST list
#TESTS=chk

# fish_SOURCES=aclocal.m4
# EXTRA_DIST=fish.lsm.in fish.spec.in

DEFCONF = fish.scm
EXTRA_DIST = $(DEFCONF) 
Defconfdir = $(sysconfdir)/fish
Defconf_DATA = $(DEFCONF)
