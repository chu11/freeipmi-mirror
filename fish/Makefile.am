SUBDIRS = src extensions scripts

dist_man_MANS = bmc-config.1 bmc-info.1 fish.1 sel.1 sensors.1

CLEANFILES = *~
DISTCLEANFILES = .deps/*.P

#  if you write a self-test script named `chk', uncomment the
#  following and add `chk' to the EXTRA_DIST list
#TESTS=chk

# fish_SOURCES=aclocal.m4
# EXTRA_DIST=fish.lsm.in fish.spec.in

DEFCONF = fish.scm
EXTRA_DIST = $(DEFCONF) 
Defconfdir = $(sysconfdir)/fish
Defconf_DATA = $(DEFCONF)
