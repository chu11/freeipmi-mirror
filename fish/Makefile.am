SUBDIRS = src extensions scripts

dist_man_MANS = bmc-config.8 bmc-info.8 fish.8 ipmi-sel.8 ipmi-sensors.8 bmc-config.conf.5

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
