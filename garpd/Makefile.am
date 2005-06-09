SUBDIRS = src

dist_man_MANS = garpd.8

CLEANFILES = *~
DISTCLEANFILES = .deps/*.P

#  if you write a self-test script named `chk', uncomment the
#  following and add `chk' to the EXTRA_DIST list
#TESTS=chk

# fish_SOURCES=aclocal.m4
# EXTRA_DIST=fish.lsm.in fish.spec.in

DEFCONF = garpd.conf
EXTRA_DIST = $(DEFCONF) 
Defconfdir = $(sysconfdir)/garpd
Defconf_DATA = $(DEFCONF)
