SUBDIRS = include src 

MANS_CPP = libfreeipmi.3 freeipmi_interpret_sensors.conf.5

$(MANS_CPP): $(MANS_CPP:%=%.pre)
	$(CPP) -C -P -I. $@.pre  $@

dist_man_MANS = $(MANS_CPP)

CLEANFILES = $(MANS_CPP) *~

DISTCLEANFILES = $(MANS_CPP) $(MANS_CPP:%=%.pre) .deps/*.P

# Semaphore IPC Key file for ftok:
Ipckeydir     = $(localstatedir)/lib/$(PACKAGE_NAME)
Ipckey_DATA   = ipckey

EXTRA_DIST = ipckey

