SUBDIRS = src 

CLEANFILES = 
DISTCLEANFILES = .deps/*.P

# Semaphore IPC Key file for ftok:
IPCKEY = ipckey

if RPM_INSTALL
IPCDIR = $(RPM_BUILD_ROOT)/var/lib/$(PACKAGE_NAME)
else
IPCDIR = /var/lib/$(PACKAGE_NAME)
endif

Ipckeydir     = $(IPCDIR)
Ipckey_DATA   = $(IPCKEY)

EXTRA_DIST = $(IPCKEY)

