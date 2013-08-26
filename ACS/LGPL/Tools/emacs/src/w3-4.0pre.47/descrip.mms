# This file kindly written by Richard Levitte - GNU on VMS hacker
#                                               <levitte@vms.stacken.kth.se>
#
# Hacked upon by Bill Perry <wmperry@aventail.com>, whose VMS experiences
# are a thing of the past.  Let me know if this works or not.

# where the w3 lisp files should go
prefix  = gnu_root
lispdir = $(prefix):[emacs.site-lisp]
confdir = $(prefix):[emacs.w3]

DOTEMACS= $(prefix):[lib.emacs.site-lisp]default.el
SUBDIRS =lisp texi etc

RM        = delete
MKDIR	  = create/dir
ECHO      = write sys$output

.PHONY: $(SUBDIRS) dotemacs

all:	w3 info

w3 fast:
	set def [.lisp]
	mms $@
	set def [-]

html info dvi:
	set def [.texi]
	mms $@
	set def [-]

install: all dotemacs
	 set def [.lisp]
	 mms install
	 set def [-.texi]
	 mms install
	 set def [-.etc]
	 mms install
	 set def [-]

distclean: clean
	 set def [.lisp]
	 mms distclean
	 set def [-.texi]
	 mms distclean
	 set def [-.etc]
	 mms distclean
	 set def [-]
	 $(RM) config.* Makefile

clean:
	 set def [.lisp]
	 mms clean
	 set def [-.texi]
	 mms clean
	 set def [-.etc]
	 mms clean
	 set def [-]

dotemacs:
	@if (grep ";;; Emacs/W3 Configuration" $(DOTEMACS) 2>&1) >/dev/null; then \
		echo Emacs/W3 setup already exists in $(DOTEMACS);	\
	else								\
		test -d `dirname $(DOTEMACS)` || mkdir -p `dirname $(DOTEMACS)`; \
		(echo >> $(DOTEMACS)); \
		(echo ";;; Emacs/W3 Configuration" >> $(DOTEMACS)); \
		(echo "(setq load-path (cons \"$(lispdir)\" load-path))" >> $(DOTEMACS)); \
		(echo "(require 'w3-autoloads \"w3/auto-autoloads\")" >> $(DOTEMACS)); \
		echo "Added Emacs/W3 setup to $(DOTEMACS)"; \
	fi
