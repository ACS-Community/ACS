# where the w3 lisp files should go
prefix  = gnu_root
datadir = $(prefix):[lib]
lispdir = $(prefix):[lib.emacs.site-lisp]
confdir = $(prefix):[lib.emacs.w3]

EMACS = emacs
ECHO  = write sys$output
MKDIR = create/dir

############## no user servicable parts beyond this point ###################
# Have to preload a few things to get a nice clean compile

EMACS     = emacs
WIDGETDIR = 

DEPS = -l sys$disk:[]vmsloadup.el

# compile with noninteractive and relatively clean environment
BATCHFLAGS = -batch -q -no-site-file

URLSOURCES = \
	url-nfs.el url-file.el url-cookie.el url-parse.el url-irc.el	\
	url-gopher.el url-http.el url-mail.el url-misc.el url-news.el	\
	url-vars.el url-auth.el mm.el md5.el url-gw.el ssl.el base64.el	\
	url.el socks.el url-cache.el url-ns.el

URLOBJECTS    = $(URLSOURCES:.el=.elc)

SOURCES = \
	mule-sysdp.el w3-widget.el devices.el w3-imap.el		\
	css.el dsssl.el dsssl-flow.el font.el images.el w3-vars.el	\
	w3-cus.el w3-style.el w3-keyword.el w3-forms.el w3-emulate.el	\
	w3-props.el w3-auto.el w3-menu.el w3-mouse.el w3-toolbar.el	\
	w3-speak.el w3-latex.el w3-parse.el w3-display.el		\
	w3-print.el w3-about.el w3-hot.el w3-e19.el w3-xemac.el w3.el	\
	w3-script.el w3-jscript.el w3-elisp.el w3-e20.el		\
	auto-autoloads.el custom-load.el w3-speak-table.el

OBJECTS = $(SOURCES:.el=.elc)

AUTOSOURCES = auto-autoloads.el custom-load.el w3-auto.el
AUTOOBJECTS = $(AUTOSOURCES:.el=.elc)

ALLSOURCES = $(SOURCES) $(URLSOURCES) $(AUTOSOURCES)
ALLOBJECTS = $(OBJECTS) $(URLOBJECTS) $(AUTOOBJECTS)

# Warning!  Currently, the following file can _NOT_ be bytecompiled.
EXTRAS = w3-sysdp.el docomp.el

.SUFFIXES: .elc .el

.el.elc:
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile $(MMS$SOURCE)

all:	w3

w3:	$(SOURCES) $(EXTRAS) $(OBJECTS)
	@echo Build of w3 complete...

fast:	$(SOURCES) $(EXTRAS)
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile $(SOURCES)

install: all
	@$(ECHO) Installing in $(lispdir)
	if f$parse("$(lispdir)") .eqs. "" then $(MKDIR) $(lispdir)
	copy/log $(SOURCES),$(OBJECTS),$(EXTRAS) $(lispdir)
	- purge/log $(lispdir)

distclean: clean
	$(RM) config.* Makefile

clean:
	$(RM) $(OBJECTS)

w3-vars.elc: w3-cus.elc w3-vars.el
w3-display.elc: w3-display.el css.elc font.elc w3-imap.elc
css.elc: css.el font.elc
w3.elc: css.elc w3-vars.elc w3.el
dsssl.elc: dsssl.el dsssl-flow.elc
