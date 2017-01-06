
CGI_DIR=/cgi-bin/trajectory_test

EXES=find_file nc2nasa nc2www plot plot_traj traj_form

ALLTARGETS=$(EXES) \
           Traj/Job.pm find_file.pl \
           plot.pl plot.idl plot_traj.pl

INSTALL_DIR=/var/www/badc_site/$(CGI_DIR)

all: $(ALLTARGETS)

install: $(ALLTARGETS)
        # First create install directory.
        # It is good that this aborts the install if the dir already exists.
	mkdir -m 755 $(INSTALL_DIR)
	cp -r *.pl idl Traj .flexlmrc plot.idl $(INSTALL_DIR)
	chown -R badc.badcint $(INSTALL_DIR)
	chmod -R go+rX-w $(INSTALL_DIR)
	install -o root -g badcint -m 6755 $(EXES) $(INSTALL_DIR)

uninstall:
	rm -fr $(INSTALL_DIR)

%: %.in
	sed 's,@@INSTALL_DIR@@,$(INSTALL_DIR),g; s,@@CGI_DIR@@,$(CGI_DIR),g' $< > $@
	if [ -x $< ] ; then chmod +x $@ ; fi

config.h:
	echo "#define INSTALL_DIR \"$(INSTALL_DIR)\"" > $@

%: %.c config.h
	gcc -Wall -o $@ $<

clean:
	rm -f $(EXES) $(ALLTARGETS) config.h
