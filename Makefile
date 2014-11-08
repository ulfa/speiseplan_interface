PROJECT = erlything
DIALYZER = dialyzer
REBAR = rebar
RELX = ./relx
REPO = ../../../../repository
REPOSRC = ../../repository
TARGET = ~/projects/erlang
RELEASE = _rel

PROJECT = vodafone

all: app

tar: app 
	cd rel; tar cvf $(REPO)/$(PROJECT).$(VERSION).tar $(PROJECT)

tarall: app 
	cd ..; tar cf $(REPOSRC)/$(PROJECT).src.$(VERSION).tar $(PROJECT) --exclude log/* --exclude apps/horst/priv/config --exclude deps/gpio/priv/gpio_drv.so --exclude deps/syslog/priv/syslog_drv.so --exclude apps/horst/priv/config/accounts.conf --exclude Mnesia.erlything@ua-TA880GB

cpall: tarall
	cd ..;scp $(REPOSRC)/$(PROJECT).src.$(VERSION).tar $(USR)@$(HOST):$(TARGET)
	ssh $(USR)@$(HOST) 'cd $(TARGET); tar xf $(TARGET)/$(PROJECT).src.$(VERSION).tar'

cp: tar
	 cd ..;scp $(REPOSRC)/$(PROJECT).$(VERSION).tar $(USR)@$(HOST):$(TARGET)

app: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean
	rm -f test/*.beam
	rm -f erl_crash.dump
	rm -f log/*
	rm -rf $(RELEASE)

tests: clean app eunit ct

eunit:
	@$(REBAR) eunit skip_deps=true


docs:
	@$(REBAR) doc skip_deps=true

release: clean-release all
	@$(RELX) -o $(RELEASE)/$(PROJECT)

clean-release: 
	rm -rf $(RELEASE)/$(PROJECT)
