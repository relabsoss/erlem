APPNAME=erlem

REBAR=./rebar3
ERL=erl
ERLC=erlc

all: compile

$(REBAR):
	wget https://s3.amazonaws.com/rebar3/rebar3 -O $(REBAR)
	chmod +x $(REBAR)

compile: $(REBAR)
	@$(REBAR) compile

clean: $(REBAR)
	@$(REBAR) clean

run: $(REBAR)
	$(REBAR) shell +pc unicode --config config/config.sys --sname $(APPNAME)@localhost

