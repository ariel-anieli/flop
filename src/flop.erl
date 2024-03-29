-module(flop).
-define(SRV, ?MODULE).
-define(CALLBACKS, flop_cb).
-define(DEF_OPTS, #{length=>5, log=>disable}).
-define(LOAD_OPTS(Options), maps:merge(?DEF_OPTS, Options)).

% Database operations
-export([
	 start_link/0,
	 load/1,
	 stop/0,
	 save/0,
	 template_link/0,
	 template_db/0,
	 create/1,
	 read/0,
	 read/1,
	 read/2,
	 update/3,
	 delete/1
]).

% CLI translators
-export([
	 description/1,
	 intf_eth/1,
	 intf_prt_chnl/1,
	 intf_vlan/1,
	 print/1,
	 route_map/1,
	 split_by_tag/1,
	 vlan/1
]).

% Database operations
start_link()         -> gen_server:start_link({local, ?SRV}, ?CALLBACKS, [], []).
load(DB)             -> gen_server:call(?SRV, #{request=>load, db=>DB}).
stop()               -> gen_server:call(?SRV, #{request=>stop}).
save()               -> gen_server:call(?SRV, #{request=>save}).
template_link()      -> gen_server:call(?SRV, #{request=>template_link}).
template_db()        -> gen_server:call(?SRV, #{request=>template_db}).
create(Link)         -> gen_server:call(?SRV, #{request=>create, link=>Link}).

read() -> 
    gen_server:call(?SRV, #{request=>read,page=>1,options=>?DEF_OPTS}).

read(Page) when is_integer(Page) -> 
    gen_server:call(?SRV, #{request=>read,page=>Page,options=>?DEF_OPTS});

read(Options) when is_map(Options) ->
    gen_server:call(?SRV, #{request=>read,page=>1,options=>?LOAD_OPTS(Options)}).

read(Page, Options) when is_integer(Page), is_map(Options) ->
    gen_server:call(?SRV, #{request=>read,page=>Page,options=>?LOAD_OPTS(Options)}).

update(ID, Key, Val) -> gen_server:call(?SRV, #{request=>update, 
						id=>ID,
						key=>Key,
						val=>Val}).
delete(ID)         -> gen_server:call(?SRV, #{request=>delete, id=>ID}).

% CLI translators
description(Type)  -> gen_server:call(?SRV, #{request=>description,
					      type=>Type}).
intf_prt_chnl(Type) -> gen_server:call(?SRV, #{request=>'interface port-channel',
					      type=>Type}).
intf_eth(Type)     -> gen_server:call(?SRV, #{request=>'interface ethernet',
					      type=>Type}).
intf_vlan(Type)    -> gen_server:call(?SRV, #{request=>'interface vlan',
					      type=>Type}).
print(Cmds)        -> gen_server:cast(?SRV, #{request=>print, cmds=>Cmds}).
route_map(Type)    -> gen_server:call(?SRV, #{request=>'route map', type=>Type}).
split_by_tag(Type) -> gen_server:call(?SRV, #{request=>'split by tag', 
					      type=>Type}).
vlan(Type)         -> gen_server:call(?SRV, #{request=>vlan, type=>Type}).
