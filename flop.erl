-module(flop).
-define(SRV, callbacks).

-import(
   callbacks, 
   [
    init/1, 
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
   ]
).

% Database operations
-export([
	 start/1,
	 stop/0,
	 save/0,
	 template_link/0,
	 template_db/0,
	 create/1,
	 read/0,
	 update/3,
	 delete/1
]).

% CLI translators
-export([
	 description/1,
	 intf_eth/1,
	 intf_vlan/1,
	 print/1,
	 route_map/1,
	 split_by_tag/1,
	 vlan/1
]).

% Database operations
start(DB)            -> gen_server:start_link({local, ?SRV}, ?SRV, DB, []).
stop()               -> gen_server:call(?SRV, #{request=>stop}).
save()               -> gen_server:call(?SRV, #{request=>save}).
template_link()      -> gen_server:call(?SRV, #{request=>template_link}).
template_db()        -> gen_server:call(?SRV, #{request=>template_db}).
create(Link)         -> gen_server:call(?SRV, #{request=>create, link=>Link}).
read()               -> gen_server:call(?SRV, #{request=>read}).
update(ID, Key, Val) -> gen_server:call(?SRV, #{request=>update, 
						id=>ID,
						key=>Key,
						val=>Val}).
delete(ID)           -> gen_server:call(?SRV, #{request=>delete, id=>ID}).

% CLI translators
description(Type)  -> gen_server:call(?SRV, #{request=>description,
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
