-module(contract_checker).
-behaviour(gen_server).

-define(SRV, ?MODULE).
-define(HEAD_IS_INT(Net), hd(Net)>=48,hd(Net)=<57).
-define(HEAD_IS_UPPC(Str), hd(Str)>=65,hd(Str)=<90).
-define(HEAD_IS_LOWC(Str), hd(Str)>=97,hd(Str)=<122).

-export([
	 init/1, 
	 handle_call/3,
	 handle_info/2,
	 terminate/2,
	 code_change/3
]).

-export([
	 start_link/0,
	 stop/0,
	 is_type/2,
	 get_fault/1,
	 get_defval/1
]).

start_link()       -> gen_server:start_link({local, ?SRV}, ?MODULE, [], []).
stop()             -> gen_server:call(?SRV, #{request=>stop}).
is_type(Item,Type) -> gen_server:call(?SRV, #{request=>'is?', item=>Item, is=>Type}).
get_fault(Type)    -> gen_server:call(?SRV, #{request=>fault, type=>Type}).
get_defval(Key)    -> gen_server:call(?SRV, #{request=>defval, key=>Key}).
     

init([]) ->
    {ok, []}.

handle_call(#{request:='is?', item:=Item, is:=Type}, From, State) ->
    Default  = fun(Item) -> false end,
    Contract = maps:get(Type, get_contracts(), Default),
    {reply, Contract(Item), State};
handle_call(#{request:=defval, key:=Key}, From, State) ->
    {reply, maps:get(Key, get_defaults()), State};
handle_call(#{request:=fault, type:=Type}, From, State) ->
    Default = nofault,
    Fault   = maps:get(Type, get_faults(), Default),
    {reply, Fault, State};
handle_call(#{request:=stop}, _From, DB) ->
    {stop, normal, stopped, DB}.

terminate(Reason, DB)          -> ok.
handle_info(Info, DB)	       -> {noreply, DB}.
code_change(OldVsn, DB, Extra) -> {ok, DB}.

is_valid(Term) when is_boolean(Term) -> 
    Term==true;
	     
is_valid(Terms) when is_list(Terms) -> 
    lists:all(fun(Term) -> Term==true end, Terms).

is_string(String) when is_list(String) ->
    re:run(String, "^[a-z0-9A-Z\-]+$", [{capture, none}])==match;

is_string(_) ->
    false.

is_like_addr_with_mask([AddrAsString|[MaskAsString]], true) ->
    {ok, Addr} = inet:parse_address(AddrAsString),
    Mask = erlang:list_to_integer(MaskAsString),

    inet:is_ipv4_address(Addr) andalso Mask<33 andalso Mask>=0;

is_like_addr_with_mask(_, false) ->
    false.

is_net(String) ->
    HasGoodFmt  = re:run(String, "^[0-9\.]+/[0-9]+$", [{capture, none}])==match,
    AddrAndMask = re:split(String, "/", [{return, list}]),

    is_like_addr_with_mask(AddrAndMask, HasGoodFmt).

is_mac(String) ->
    re:run(String, "^([a-f0-9]{2}:){5}[a-f0-9]{2}$", [{capture, none}])==match.

get_tag_contract(Tags) when is_list(hd(Tags)) ->
    is_valid([is_valid(is_list(Tag) andalso is_string(Tag))
	      || Tag<-Tags]);
get_tag_contract(Tag) 
  when ?HEAD_IS_INT(Tag); ?HEAD_IS_LOWC(Tag); ?HEAD_IS_UPPC(Tag)  ->
    is_valid(is_list(Tag) andalso is_string(Tag));
get_tag_contract(Tag) ->
    false.

get_net_contract(Nets) when is_list(hd(Nets)) ->
    is_valid([is_valid(is_list(Net) andalso is_net(Net))
		       || Net<-Nets]);
get_net_contract(Net) when ?HEAD_IS_INT(Net) ->
    is_valid(is_list(Net) andalso is_net(Net));
get_net_contract(Net) ->
    false.

get_vlan_contract(VLAN) when is_integer(VLAN) ->
    is_valid([is_integer(VLAN), VLAN<4095, VLAN>=0]);
get_vlan_contract(VLANs) when is_list(VLANs) ->
    is_valid([is_valid([is_integer(VLAN), VLAN<4095, VLAN>=0])
	      || VLAN <- VLANs]).

get_port_contract(Port) ->
    is_valid([is_integer(Port), Port>=0]).

get_dev_contract(Dev) ->
    is_string(Dev).

get_addr_contract(Mac) ->
    is_mac(Mac).

get_endpoint_contract(#{addr:=Addr, dev:=Dev, port:=Port} = End) 
  when map_size(End)==3 ->
    Contracts = get_contracts(),
    IsConform = fun(Key) ->
			Check     = maps:get(Key, Contracts),
			Value     = maps:get(Key, End),
			Check(Value)
		end,

    is_valid([IsConform(Key) || Key <- maps:keys(End)]);

get_endpoint_contract(_) ->
    false.

get_link_contract(#{from:=From, to:=To, net:=Net, tag:=Tag, vlan:=Vlan}=Link)
  when map_size(Link)==5  ->
    Contracts = get_contracts(),
    IsConform = fun(Key) ->
			Check     = maps:get(Key, Contracts),
			Value     = maps:get(Key, Link),
			Check(Value)
		end,

    is_valid([IsConform(Key) || Key <- maps:keys(Link)]);

get_link_contract(#{from:=From, to:=To, net:=Net, 
		    tag:=Tag, aggr:=Aggr, vlan:=Vlan}=Link)
  when map_size(Link)==6  ->
    Contracts = get_contracts(),
    IsConform = fun(Key) ->
			Check     = maps:get(Key, Contracts),
			Value     = maps:get(Key, Link),
			Check(Value)
		end,

    is_valid([IsConform(Key) || Key <- maps:keys(Link)]);

get_link_contract(_) ->
    false.

get_contracts() -> 
    #{
      link => fun get_link_contract/1,
      to   => fun get_endpoint_contract/1,
      from => fun get_endpoint_contract/1,
      net  => fun get_net_contract/1,
      tag  => fun get_tag_contract/1,
      vlan => fun get_vlan_contract/1,
      port => fun get_port_contract/1,
      dev  => fun get_dev_contract/1, 
      addr => fun get_addr_contract/1,
      aggr => fun get_port_contract/1
     }.

get_defaults() ->
    #{
      net   => "0.0.0.0/0.0.0.0",
      tag   => "",
      vlan  => 0,
      port  => 0,
      dev   => "",
      addr  => "ff:ff:ff:ff:ff:ff",
      aggr  => 0
     }.

get_faults() -> 
    #{
      link  => 'Wrong value; for an example, run flop:template_link()',
      to    => 'Wrong value; for an example, run flop:template_link()',
      from  => 'Wrong value; for an example, run flop:template_link()',
      net   => 'IPv4/Mask, or a list of IPv4/Mask: see flop:template_link()',
      tag   => 'Must be a string; or a list of strings',
      vlan  => 'Must be an integer: <4095 and >=0; or a list of integers',
      port  => 'Must be an integer: and >=0',
      dev   => 'Must be a string',
      addr  => 'Must be a MAC address',
      aggr  => 'Must be an integer: and >=0'
     }.
