-module(contracts).

-export([
	 get_contracts/0,
	 get_defaults/0,
	 get_faults/0
	]).

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

get_tag_contract(Tag) ->
    is_valid(is_list(Tag) andalso is_string(Tag)).

get_net_contract(Net) ->
    is_valid(is_list(Net) andalso is_net(Net)).

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
    %get_port_contract(Port) andalso is_string(Dev) andalso is_mac(Addr);
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
      link => fun(Link) -> get_link_contract(Link) end,
      to   => fun(End)  -> get_endpoint_contract(End) end,
      from => fun(End)  -> get_endpoint_contract(End) end,
      net  => fun(Net)  -> get_net_contract(Net) end,
      tag  => fun(Tag)  -> get_tag_contract(Tag) end,
      vlan => fun(Vlan) -> get_vlan_contract(Vlan) end,
      port => fun(Port) -> get_port_contract(Port) end,
      dev  => fun(Dev)  -> get_dev_contract(Dev) end,
      addr => fun(Addr) -> get_addr_contract(Addr) end,
      aggr => fun(Aggr) -> get_port_contract(Aggr) end
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
      net   => 'Must be IPv4/Mask; for an example, run flop:template_link()',
      tag   => 'Must be a string',
      vlan  => 'Must be an integer: <4095 and >=0; or a list of integers',
      port  => 'Must be an integer: and >=0',
      dev   => 'Must be a string',
      addr  => 'Must be a MAC address',
      aggr  => 'Must be an integer: and >=0'
     }.
