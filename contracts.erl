-module(contracts).

-export([
	 get_contracts/0,
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

get_vlan_contract(Vlan) ->
    is_valid([is_integer(Vlan), Vlan<4095, Vlan>=0]).

get_endpoint_contract(#{addr:=Addr, dev:=Dev, port:=Port}) 
  when map_size(Link)==5 ->
    is_integer(Port) andalso Port>=0 andalso is_string(Dev) andalso is_mac(Addr);

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

get_link_contract(_) ->
    false.

get_contracts() -> 
    #{
      link => fun(Link) -> get_link_contract(Link) end,
      to   => fun(End)  -> get_endpoint_contract(End) end,
      from => fun(End)  -> get_endpoint_contract(End) end,
      net  => fun(Net)  -> get_net_contract(Net) end,
      tag  => fun(Tag)  -> get_tag_contract(Tag) end,
      vlan => fun(Vlan) -> get_vlan_contract(Vlan) end
     }.

get_faults() -> 
    #{
      link => 'Wrong value; for an example, run flop:template_link()',
      to   => 'Wrong value; for an example, run flop:template_link()',
      from => 'Wrong value; for an example, run flop:template_link()',
      net  => 'Must be IPv4/Mask; for an example, run flop:template_link()',
      tag  => 'Must be a string',
      vlan => 'Must be an integer: <4095 and >=0'
     }.
