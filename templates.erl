-module(templates).
-define(HASH_METHOD, ripemd160).
-export([
	 build_snippet_using_keys/1,
	 get_db_template/1,
	 get_link_template/0,
	 hash/1,
	 tag_link_with_hash_of_addrs/1
]).

pipe(Arg, Funcs) -> 
    lists:foldl(fun(Func, Arg) -> Func(Arg) end, Arg, Funcs).

get_template(#{type:=nxos, request:=description}) ->
    "cx=!name!;to=!to-dev!_!to-port!";

get_template(#{type:=nxos, request:='desc aggr'}) ->
    "cx=!name!;to=!to-dev!_!to-aggr-port!";

get_template(#{type:=nxos, request:='interface port-channel'}) ->
    "interface port-channel !aggr!
     description !desc-aggr!
     switchport mode trunk
     switchport trunk allowed vlan !vlans-from-aggr!";

get_template(#{type:=nxos, request:='interface ethernet', link:=Link}) 
  when not is_map_key(aggr, Link) ->
    "interface ethernet 1/!from-port!
     description !desc!
     switchport mode trunk
     switchport trunk allowed vlan !vlans-from-dev!";

get_template(#{type:=nxos, request:='interface ethernet', link:=Link}) 
  when is_map_key(aggr, Link) ->
    "interface ethernet 1/!from-port!
     description !desc!
     switchport mode trunk
     switchport trunk allowed vlan !vlans-from-dev!
     channel-group !aggr! mode active";

get_template(#{type:=nxos, request:='interface vlan'}) ->
    "interface vlan !vlan!
     no shutdown
     vrf member !vrf!
     ip address !ip!";

get_template(#{type:=nxos, request:='route map'}) ->
    "route-map !vrf! permit 10
     match interface !vlans-from-vrf!";

get_template(#{type:=nxos, request:='split by tag'}) ->
    "!name!_!tag!";

get_template(#{type:=nxos, request:=vlan}) ->
    "vlan !vlan!
    name !name!-!net!".

get_key(#{key:=aggr, link:=Link}) -> 
    #{
      key => "!aggr!",
      val => integer_to_list(maps:get(aggr, Link, 0))
     };

get_key(#{key:=desc, link:=Link} = Args) -> 
    Keys = ['to dev', 'to port', name],
    Req  = 'description',

    #{
      key => "!desc!",
      val => build_from_link(Args#{keys=>Keys, request=>Req, link=>Link})
     };

get_key(#{key:='desc aggr', link:=Link} = Args) -> 
    Keys = ['to dev', 'to aggr port', name],
    Req  = 'desc aggr',

    #{
      key => "!desc-aggr!",
      val => build_from_link(Args#{keys=>Keys, request=>Req, link=>Link})
     };

get_key(#{key:='from port', link:=Link}) -> 
    From = maps:get(from, Link),

    #{
      key => "!from-port!",
      val => integer_to_list(maps:get(port, From))
     };

get_key(#{key:=name, db:=DB}) -> 
    #{
      key => "!name!",
      val => maps:get(name, DB)
     };

get_key(#{key:=ip, link:=Link}) -> 
    #{
      key => "!ip!",
      val => maps:get(net, Link)
     };

get_key(#{key:=net, link:=Link}) -> 
    #{
      key => "!net!",
      val => maps:get(net, Link)
     };

get_key(#{key:='to port', link:=Link}) -> 
    To = maps:get(to, Link),

    #{
      key => "!to-port!",
      val => integer_to_list(maps:get(port, To))
     };

get_key(#{key:='to aggr port', link:=Link, db:=DB}) -> 
    GetPort = fun(Link) -> To = maps:get(to,Link),
			   maps:get(port, To) end,

    Aggr  = maps:get(aggr, Link, 0),
    Links = maps:get(links, DB),
    Ports = [integer_to_list(GetPort(Link))
	     || Link <- Links, Aggr=:=maps:get(aggr,Link,0)],

    #{
      key => "!to-aggr-port!",
      val => lists:join("-", lists:sort(Ports))
     };

get_key(#{key:=tag, link:=Link}) -> 
    #{
      key => "!tag!",
      val => maps:get(tag, Link)
     };

get_key(#{key:='to dev', link:=Link}) -> 
    To = maps:get(to, Link),

    #{
      key => "!to-dev!",
      val => maps:get(dev, To)
     };

get_key(#{key:=vlan, link:=Link}) -> 
    #{
      key => "!vlan!",
      val => list_vlans(maps:get(vlan,Link))
     };

get_key(#{key:='vlans from aggr', db:=DB, link:=Link}) ->
    Aggr  = maps:get(aggr, Link, 0),
    Links = maps:get(links, DB),
    VLANs = pipe(
	      [list_vlans(maps:get(vlan, Link)) 
	       || Link <- Links, Aggr=:=maps:get(aggr,Link,0)],
	      [
	       fun lists:merge/1,
	       fun lists:sort/1,
	       fun lists:uniq/1
	      ]
	     ),

    #{
      key => "!vlans-from-aggr!",
      val => lists:join(",", VLANs)
     };

get_key(#{key:='vlans from dev', link:=Link}) ->
    #{
      key => "!vlans-from-dev!",
      val => flatten_vlans(maps:get(vlan, Link))
     };

get_key(#{key:='vlans from vrf', db:=DB, link:=Link}) ->
    Tag   = maps:get(tag, Link),
    Links = maps:get(links, DB),
    VLANs = [prefix_vlan(maps:get(vlan, Link))
	     || Link <- Links, Tag=:=maps:get(tag,Link)],

    #{
      key => "!vlans-from-vrf!",
      val => lists:join(" ", VLANs)
     };

get_key(#{key:=vrf, link:=Link} = Args) -> 
    Keys = [name, tag],
    Req  = 'split by tag',

    #{
      key => "!vrf!",
      val => build_from_link(Args#{keys=>Keys, request=>Req, link=>Link})
     }.

build_from_link(#{keys:=Keys} = Args) ->
    map_keyset_into_template(
      Args#{
	template => get_template(Args),
	keyset   => [get_key(Args#{key=>Key}) || Key <- Keys]
       }
     ).

prefix_vlan(VLAN) when is_integer(VLAN) ->
    lists:concat(["vlan", VLAN]);
prefix_vlan(VLANs) when is_list(VLANs) ->
    lists:join(" ", [lists:concat(["vlan", VLAN]) || VLAN <- VLANs]).

list_vlans(VLAN) when is_integer(VLAN) ->
    [integer_to_list(VLAN)];
list_vlans(VLANs) when is_list(VLANs) ->
    [integer_to_list(VLAN) || VLAN <- VLANs].

flatten_vlans(VLAN) when is_integer(VLAN) ->
    integer_to_list(VLAN);
flatten_vlans(VLANs) when is_list(VLANs) ->
    lists:join(",", [integer_to_list(VLAN) || VLAN <- VLANs]).

build_snippet_using_keys(#{db:=DB, request:=Request} = Args)
  when Request=/=vlan,Request=/='interface vlan'  ->
    BuildFromLink    = fun(Link)  -> build_from_link(Args#{link=>Link}) end,
    BuildAllLinks    = fun(Links) -> lists:map(BuildFromLink, Links) end,

    pipe(maps:get(links, DB),
	 [
	  BuildAllLinks,
	  % fun lists:merge/1,
	  fun lists:uniq/1
	 ]
	);

build_snippet_using_keys(#{db:=DB, request:=Request} = Args)
  when Request=:=vlan;Request=:='interface vlan' ->
    BuildFromLink    = fun(Link)  -> build_from_link(Args#{link=>Link}) end,
    BuildAllLinks    = fun(Links) -> lists:map(BuildFromLink, Links) end,

    pipe(maps:get(links, DB),
	 [
	  BuildAllLinks,
	  fun lists:merge/1,
	  fun lists:uniq/1
	 ]
	).

map_keyset_into_template(#{keyset:=KS, template:=T,request:=Request})
  when Request=:=vlan;Request=:='interface vlan' ->
    VinK = fun(#{key:=K,val:=V},T) -> re:replace(T, K, V, [{return, list}]) end,
    [#{key:=K,val:=Vs}] = lists:filter(fun(#{key:=K}) -> K=:="!vlan!" end, KS),
    ButVs  = lists:filter(fun(#{key:=K}) -> K=/="!vlan!" end, KS),
    Mapper = fun(V) -> lists:foldr(VinK, T, [#{key=>K, val=>V} | ButVs]) end,

    lists:map(Mapper, Vs);

map_keyset_into_template(#{keyset:=KS, template:=T, request:=Request}) 
  when Request=/=vlan,Request=/='interface vlan' ->
    VinK = fun(#{key:=K,val:=V},T) -> re:replace(T, K, V, [{return, list}]) end,

    lists:foldr(VinK, T, KS).

get_db_template(Name) ->
    #{
      name    => get_random_str(4),
      file  => Name,
      links => [tag_link_with_hash_of_addrs(get_link_template())]
     }.    

get_link_template() ->
    #{
      vlan => crypto:rand_uniform(0, 4095),
      net  => get_random_ip_addr(),
      tag  => get_random_str(4),
      from => get_random_endpoint_link(),
      to   => get_random_endpoint_link()
     }.

get_random_ip_addr() ->
    pipe(
      [crypto:rand_uniform(0,255) || Byte <- lists:seq(1,4)],
      [
       fun(Bytes) -> lists:join(".", Bytes) end,
       fun(Addr)  -> lists:concat(Addr) end,
       fun(Addr)  -> Addr ++ "/24" end
      ]
     ).

get_random_mac_addr() ->
    pipe(
      [get_random(2, "abcdef1234567890") || Byte <- lists:seq(1,6)],
      [
       fun(Bytes) -> lists:join(":", Bytes) end,
       fun(Addr)  -> lists:concat(Addr) end
      ]
     ).

get_random_str(Length) ->
    CharSet = "abcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ",

    get_random(Length, CharSet).

get_random(Length, CharSet) ->
    MaxLength = length(CharSet),

    lists:foldl(
      fun(_, Acc) ->
	      [lists:nth(crypto:rand_uniform(1, MaxLength), CharSet)] ++ Acc end,
      [], lists:seq(1, Length)
     ).

get_random_endpoint_link() ->
    #{
      dev  => get_random_str(8),
      port => crypto:rand_uniform(1, 255),
      addr => get_random_mac_addr()
     }.

tag_link_with_hash_of_addrs(#{from:=From, to:=To} = Link) ->
    FromAddr = maps:get(addr, From),
    ToAddr   = maps:get(addr, To),
    FromStr  = erlang:list_to_binary(
		 re:replace(FromAddr, "\:", "", [global, {return, list}])),
    ToStr    = erlang:list_to_binary(
		 re:replace(ToAddr, "\:", "", [global, {return, list}])),
    HashXOR  = hash(crypto:exor(FromStr, ToStr)),

    Link#{id => HashXOR}.

hash(Term) ->
    pipe(
     Term,
      [
       fun(Str)  -> crypto:hash(?HASH_METHOD, Str) end,
       fun(Hash) -> binary:encode_hex(Hash) end,
       fun(Hex)  -> binary:bin_to_list(Hex) end,
       fun(Str)  -> string:lowercase(Str) end
      ]).
