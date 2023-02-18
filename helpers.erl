-module(helpers).

-export([
	 build_snippet_using_keys/1,
	 create_link/3,
	 get_db_template/1,
	 get_link_template/0,
	 get_template/1,
	 get_key/1,
	 find_matching_link/2,
	 find_not_matching_links/2,
	 hash/1,
	 if_needed_update_and_log/3,
	 open_db_or_create_from_template/2,
	 save_db_if_ids_differ/3,
	 tag_link_with_hash_of_addrs/1
]).

pipe(Arg, Funcs) -> 
    lists:foldl(fun(Func, Arg) -> Func(Arg) end, Arg, Funcs).

build_from_link(#{keys:=Keys} = Args) ->
    map_keyset_into_template(
      #{
	template => get_template(Args),
	keyset   => [get_key(Args#{key=>Key}) || Key <- Keys]
       }
     ).

build_snippet_using_keys(#{db:=DB} = Args) -> 
    BuildFromLink    = fun(Link)  -> build_from_link(Args#{link=>Link}) end,
    BuildAllLinks    = fun(Links) -> lists:map(BuildFromLink, Links) end,
    KeepUniqSnippets = fun(List)  -> lists:uniq(List) end,

    pipe(maps:get(links, DB), [BuildAllLinks, KeepUniqSnippets]).

map_keyset_into_template(#{keyset:=KeySet, template:=Temp}) ->
    ReplaceKwithVal = fun(#{key:=Key, val:=Val}, Temp) -> 
			      re:replace(Temp, Key, Val, [{return, list}]) end,

    lists:foldr(ReplaceKwithVal, Temp, KeySet).

get_template(#{type:=nxos, request:=description}) ->
    "cx=!name!;to=!to-dev!_!to-port!";

get_template(#{type:=nxos, request:='interface ethernet'}) ->
    "interface ethernet 1/!from-port!
     description !desc!
     switchport mode trunk
     switchport trunk allowed vlan !vlans-from-dev!";

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

get_key(#{key:=desc, link:=Link} = Args) -> 
    Keys = ['to dev', 'to port', name],
    Req  = 'description',

    #{
      key => "!desc!",
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
    Network  = maps:get(net, Link),
    Pattern  = "[0-9]+/",
    Replace  = "1/",

    #{
      key => "!ip!",
      val => re:replace(Network, Pattern, Replace, [{return, list}])
     };

get_key(#{key:=net, link:=Link}) -> 
    Pattern = "\.[0-9]+/[0-9]+",
    String  = maps:get(net, Link),

    #{
      key => "!net!",
      val => re:replace(String, Pattern, "", [{return, list}])
     };

get_key(#{key:='to port', link:=Link}) -> 
    To = maps:get(to, Link),

    #{
      key => "!to-port!",
      val => integer_to_list(maps:get(port, To))
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
      val => integer_to_list(maps:get(vlan,Link))
    };

get_key(#{key:='vlans from dev', db:=DB, link:=Link}) ->
    From  = maps:get(from, Link),
    Links = maps:get(links, DB),
    VLANs = [integer_to_list(maps:get(vlan, Link))
	     || Link <- Links, From=:=maps:get(from,Link)],

    #{
      key => "!vlans-from-dev!",
      val => lists:join(",", VLANs)
    };

get_key(#{key:='vlans from vrf', db:=DB, link:=Link}) ->
    Tag   = maps:get(tag, Link),
    Links = maps:get(links, DB),
    VLANs = [lists:concat(["vlan", maps:get(vlan, Link)])
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

hash(Term) ->
    pipe(
     Term,
      [
       fun(Str)  -> crypto:hash(ripemd160, Str) end,
       fun(Hash) -> binary:encode_hex(Hash) end,
       fun(Hex)  -> binary:bin_to_list(Hex) end,
       fun(Str)  -> string:lowercase(Str) end
      ]).

open_db_or_create_from_template({ok, [DB]}, _) ->
    Links       = maps:get(links, DB),
    TaggedLinks = [tag_link_with_hash_of_addrs(Link) || Link <- Links], 

    DB#{links := TaggedLinks};

open_db_or_create_from_template({error, _}, Name) -> 
    get_db_template(Name).

tag_link_with_hash_of_addrs(#{from:=From, to:=To} = Link) ->
    FromAddr = maps:get(addr, From),
    ToAddr   = maps:get(addr, To),
    FromStr  = erlang:list_to_binary(
		 re:replace(FromAddr, "\:", "", [global, {return, list}])),
    ToStr    = erlang:list_to_binary(
		 re:replace(ToAddr, "\:", "", [global, {return, list}])),
    HashXOR  = hash(crypto:exor(FromStr, ToStr)),

    Link#{id => HashXOR}.

is_link(Link, UserID) -> 
    ID = maps:get(id, Link),

    match=:=is_match(search_user_input_in_id(ID, UserID)).

is_not_link(Link, UserID) -> 
    ID = maps:get(id, Link),

    nomatch=:=is_match(search_user_input_in_id(ID, UserID)).

is_match(nomatch)    -> nomatch;
is_match({match, _}) -> match.

search_user_input_in_id(ID, UserID) -> 
    re:run(ID, string:concat("^", UserID)).

find_matching_link(DB, UserID) ->
    [Link || Link <- maps:get(links, DB), is_link(Link, UserID)].

find_not_matching_links(DB, UserID) ->
    [Link || Link <- maps:get(links, DB), is_not_link(Link, UserID)].

time_in_iso8601() ->
    calendar:system_time_to_rfc3339(erlang:system_time(second)).

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

if_needed_update_and_log(OldLink, Key, Val) when map_get(Key,OldLink)==Val ->
    OldLink;

if_needed_update_and_log(OldLink, Key, Val) when map_get(Key,OldLink)/=Val ->
    NewLink = maps:update(Key, Val, OldLink),   
    OldLog  = maps:get(log, NewLink, []),
    NewLog  = [#{Key => maps:get(Key, OldLink), 
		 until=> time_in_iso8601()} | OldLog],

    tag_link_with_hash_of_addrs(NewLink#{log => NewLog}).

create_link(OldDB, Link, []) ->
    OldLinkList = maps:get(links, OldDB),

    OldDB#{links := lists:append(OldLinkList, [Link])};

create_link(OldDB, _, [_]) -> 
    OldDB.

update_time_and_id(OldDB, NewID) ->
    Time  = #{'@' => time_in_iso8601()},
    NewDB = maps:merge(OldDB, Time),

    maps:merge(NewDB, #{id => NewID}).

save_db_if_ids_differ(OldDB, NewID, OldID) when NewID/=OldID ->
    File   = maps:get(file, OldDB),
    NewDB  = update_time_and_id(OldDB, NewID),
    Status = pipe(
	      NewDB,
	      [
	       fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
	       fun(Text) -> unicode:characters_to_binary(Text) end,
	       fun(Bin)  -> file:write_file(File, Bin) end
	      ]
	     ),

    #{status=>Status, db=>NewDB};

save_db_if_ids_differ(OldDB, NewID, OldID) when NewID==OldID ->
    #{status=>no_diff, db=>OldDB}.
