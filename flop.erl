-module(flop).
-behaviour(gen_server).

-export([
	 init/1, 
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
]).

% Database operations
-export([
	 start/1,
	 stop/0,
	 template_link/0,
	 template_db/0,
	 create/1,
	 read/0,
	 update/3,
	 delete/1,
	 save/0
]).

% Cisco Nexus CLI translators
-export([
	 description/1,
	 intf_eth/1,
	 intf_vlan/1,
	 route_map/1,
	 split_by_tag/1,
	 vlan/1,
	 print/1
]).

start(DB)	     -> gen_server:start_link({local, ?MODULE}, ?MODULE, DB, []).
stop()	             -> gen_server:call(?MODULE, #{request=>stop}).
save()	             -> gen_server:call(?MODULE, #{request=>save}).
template_link()      -> gen_server:call(?MODULE, #{request=>template_link}).
template_db()        -> gen_server:call(?MODULE, #{request=>template_db}).
create(UnTaggedLink) -> gen_server:call(?MODULE, #{
						   request=>create,
						   link=>UnTaggedLink
						  }).

read()               -> gen_server:call(?MODULE, #{request=>read}).
update(ID, Key, Val) -> gen_server:call(?MODULE, #{
						   request=>update, 
						   id=>ID,
						   key=>Key,
						   val=>Val
						  }).
delete(ID)           -> gen_server:call(?MODULE, #{request=>delete, id=>ID}).
description(Type)    -> gen_server:call(?MODULE, #{request=>description, 
						   type=>Type}).
intf_eth(Type)       -> gen_server:call(?MODULE, #{request=>'interface ethernet', 
						   type=>Type}).
intf_vlan(Type)      -> gen_server:call(?MODULE, #{request=>'interface vlan', 
						   type=>Type}).
route_map(Type)      -> gen_server:call(?MODULE, #{request=>'route map', 
						   type=>Type}).
split_by_tag(Type)   -> gen_server:call(?MODULE, #{request=>'split by tag',
						   type=>Type}).
vlan(Type)           -> gen_server:call(?MODULE, #{request=>vlan, 
						   type=>Type}).
print(Expr)          -> gen_server:call(?MODULE, #{request=>print, expr=>Expr}).
    

init(DB) -> 
    {ok, open_db_or_create_from_template(file:consult(DB), DB)}.

pipe(Arg, Funcs) -> 
    lists:foldl(fun(Func, Arg) -> Func(Arg) end, Arg, Funcs).

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
    LinksFromDB = maps:get(links, DB),
    DB#{links := [tag_link_with_hash_of_addrs(Link) || Link <- LinksFromDB]};

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

map_keyset_into_template(#{keyset:=KeySet, template:=Temp}) ->
    lists:foldr(
      fun(#{key:=Key, val:=Val}, Temp) -> 
	      re:replace(Temp, Key, Val, [{return, list}]) end,
      Temp, 
      KeySet
     ).

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

is_link(Link, UserID) -> 
    ID = maps:get(id, Link),
    match=:=is_match(search_user_input_in_id(ID, UserID)).

is_not_link(Link, UserID) -> 
    ID = maps:get(id, Link),
    nomatch=:=is_match(search_user_input_in_id(ID, UserID)).

is_match(nomatch)    -> nomatch;
is_match({match, _}) -> match.

search_user_input_in_id(ID, UserID) -> re:run(ID, string:concat("^", UserID)).

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

handle_call(#{request:=template_db}, _From, DB) -> 
    Time = erlang:system_time(second),
    {reply, get_db_template(erlang:integer_to_list(Time)), DB};

handle_call(#{request:=template_link}, _From, DB) -> 

    {reply, get_link_template(), DB};

handle_call(#{request:=create, link:=UntaggedLink}, _From, OldDB) -> 

    TaggedLink   = tag_link_with_hash_of_addrs(UntaggedLink),
    TaggedLinkID = maps:get(id, TaggedLink),
    MatchingLnks = find_matching_link(OldDB, TaggedLinkID),
    NewDB        = create_link(OldDB, TaggedLink, MatchingLnks),

    {reply, NewDB, NewDB};

handle_call(#{request:=read}, _From, DB) -> 
    
    {reply, DB, DB};

handle_call(#{request:=update, id:=UserID, key:=Key, val:=Val}, _From, OldDB) ->

    Updated       = [if_needed_update_and_log(Link, Key, Val)
		     || Link <- find_matching_link(OldDB, UserID)],
    AllButUpdated = find_not_matching_links(OldDB, UserID),
    NewDB         = OldDB#{links := lists:append(Updated, AllButUpdated)},

    {reply, NewDB, NewDB};

handle_call(#{request:=delete, id:=UserID}, _From, OldDB) -> 
    AllButDeletedLink = find_not_matching_links(OldDB, UserID),
    NewDB             = OldDB#{links := AllButDeletedLink},

    {reply, NewDB, NewDB};

handle_call(#{request:=description, type:=nxos} = Args, _From, DB) -> 
    Keys  = [name, 'to port', 'to dev'],
    Descs = build_snippet_using_keys(Args#{keys=>Keys, db=>DB}),

    {reply, #{'description'=>Descs}, DB};

handle_call(#{request:='interface ethernet', type:=nxos} = Args, _From, DB) -> 
    Keys  = [desc, 'from port', 'vlans from dev'],
    Intfs = build_snippet_using_keys(Args#{keys=>Keys, db=>DB}),

    {reply, #{'interface ethernet'=>Intfs}, DB};

handle_call(#{request:='interface vlan', type:=nxos} = Args, _From, DB) -> 
    Keys  = [vlan, ip, vrf],
    Intfs = build_snippet_using_keys(Args#{keys=>Keys, db=>DB}),

    {reply, #{'interface vlan'=>Intfs}, DB};

handle_call(#{request:='route map', type:=nxos} = Args, _From, DB) -> 
    Keys = [vrf, 'vlans from vrf'],
    Maps = build_snippet_using_keys(Args#{keys=>Keys, db=>DB}),

    {reply, #{maps=>Maps}, DB};

handle_call(#{request:='split by tag', type:=nxos} = Args, _From, DB) -> 
    Keys  = [name, tag],
    Zones = build_snippet_using_keys(Args#{keys=>Keys, db=>DB}),

    {reply, #{splits=>Zones}, DB};

handle_call(#{request:=vlan, type:=nxos} = Args, _From, DB) -> 
    Keys  = [vlan, name, net],
    VLANs = build_snippet_using_keys(Args#{keys=>Keys, db=>DB}),

    {reply, #{vlan=>VLANs}, DB};

handle_call(#{request:=save}, _From, OldDB) ->
    Links = maps:get(links, OldDB),
    NewID = hash(erlang:term_to_binary(Links)),
    Rslt  = pipe(
	      OldDB,
	      [
	       fun(Map)   -> maps:get(id, Map, "") end,
	       fun(OldID) -> save_db_if_ids_differ(OldDB, NewID, OldID) end
	      ]
	     ),

    {reply, Rslt, maps:get(db, Rslt)};

handle_call(#{request:=print, expr:=Expr}, _From, DB) -> 
    [Values] = maps:values(Expr),
    Text     = lists:concat(lists:join("\n", Values)),
    io:format("~s~n", [Text]),

    {reply, done, DB};

handle_call(#{request:=stop}, _From, DB) ->
    
    {stop, normal, stopped, DB}.

handle_cast(_Msg, DB)            -> {noreply, DB}.
handle_info(_Info, DB)	         -> {noreply, DB}.
terminate(_Reason, _DB)	         -> ok.
code_change(_OldVsn, DB, _Extra) -> {ok, DB}.
