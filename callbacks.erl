-module(callbacks).
-behaviour(gen_server).

-import(
   helpers,
   [
    create_link/3,
    get_template/1,
    get_key/1,
    find_matching_link/2,
    find_not_matching_links/2,
    if_needed_update_and_log/3,
    open_db_or_create_from_template/2,
    pipe/2,
    save_db_if_ids_differ/3
   ]
).

-import(
   templates,
   [
    build_snippet_using_keys/1,
    get_db_template/1,
    get_link_template/0,
    hash/1,
    tag_link_with_hash_of_addrs/1
   ]
).

-export([
	 init/1, 
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
]).

init(DB) -> 
    {ok, open_db_or_create_from_template(file:consult(DB), DB)}.

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

handle_call(#{request:=stop}, _From, DB) ->
    {stop, normal, stopped, DB}.

handle_cast(#{request:=print, cmds:=Cmds}, DB) -> 
    [Values] = maps:values(Cmds),
    Text     = lists:concat(lists:join("\n", Values)),
    io:format("~s~n", [Text]),

    {noreply, DB}.

handle_info(_Info, DB)	         -> {noreply, DB}.
terminate(_Reason, _DB)	         -> ok.
code_change(_OldVsn, DB, _Extra) -> {ok, DB}.
