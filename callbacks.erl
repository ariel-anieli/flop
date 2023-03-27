-module(callbacks).
-behaviour(gen_server).

-import(
   contracts,
   [
    get_contracts/0,
    get_faults/0
   ]
  ).
	
-import(
   helpers,
   [
    create_link/3,
    get_template/1,
    get_key/1,
    find_matching_link/2,
    find_not_matching_links/2,
    open_db_or_create_from_template/2,
    pipe/2,
    save_db_if_ids_differ/3,
    act_if_match_found/2,
    if_conform_tag_link_and_add_to_db/1,
    if_valid_shape_newlink/1,
    time_in_iso8601/0,
    if_newlink_update_list/1,
    update_key_with_val_in_link/2,
    if_request_is_valid_update_db/1
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

handle_call(#{request:=create, link:=UntaggedLink} = Args, _From, OldDB) -> 
    Shaper   = fun(Link) -> tag_link_with_hash_of_addrs(Link) end,
    Updater  = fun(Links, NewLink) -> lists:append(Links, [NewLink]) end,
    Contract = maps:get(link, get_contracts()),
    NewArgs  = Args#{
		     db       => OldDB,
		     updater  => Updater,
		     contract => Contract(UntaggedLink),
		     matches  => [UntaggedLink],
		     faults   => maps:get(link, get_faults()),
		     shaper   => Shaper
		   },
    #{db:=NewDB, status:=Status} = if_request_is_valid_update_db(NewArgs),

    {reply, #{db=>NewDB, status=>Status}, NewDB};

handle_call(#{request:=read}, From, DB) -> 
    {reply, #{db=>DB, status=>ok}, DB};

handle_call(#{request:=update, id:=UserID, 
	      key:=Key, val:=Val}=Args, From, OldDB) ->
    Updater  = fun(Links, NewLink) -> lists:append(Links, [NewLink]) end,
    Contract = maps:get(Key, get_contracts(), fun(Val) -> false end),
    NewArgs  = Args#{
		     db       => OldDB,
		     updater  => Updater,
		     contract => Contract(Val),
		     faults   => maps:get(Key, get_faults(), nofault),
		     shaper   => update_key_with_val_in_link(Key, Val),
		     matches  => find_matching_link(OldDB, UserID)
		   },
    #{db:=NewDB, status:=Status} = if_request_is_valid_update_db(NewArgs),

    {reply, #{db=>NewDB, status=>Status}, NewDB};

handle_call(#{request:=delete, id:=UserID} = Args, From, OldDB) -> 
    Updater = fun(Links, NewLink) -> Links end,
    NewArgs = Args#{
		    db      => OldDB, 
		    updater => Updater,
		    matches => find_matching_link(OldDB, UserID)
		   },

    #{db:=NewDB, status:=Status} = if_request_is_valid_update_db(NewArgs),
    {reply, #{db=>NewDB, status=>Status}, NewDB};

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


