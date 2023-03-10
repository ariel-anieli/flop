-module(helpers).

-import(
   templates,
   [
    get_db_template/1,
    get_link_template/0,
    get_template/1,
    get_key/1,
    tag_link_with_hash_of_addrs/1
]).

-export([
	 create_link/3,
	 find_matching_link/2,
	 find_not_matching_links/2,
	 if_needed_update_and_log/1,
	 open_db_or_create_from_template/2,
	 pipe/2,
	 save_db_if_ids_differ/3,
	 act_if_match_found/2,
	 if_act_done_update_db/3,
	 if_act_done_delete_from_db/3,
	 if_conform_tag_link_and_add_to_db/1
]).

pipe(Arg, Funcs) -> 
    lists:foldl(fun(Func, Arg) -> Func(Arg) end, Arg, Funcs).

open_db_or_create_from_template({ok, [DB]}, _) ->
    Links       = maps:get(links, DB),
    TaggedLinks = [tag_link_with_hash_of_addrs(Link) || Link <- Links], 

    DB#{links := TaggedLinks};

open_db_or_create_from_template({error, _}, Name) -> 
    get_db_template(Name).

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

if_conform_tag_link_and_add_to_db(#{link:=Link, db:=OldDB, conform:=true}) ->
    TaggedLink   = tag_link_with_hash_of_addrs(Link),
    TaggedLinkID = maps:get(id, TaggedLink),
    MatchingLnks = find_matching_link(OldDB, TaggedLinkID),

    create_link(OldDB, TaggedLink, MatchingLnks);

if_conform_tag_link_and_add_to_db(#{db:=OldDB, conform:=false, faults:=Faults}) ->
    #{db=>OldDB, status=>maps:get(link, Faults)}.
    

if_needed_update_and_log(#{key:=Key, contract:=Contract, link:=OldLink}) 
  when not is_map_key(Key, Contract) ->
    #{
      link   => OldLink,
      status => 'not allowed'
     };

if_needed_update_and_log(#{link:=OldLink, key:=Key, val:=Val}) 
  when map_get(Key,OldLink)==Val ->
    #{
      link   => OldLink,
      status => 'same value'
     };

if_needed_update_and_log(#{contract:=Contract, val:=Val, key:=Key}=Args) ->
    IsConform = maps:get(Key, Contract),
    if_conform_update_and_log(Args#{conform=>IsConform(Val)}).

if_conform_update_and_log(#{conform:=true, link:=OldLink, 
			    key:=Key, val:=Val}) ->
    NewLink = maps:update(Key, Val, OldLink),   
    OldLog  = maps:get(log, NewLink, []),
    NewLog  = [#{Key => maps:get(Key, OldLink), 
		 until=> time_in_iso8601()} | OldLog],

    #{
      link   => tag_link_with_hash_of_addrs(NewLink#{log => NewLog}),
      status => ok
     };

if_conform_update_and_log(#{conform:=false, link:=OldLink, 
			    key:=Key, faults:=Faults}) ->
    #{
      link   => OldLink,
      status => maps:get(Key, Faults)
     }.

create_link(OldDB, Link, []) ->
    OldLinkList = maps:get(links, OldDB),
    NewDB = OldDB#{links := lists:append(OldLinkList, [Link])},
    #{db=>NewDB, status=>ok};

create_link(OldDB, _, [_]) -> 
    #{db=>OldDB, status=>'already in db'}.

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
    #{status=>'no diff', db=>OldDB}.

act_if_match_found(_, Links) when is_list(Links), length(Links)>1 ->
    #{status=>'nore than one match'};

act_if_match_found(Act, [Link]) when is_map(Link) ->
    Act(Link);

act_if_match_found(_, []) ->
    #{status=>'not found'}.

if_act_done_update_db(OldDB, Links, #{status:=ok, link:=NewLink}) ->
    NewList = lists:append(Links, [NewLink]),
    OldDB#{links := NewList};

if_act_done_update_db(OldDB, Links, #{status:=_}) ->
    OldDB.

if_act_done_delete_from_db(OldDB, Links, #{status:=ok}) ->
    OldDB#{links := Links};

if_act_done_delete_from_db(OldDB, Links, #{status:=_}) ->
    OldDB.
