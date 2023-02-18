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
	 if_needed_update_and_log/3,
	 open_db_or_create_from_template/2,
	 pipe/2,
	 save_db_if_ids_differ/3
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
