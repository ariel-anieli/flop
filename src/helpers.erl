-module(helpers).
-define(PAGE_LENGTH, 5).
-define(CONTRACT_CHECKER, contract_checker).

-import(
   templates,
   [
    get_db_template/1,
    tag_link_with_hash_of_addrs/1
]).

-export([
	 find_matching_link/2,
	 find_not_matching_links/2,
	 open_db_or_create_from_template/2,
	 pipe/2,
	 get_page/2,
	 get_total_pages/1,
	 mark_page/2,
	 print_page/1,
	 save_db_if_ids_differ/3,
	 if_newlink_update_list/1,
	 update_key_with_val_in_link/2,
	 if_request_is_valid_update_db/1
]).

pipe(Arg, Funcs) -> 
    lists:foldl(fun(Func, Arg) -> Func(Arg) end, Arg, Funcs).

open_db_or_create_from_template({ok, [DB]}, _) ->
    Links       = maps:get(links, DB),
    TaggedLinks = [tag_link_with_hash_of_addrs(Link) || Link <- Links], 

    DB#{links := TaggedLinks};

open_db_or_create_from_template({error, _}, Name) -> 
    DBTemp = get_db_template(Name),
    Links  = maps:get(links, DBTemp),
    ID     = templates:hash(erlang:term_to_binary(Links)),
  
    maps:merge(#{id=>ID, '@'=>time_in_iso8601()}, DBTemp).

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
    
if_valid_shape_newlink(#{matches:=[], request:=Request}) 
  when Request==update;Request==delete ->
    #{status => 'not found', link =>[]};
if_valid_shape_newlink(#{matches:=Links}) 
  when is_list(Links), length(Links)>1 ->
    #{status => 'more than one match', link => Links};
if_valid_shape_newlink(#{faults:=nofault, matches:=NotAllowed}) ->
    #{status => 'not allowed', link => NotAllowed};
if_valid_shape_newlink(#{matches:=[OldLink], key:=Key, val:=Val}) 
  when map_get(Key,OldLink)==Val ->
    #{status => 'same value', link => OldLink};
if_valid_shape_newlink(#{contract:=false, faults:=Faults, matches:=BadLink}) ->
    #{status => Faults, link => BadLink};
if_valid_shape_newlink(#{shaper:=Shaper, request:=create}=Args)
  when not is_map_key(matches, Args) ->
    OldDB        = maps:get(db, Args),
    UnTaggedLink = maps:get(link, Args),
    TaggedLink   = Shaper(UnTaggedLink), 
    TaggedLinkID = maps:get(id, TaggedLink),
    Matches      = find_matching_link(OldDB, TaggedLinkID),
    NewParams    = #{matches=>Matches, link=>TaggedLink},

    if_valid_shape_newlink(maps:merge(Args, NewParams));
if_valid_shape_newlink(#{matches:=Links, request:=create}) 
  when is_list(Links), length(Links)>0 ->
    #{status=>'already in db', link => Links};
if_valid_shape_newlink(#{matches:=[], request:=create, link:=TaggedLink}) ->
    #{status => ok, link => TaggedLink};
if_valid_shape_newlink(#{matches:=[Link], request:=delete}) ->
    #{status => ok, link => Link};
if_valid_shape_newlink(#{matches:=[OldLink], request:=update, 
			 contract:=true, key:=Key} = Args) 
  when not is_map_key(Key, OldLink) ->
    DefVal  = ?CONTRACT_CHECKER:get_defval(Key),
    NewLink = OldLink#{Key => DefVal},

    if_valid_shape_newlink(Args#{matches := [NewLink]});
if_valid_shape_newlink(#{matches:=[OldLink], shaper:=Shaper, request:=update}) ->
    #{status => ok, link => Shaper(OldLink)}.

update_key_with_val_in_link(Key, Val) ->
    fun(OldLink) -> 
	    NewLink = maps:update(Key, Val, OldLink),   
	    OldLog  = maps:get(log, NewLink, []),
	    NewLog  = [#{Key => maps:get(Key, OldLink), 
			 until=> time_in_iso8601()} | OldLog],
	    tag_link_with_hash_of_addrs(NewLink#{log => NewLog})
    end.

get_userid(#{id:=ID}) ->
    ID;
get_userid(#{is_valid := #{link:=Link}}) ->
    maps:get(id, Link).

if_request_is_valid_update_db(#{db:=OldDB, updater:=Updater}=Args) ->
    IsValid    = if_valid_shape_newlink(Args),
    UserID     = get_userid(Args#{is_valid => IsValid}),
    ButMatches = find_not_matching_links(OldDB, UserID),
    UpdParams  = #{
		   oldlist           => maps:get(links, OldDB),
		   updater           => Updater, 
		   'all but matches' => ButMatches
		  },
    NewList  = if_newlink_update_list(maps:merge(IsValid, UpdParams)),

    #{
      db     => OldDB#{links:=NewList},
      status => maps:get(status, IsValid),
      links  => maps:get(link, IsValid)
     }.

update_time_and_id(OldDB, NewID) ->
    Time  = #{'@' => time_in_iso8601()},
    NewDB = maps:merge(OldDB, Time),

    maps:merge(NewDB, #{id => NewID}).

save_db_if_ids_differ(#{file:=File} = OldDB, NewID, OldID) when NewID/=OldID ->
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

if_newlink_update_list(#{link:=NewLink, updater:=Updater, status:=ok,
			 'all but matches':=ButMatches}) ->
    Updater(ButMatches, NewLink);
if_newlink_update_list(#{status:=Status, oldlist:=OldList}) when Status/=ok ->
    OldList.

get_page(Links, Page) ->
    Start = ?PAGE_LENGTH*(Page -1) + 1,
    lists:sublist(Links, Start, ?PAGE_LENGTH).

get_total_pages(Links) when length(Links) rem ?PAGE_LENGTH =/=0 ->
    trunc(floor(length(Links)/?PAGE_LENGTH)) + 1;

get_total_pages(Links) when length(Links) rem ?PAGE_LENGTH =:=0 ->
    trunc(floor(length(Links)/?PAGE_LENGTH)).

mark_page(Page, Total) ->
    list_to_atom(
      lists:concat([
		    integer_to_list(Page), 
		    "/", 
		    integer_to_list(Total)])
     ).

print_page(#{links:=Page, page:=Mark, 
	     total:=TotalLinks, pages:=TotalPages, 
	     current:=PageNum} = Args) when PageNum=<TotalPages ->
    #{
      status => ok, 
      extract => maps:without([current,pages], Args)
     };
print_page(#{pages:=TotalPages, current:=PageNum}) when PageNum>TotalPages ->
    #{
      status => 'not allowed',
      extract => #{
		   asked => PageNum, 
		   links => [],
		   pages => TotalPages
		  }
     }.

