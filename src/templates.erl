-module(templates).
-define(HASH_METHOD, md4).
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
     no shutdown
     switchport
     switchport mode trunk
     switchport trunk allowed vlan !vlans-from-aggr!";

get_template(#{type:=nxos, request:='interface ethernet', link:=Link}) 
  when not is_map_key(aggr, Link) ->
    "interface ethernet 1/!from-port!
     description !desc!
     no shutdown
     switchport
     switchport mode trunk
     switchport trunk allowed vlan !vlans-from-dev!";

get_template(#{type:=nxos, request:='interface ethernet', link:=Link}) 
  when is_map_key(aggr, Link) ->
    "interface ethernet 1/!from-port!
     description !desc!
     no shutdown
     channel-group !aggr! mode active";

get_template(#{type:=nxos, request:='interface vlan'}) ->
    "interface vlan !vlan!
     no shutdown
     vrf member !name!_!tag!
     ip address !ip!";

get_template(#{type:=nxos, request:='route map'}) ->
    "route-map !name!_!tag! permit 10
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

get_key(#{key:='from port', link:=#{from := From}}) ->
    #{port:=Port} = From,

    #{
      key => "!from-port!",
      val => integer_to_list(Port)
     };

get_key(#{key:=name, db := #{name:=Name}}) -> 
    #{
      key => "!name!",
      val => Name
     };

get_key(#{key:=ip, link := #{net:=IP}}) -> 
    #{
      key => "!ip!",
      val => IP
     };

get_key(#{key:=net, link := #{net:=Net}}) -> 
    #{
      key => "!net!",
      val => Net
     };

get_key(#{key:='to port', link := #{to:=To}}) -> 
    #{port := Port} = To,

    #{
      key => "!to-port!",
      val => integer_to_list(Port)
     };

get_key(#{key:='to aggr port', link:=Link, db := #{links:=Links}}) -> 
    GetPort = fun(#{to := #{port:=Port}}) -> Port end,
    Aggr    = maps:get(aggr, Link, 0),
    Ports   = [integer_to_list(GetPort(Link))
	       || Link <- Links, Aggr=:=maps:get(aggr,Link,0)],

    #{
      key => "!to-aggr-port!",
      val => lists:join("-", lists:sort(Ports))
     };

get_key(#{key:=tag, link := #{tag:=Tag}}) -> 
    #{
      key => "!tag!",
      val => Tag
     };

get_key(#{key:='to dev', link := #{to:=To}}) -> 
    #{dev:=Dev} = To,

    #{
      key => "!to-dev!",
      val => Dev
     };

get_key(#{key:=vlan, link := #{vlan:=VLAN}}) -> 
    #{
      key => "!vlan!",
      val => list_vlans(VLAN)
     };

get_key(#{key:='vlans from aggr', db := #{links:=Links}, link:=Link}) ->
    Aggr  = maps:get(aggr, Link, 0),
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

get_key(#{key:='vlans from dev', link := #{vlan:=VLAN}}) ->
    #{
      key => "!vlans-from-dev!",
      val => flatten_vlans(VLAN)
     };

get_key(#{key:='vlans from vrf', db := #{links:=Links}, link := #{tag:=Tag}}) ->
    VLANs = [prefix_vlan(maps:get(vlan, Link))
	     || Link <- Links, Tag=:=maps:get(tag,Link)],

    #{
      key => "!vlans-from-vrf!",
      val => lists:join(" ", lists:uniq(VLANs))
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

merge_snippets_if_needed(#{request:=Request}) 
  when Request=:=vlan;Request=:='interface vlan' ->
    fun lists:merge/1;
merge_snippets_if_needed(#{request:=Request}) 
  when Request=/=vlan,Request=/='interface vlan' ->
    fun(Snippet) -> Snippet end.

build_snippet_using_keys(#{db := #{links:=Links}, request:=Request} = Args) ->
    BuildFromLink = fun(Link)  -> build_from_link(Args#{link=>Link}) end,
    BuildAllLinks = fun(Links) -> lists:map(BuildFromLink, Links) end,
    MergeIfNeeded = merge_snippets_if_needed(Args),
    pipe(Links,
	 [
	  BuildAllLinks,
	  MergeIfNeeded,
	  fun lists:uniq/1
	 ]
	).

map_key_into_value(#{key:=Key, val:=Val}, Template) -> 
    re:replace(Template, Key, Val, [{return, list}]).

is_key_in_keyset(GoodKey) ->
    fun(#{key:=TestKey}) -> TestKey=:=GoodKey end.

add_nets_and_vlans_into_ks(#{vlan   := VLANs, 
			     alts   := Alt, 
			     zipper := Zipper,
			     tags   := Tags}) 
  when hd(Alt)>=48, hd(Alt)=<57, Tags=:='no tag' ->
    lists:zipwith(Zipper, VLANs, [Alt], trim);

add_nets_and_vlans_into_ks(#{vlan   := VLANs, 
			     alts   := Alts, 
			     zipper := Zipper,
			     tags   := Tags})
  when is_list(hd(Alts)), Tags=:='no tag' ->
    lists:zipwith(Zipper, VLANs, Alts, trim);

add_nets_and_vlans_into_ks(#{vlan   := VLANs, 
			     alts   := Alt, 
			     zipper := Zipper,
			     tags   := Tags})
  when hd(Alt)>=48, hd(Alt)=<57, is_list(hd(Tags)) ->
    lists:zipwith3(Zipper, VLANs, [Alt], Tags, trim);

add_nets_and_vlans_into_ks(#{vlan   := VLANs, 
			     alts   := Alt, 
			     zipper := Zipper,
			     tags   := Tag})
  when hd(Alt)>=48, hd(Alt)=<57, is_list(Tag) ->
    lists:zipwith3(Zipper, VLANs, [Alt], [Tag], trim);

add_nets_and_vlans_into_ks(#{vlan   := VLANs, 
			     alts   := Alts, 
			     zipper := Zipper,
			     tags   := Tags})
  when is_list(hd(Alts)), is_list(hd(Tags)) ->
    lists:zipwith3(Zipper, VLANs, Alts, Tags, trim);

add_nets_and_vlans_into_ks(#{vlan   := VLANs, 
			     alts   := Alts, 
			     zipper := Zipper,
			     tags   := Tag})
  when is_list(hd(Alts)), is_list(Tag) ->
    lists:zipwith3(Zipper, VLANs, Alts, [Tag], trim).

get_tag([#{val:=Tags}]) ->
    Tags;
get_tag(_) ->
    'no tag'.

get_mapper(Tags, Template, ButVLANs, AltKeyFromRq) when Tags=:='no tag' ->
    fun(#{"!vlan!":=VLAN, AltKeyFromRq:=Alt}) ->
	    VLANSet = #{key=>"!vlan!", val=>VLAN},
	    AltSet  = #{key=>AltKeyFromRq,  val=>Alt},
	    KeySet  = [VLANSet, AltSet | ButVLANs],
	    lists:foldr(fun map_key_into_value/2, Template, KeySet) end;

get_mapper(Tags, Template, ButVLANs, AltKeyFromRq) when Tags=/='no tag' ->
    fun(#{"!vlan!":=VLAN, AltKeyFromRq:=Alt, "!tag!":=Tag}) ->
	    VLANSet = #{key=>"!vlan!", val=>VLAN},
	    TagSet  = #{key=>"!tag!", val=>Tag},
	    AltSet  = #{key=>AltKeyFromRq,  val=>Alt},
	    KeySet  = [VLANSet, TagSet, AltSet | ButVLANs],
	    lists:foldr(fun map_key_into_value/2, Template, KeySet) end.

get_zipper(#{tags:=Tags, altkey:=AltKey}) when Tags=:='no tag' ->
    fun(VLAN, Alt) -> #{"!vlan!"=>VLAN, AltKey=>Alt} end;

get_zipper(#{tags:=Tags, altkey:=AltKey}) when Tags=/='no tag' ->
    fun(VLAN, Alt, Tag) -> #{"!vlan!"=>VLAN, "!tag!"=>Tag, AltKey=>Alt} end.

map_keyset_into_template(#{keyset:=KS, template:=T,request:=Request})
  when Request=:=vlan;Request=:='interface vlan' ->

    Alternatives  = #{
		      vlan=>"!net!", 
		      'interface vlan'=>"!ip!"
		     },
    AltKeyFromRq  = maps:get(Request, Alternatives, ''),
    IsTagInKS     = is_key_in_keyset("!tag!"),
    IsVLANinKS    = is_key_in_keyset("!vlan!"),
    IsAltKeyinKS  = is_key_in_keyset(AltKeyFromRq),
    NotVLANNorAlt = fun(#{key:=K}) 
		       -> K=/="!vlan!" 
			      andalso K=/="!tag!" 
			      andalso K=/=AltKeyFromRq end,
    
    [#{val:=VLANs}] = lists:filter(IsVLANinKS, KS),
    [#{val:=Alts}]  = lists:filter(IsAltKeyinKS, KS),

    Tags        = get_tag(lists:filter(IsTagInKS, KS)),
    ButVLANs    = lists:filter(NotVLANNorAlt, KS),
    Mapper      = get_mapper(Tags, T, ButVLANs, AltKeyFromRq),
    AugmentArgs = #{
		    vlan   => VLANs,
		    alts   => Alts,
		    tags   => Tags,
		    zipper => get_zipper(#{tags=>Tags, altkey=>AltKeyFromRq})
		   },

    AugmentedKS = add_nets_and_vlans_into_ks(AugmentArgs),

    lists:map(Mapper, AugmentedKS);

map_keyset_into_template(#{keyset:=KS, template:=T, request:=Request}) 
  when Request=/=vlan,Request=/='interface vlan' ->
    
    lists:foldr(fun map_key_into_value/2, T, KS).

get_db_template(Name) ->
    #{
      name  => get_random_str(4),
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
       fun lists:concat/1,
       fun(Addr)  -> Addr ++ "/24" end
      ]
     ).

get_random_mac_addr() ->
    pipe(
      [get_random(2, "abcdef1234567890") || Byte <- lists:seq(1,6)],
      [
       fun(Bytes) -> lists:join(":", Bytes) end,
       fun lists:concat/1
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
    FromStr  = erlang:list_to_binary(FromAddr),
    ToStr    = erlang:list_to_binary(ToAddr),
    HashXOR  = hash(crypto:exor(hash(FromStr), hash(ToStr))),

    Link#{id => HashXOR}.

hash(Term) ->
    pipe(
     Term,
      [
       fun(Str)  -> crypto:hash(?HASH_METHOD, Str) end,
       fun binary:encode_hex/1,
       fun binary:bin_to_list/1,
       fun string:lowercase/1
      ]).
