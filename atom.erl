%%% ``The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%% 
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%% @copyright 2009 Eric Cestari
%%% @author Eric Cestari <eric+atom@ohmforce.com>
%%%   [http://www.cestari.info/]

-module(atom).
-author('eric@ohmforce.com').
-export([check/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-define(NS_ATOM, "http://www.w3.org/2005/Atom").
-define(NS_XHTML, "http://www.w3.org/1999/xhtml").


check(El) ->
	catch check1(El).


-define(CHECK_SIMPLE(Name, Parent),
	check_child([{xmlelement, Name, Attributes, []}|Rest], Parent)-> 
		Attributes,
		check_child(Rest, Parent);
	check_child([{xmlelement, Name, Attributes, [{xmlcdata, _}]}|Rest], Parent)-> 
		Attributes,
		check_child(Rest, Parent);
	check_child([{xmlelement, Name, Attributes, SubEls}|_Rest], Parent)-> 
		Attributes, SubEls, %For removing the warnings due to ?EL
		{invalid, "No subelements allowed for " ++ Parent ++ "/" ++ Name};
).

-define(CHECK_CONTENT(Name, Parent),
	check_child([{xmlelement, Name, Attributes, SubEls}|Rest], Parent)->
		case xml:get_attr_s("type",Attributes) of
			Type when Type == "text" orelse Type == "html" orelse Type == "" ->
				case SubEls of
					[{xmlcdata, _}] -> check_child(Rest, "entry");
					_ -> {invalid, "Text element : No subelements allowed for " ++ Parent ++ "/" ++ Name}
				end;
			"xhtml" ->
				case SubEls of
					[{xmlelement, "div",_,_}] -> check_child(Rest, "entry");
					_ -> {invalid, "div as only child element for " ++ Parent ++ "/" ++ Name}
				end;
			_ when Name == "content" ->
				case xml:get_attr_s("src",Attributes) of
					"" -> invalid;
					_Uri when SubEls == []-> check_child(Rest, "entry");
					_ -> {invalid, "No src attribute " ++ Parent ++ "/" ++ Name}
				end;
			_ -> {invalid, "Wrong type for " ++ Parent ++ "/" ++ Name}
		end;
	).
	
-define(CHECK_REC(Name, Parent, Required),
	check_child([{xmlelement, Name, _Attr, SubEls}|Rest], Parent)-> 
		case lists:foldl(
			fun({xmlcdata,_}, Acc)->Acc;
				({xmlelement, EName, _Attributes, _S}, Acc)->
				case lists:member(EName, Acc) of
					true -> Acc -- [EName];
					false -> Acc
				end
			end, Required, SubEls) of
			[] -> 
				case check_child(SubEls, Name) of
					valid -> check_child(Rest, Parent);
					Error -> Error
				end;
			R -> {invalid, "Required elements for "++ Parent ++ "/" ++ Name ++" are missing : " ++ string:join(R, ",")}
		end;
	).


-define(CHECK_ATTRS(Name, Parent, Required, Optional), 
	check_child([{xmlelement, Name, Attributes, _SubEls}|Rest], Parent)->
		case  check_attrs(Attributes, Required, Optional) of
			ok -> check_child(Rest, Parent);
			{invalid, Msg} -> {invalid, "Invalid attributes for " ++ Parent ++ "/" ++ Name ++" : "++ Msg}
		end;
	).

check_attrs([], [], _)->
	ok;
check_attrs([], R, _O)->
	{invalid, "Required attributes are missing : " ++ string:join(R, ",")};
check_attrs([{"xml:base", _Value}|Rest], Required, Optional)->
	check_attrs(Rest, Required, Optional);
check_attrs([{"xml:lang", _Value}|Rest], Required, Optional)->
	check_attrs(Rest, Required, Optional);
check_attrs([{"xmlns:"++_NS, _Value}|Rest], Required, Optional)->
	check_attrs(Rest, Required, Optional);
check_attrs([{Name, _Value}|Rest], Required, Optional)->
	%%TODO : better way to check namespaces.
	case lists:member($:, Name) of
		true -> check_attrs(Rest, Required, Optional);
		false ->
			case lists:member(Name, Required) of
				true -> check_attrs(Rest, Required -- [Name], Optional);
				false -> 
					case lists:member(Name, Optional) of
						true -> check_attrs(Rest, Required , Optional -- [Name]);
						false -> {invalid, "Attribute "++ Name ++" should not be here "}
					end
			end
	end.	
	

check1({xmlelement, "entry", _Attrs, Children}=El)->
	Res = lists:foldl(fun(_Fun, {invalid, _E}=Error)->Error;
					(Fun, valid)-> Fun(El)
				end, valid, [fun check_ns/1, fun check_mandatory_els/1, fun check_content_link/1] ),
	case Res of
		valid -> check_child(Children, "entry");
		Error -> Error
	end.
	
check_ns(El)->
	case xml:get_tag_attr_s("xmlns", El) of
		?NS_ATOM -> valid;
		_ -> {invalid, "Missing namespace for entry"}
	end.
check_mandatory_els({xmlelement, _Name, _Attributes, SubEls})->
	case lists:foldl(
		fun({xmlcdata,_}, Acc)->Acc;
			({xmlelement, EName, _Attr, _S}, Acc)->
			case lists:member(EName, Acc) of
				true -> Acc -- [EName];
				false -> Acc
			end
		end, ["id", "updated", "author"], SubEls) of
		[] ->
			valid;
		R -> {invalid, "Required elements for entry are missing : " ++ string:join(R, ",")}
	end.
	
check_content_link({xmlelement, _Name, _Attributes, SubEls})->
	case lists:foldl(
		fun({xmlelement, "content", _Attr, _S}, {N, M}) -> {N+1, M};
			({xmlelement, "link", Attrs, _S}, {N, M}=Acc) ->
				case xml:get_attr_s("rel", Attrs) of
					"alternate" -> {N, M+1};
					_ -> Acc
				end;
			(_, Acc) -> Acc
		end, {0, 0} , SubEls) of %% {content#, altlink# }
		{0,0} ->
			{invalid, "Required elements : content or alternate link " };
		{0, 1} -> valid;
		{1, _M} -> valid;
		{_N, _M} -> {invalid, "Only one content element allowed"}
	end.
			
	
check_child([], _Parent)->valid;

?CHECK_CONTENT("title", "entry")
?CHECK_CONTENT("subtitle", "entry")
?CHECK_CONTENT("content", "entry")
?CHECK_CONTENT("summary", "entry")
?CHECK_CONTENT("rights", "entry")

?CHECK_SIMPLE("id", "entry")
?CHECK_SIMPLE("updated", "entry")
?CHECK_SIMPLE("published", "entry")
?CHECK_SIMPLE("name", "author")
?CHECK_SIMPLE("uri", "author")
?CHECK_SIMPLE("email", "author")
?CHECK_SIMPLE("name", "contributor")
?CHECK_SIMPLE("uri", "contributor")
?CHECK_SIMPLE("email", "contributor")

?CHECK_REC("author","entry", ["name"])
?CHECK_REC("contributor","entry", ["name"])

?CHECK_ATTRS("link", "entry", ["href"], ["rel", "type", "hreflang", "length"])
?CHECK_ATTRS("category", "entry", ["term"], ["label", "scheme"])

check_child([{xmlcdata,_SpacesOrLineFeeds}|Rest], Parent) -> check_child(Rest, Parent);
%check_child([{xmlcdata,<<" ">>}|Rest], Parent) -> check_child(Rest, Parent);
%% Different namespace check here :
check_child([{xmlelement, Name, Attrs, _Children}|Rest],Parent)->
	case xml:get_attr("xmlns",Attrs) of
		{value, _NS} -> check_child(Rest, Parent);
		_ -> {invalid, "Element " ++Parent ++"/"++ Name ++ " should not be here" }
	end.


-ifdef(TEST).
top_level_simple_test()->
	El = {xmlelement, "title", [], [{xmlcdata, <<"test">>}]},
	?assertEqual(check_child([El], "entry"), valid).

top_level_simple_fail_test()->
	El = {xmlelement, "title", [], [{xmlelement, "rien", [],[]}]},
	?assertEqual(check_child([El], "entry"), {invalid,
	                   "Text element : No subelements allowed for entry/title"}).	

atom_ns_fail_test()->
	El = {xmlelement, "entry",[], []},
	?assertEqual(check(El), {invalid,"Missing namespace for entry"}).

required_elements_fail_test()->
	El = {xmlelement, "entry",[{"xmlns", ?NS_ATOM}], []},
	?assertEqual(check(El), {invalid,
	                   "Required elements for entry are missing : id,updated,author"}).

required_elements_valid_test()->
	El = {xmlelement, "entry",[{"xmlns", ?NS_ATOM}], [
			{xmlelement, "id", [], [{xmlcdata, <<"1">>}]},
			{xmlelement, "updated", [], [{xmlcdata, <<"1">>}]},
			{xmlelement, "link", [{"rel", "alternate"},{"href", "whatever"}], []},
			{xmlelement, "author", [], [{xmlelement, "name", [], [{xmlcdata, <<"cstar">>}]}]}
		]},
	?assertEqual(check(El), valid).
	
no_content_no_link_fail_test()->
	El = {xmlelement, "entry",[{"xmlns", ?NS_ATOM}], [
			{xmlelement, "id", [], [{xmlcdata, <<"1">>}]},
			{xmlelement, "updated", [], [{xmlcdata, <<"1">>}]},
			{xmlelement, "author", [], [{xmlelement, "name", [], [{xmlcdata, <<"cstar">>}]}]}
		]},
		?assertEqual(check(El), {invalid,"Required elements : content or alternate link "}).

content_no_link_valid_test()->
	 El = {xmlelement, "entry",[{"xmlns", ?NS_ATOM}], [
	 		{xmlelement, "id", [], [{xmlcdata, <<"1">>}]},
			{xmlelement, "content", [{"type", "text"}], [{xmlcdata, <<"toto">>}]},
	 		{xmlelement, "updated", [], [{xmlcdata, <<"1">>}]},
	 		{xmlelement, "author", [], [{xmlelement, "name", [], [{xmlcdata, <<"cstar">>}]}]}
	 	]},
	 	?assertEqual(check(El), valid).
too_many_content_fail_test()->
	 El = {xmlelement, "entry",[{"xmlns", ?NS_ATOM}], [
	 		{xmlelement, "id", [], [{xmlcdata, <<"1">>}]},
			{xmlelement, "content", [{"type", "text"}], [{xmlcdata, <<"toto">>}]},
			{xmlelement, "content", [{"type", "text"}], [{xmlcdata, <<"toto">>}]},
	 		{xmlelement, "updated", [], [{xmlcdata, <<"1">>}]},
	 		{xmlelement, "author", [], [{xmlelement, "name", [], [{xmlcdata, <<"cstar">>}]}]}
	 	]},
	 	?assertEqual(check(El), {invalid,"Only one content element allowed"}).
	
different_ns_element_test()->
	El = {xmlelement, "toto", [{"xmlns", "toto"}], []},
	?assertEqual(check_child([El], "entry"), valid).

wrong_element_test()->
	El = {xmlelement, "toto", [], []},
	?assertEqual(check_child([El], "entry"),  {invalid,"Element entry/toto should not be here"}).
	
attrs_link_valid_test()->
	El = {xmlelement, "link", [{"href", "lkkj"}], []},
	?assertEqual(check_child([El], "entry"), valid).
	
attrs_link_no_href_test()->
	El = {xmlelement, "link", [{"rel", "alternate"}], []},
	?assertEqual(check_child([El], "entry"),  {invalid,
	      "Invalid attributes for entry/link : Required attributes are missing : href"}).	

attrs_link_bad_attrs_test()->
	El = {xmlelement, "link", [{"sss", "alternate"}], []},
	?assertEqual(check_child([El], "entry"),  {invalid,
	            "Invalid attributes for entry/link : Attribute sss should not be here "}).
	
attrs_ns_attrs_test()->
	El = {xmlelement, "link", [{"href", "lkkj"},{"t:toto", "toto"}], []},
	?assertEqual(check_child([El], "entry"), valid).

rec_success_test()->
	El = {xmlelement, "author", [], [
			{xmlelement, "name", [], [{xmlcdata, <<"cstar">>}]},
			{xmlelement, "email", [], [{xmlcdata, <<"test@example.com">>}]}]},
	?assertEqual(check_child([El], "entry"), valid).

rec_required_failed_test()->
	El = {xmlelement, "author", [], [
			{xmlelement, "email", [], [{xmlcdata, <<"test@example.com">>}]}]},
	?assertEqual(check_child([El], "entry"), {invalid, "Required elements for entry/author are missing : name"}).

rec_fail_test()->
	El = {xmlelement, "author", [], [
			{xmlelement, "name", [], [{xmlcdata, <<"cstar">>}]},
			{xmlelement, "bademail", [], [{xmlcdata, <<"test@example.com">>}]}]},
	?assertEqual(check_child([El], "entry"),  {invalid,"Element author/bademail should not be here"}).
content_text_valid_test()->
	El = {xmlelement, "content", [{"type", "text"}], [{xmlcdata, <<"toto">>}]},
	?assertEqual(check_child([El], "entry"), valid).
	
content_nothing_valid_test()->
	El = {xmlelement, "content", [], [{xmlcdata, <<"toto">>}]},
	?assertEqual(check_child([El], "entry"), valid).
%
content_xhtml_valid_test()->
	El = {xmlelement, "content", [{"type", "xhtml"}], [{xmlelement, "div", [],[]}]},
	?assertEqual(check_child([El], "entry"), valid).
	
content_xhtml_fail_test()->
	El = {xmlelement, "content", [{"type", "xhtml"}], [{xmlcdata, "ddsf"}]},
	?assertEqual(check_child([El], "entry"), {invalid,"div as only child element for entry/content"}).
	
content_mime_valid_test()->
	El = {xmlelement, "content", [{"type", "mime"},{"src", "http://www.google.com/"}], []},
	?assertEqual(check_child([El], "entry"), valid).
%
content_mime_invalid_test()->
	El = {xmlelement, "content", [{"type", "mime"},{"src", "http://www.google.com/"}], [{xmlcdata, "ddsf"}]},
	?assertEqual(check_child([El], "entry"),  {invalid,"No src attribute entry/content"}).
	
	
test_entry()->
	lists:map(fun(FName)->
		{ok, File}=file:read_file(FName),
		Entry = binary_to_list(File),
		E = xml_stream:parse_element(Entry),
		check(E)
	end, ["tim.atom", "sam.atom"]).

-endif.
