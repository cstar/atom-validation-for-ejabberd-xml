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
-export([check/1, check_attrs/3]).
-compile(export_all).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("ejabberd.hrl").
-define(NS_ATOM, "http://www.w3.org/2005/Atom").
-define(NS_XHTML, "http://www.w3.org/1999/xhtml").


check(El) ->
	catch check1(El).

-define(CHECK_SIMPLE_1(Name, Parent),
	check_child([{xmlelement, Name, _Attributes, []}|Rest], Parent, State)-> 
		case proplists:get_value(Name, State) of 
			undefined -> 
				check_child(Rest, Parent, [{Name,found}|State]);
			found -> {invalid, "Element "++ Name ++" has already been defined."}
		end;
	check_child([{xmlelement, Name, _Attributes, [{xmlcdata, _}]}|Rest], Parent, State)-> 
		case proplists:get_value(Name, State) of 
			undefined -> 
				check_child(Rest, Parent, [{Name,found}|State]);
			found -> {invalid, "Element "++ Name ++" has already been defined."}
		end;
	check_child([{xmlelement, Name, _Attributes, _SubEls}|_Rest], Parent, _State)-> 
		{invalid, "No subelements allowed for " ++ Parent ++ "/" ++ Name};
).
-define(CHECK_SIMPLE_ANY(Name, Parent),
	check_child([{xmlelement, Name, _Attributes, []}|Rest], Parent, State)-> 
		check_child(Rest, Parent, State);
	check_child([{xmlelement, Name, _Attributes, [{xmlcdata, _}]}|Rest], Parent, State)-> 
		check_child(Rest, Parent, State);
	check_child([{xmlelement, Name, _Attributes, _SubEls}|_Rest], Parent, _State)-> 
		{invalid, "No subelements allowed for " ++ Parent ++ "/" ++ Name};
).


-define(CHECK_CONTENT(Name, Parent),
	check_child([{xmlelement, Name, Attributes, SubEls}|Rest], Parent, State)->
		case proplists:get_value(Name, State) of 
			undefined -> 
				case xml:get_attr_s("type",Attributes) of
					Type when Type == "text" orelse Type == "html" orelse Type == "" ->
						case SubEls of
							[{xmlcdata, _}] -> check_child(Rest, "entry", [{Name,found}|State]);
							_ -> {invalid, "Text element : No subelements allowed for " ++ Parent ++ "/" ++ Name}
						end;
					"xhtml" ->
						case SubEls of
							[{xmlelement, "div",_,_}] -> check_child(Rest, "entry", [{Name,found}|State]);
							_ -> {invalid, "div as only child element for " ++ Parent ++ "/" ++ Name}
						end;
					_ when Name == "content" ->
						case xml:get_attr_s("src",Attributes) of
							"" -> invalid;
							_Uri when SubEls == []-> check_child(Rest, "entry", [{Name,found}|State]);
							_ -> {invalid, "No src attribute " ++ Parent ++ "/" ++ Name}
						end;
					_ -> {invalid, "Wrong type for " ++ Parent ++ "/" ++ Name}
				end;
			found -> {invalid, "Element "++ Name ++" has already been defined."}
		end;
).
	
-define(CHECK_REC(Name, Parent, Required),
	check_child([{xmlelement, Name, _Attr, SubEls}|Rest], Parent, State)-> 
		case lists:foldl(
			fun({xmlcdata,_}, Acc)->Acc;
				({xmlelement, EName, _Attributes, _S}, Acc)->
				case lists:member(EName, Acc) of
					true -> Acc -- [EName];
					false -> Acc
				end
			end, Required, SubEls) of
			[] -> 
				case check_child(SubEls, Name, State) of
					valid -> check_child(Rest, Parent,State);
					Error -> Error
				end;
			R -> {invalid, "Required elements for "++ Parent ++ "/" ++ Name ++" are missing : " ++ string:join(R, ",")}
		end;
	).


-define(CHECK_ATTRS(Name, Parent, Required, Optional), 
	check_child([{xmlelement, Name, Attributes, _SubEls}|Rest], Parent, State)->
		case  check_attrs(Attributes, Required, Optional) of
			ok -> check_child(Rest, Parent, State);
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
				end, valid, [fun check_ns/1, fun check_mandatory_els/1, fun check_content_link_summary/1] ),
	case Res of
		valid -> check_child(Children, "entry", []);
		Error -> Error
	end;

check1([{xmlcdata,_ }|Rest])->
	check1(Rest);
check1([{xmlelement, "entry", _Attrs, _Children}=El|_Rest])->
	check1(El);
check1(_)->
	{invalid, "only one entry element allowed"}.
	


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
		end, ["id", "updated"], SubEls) of
		[] ->
			valid;
		R -> {invalid, "Required elements for entry are missing : " ++ string:join(R, ",")}
	end.
	
check_content_link_summary({xmlelement, _Name, _Attributes, SubEls})->
	case lists:foldl(
		fun({xmlelement, "content", _Attr, _S}=Content, {C, L, S, Type}) -> 
				case is_binary_type(Content) of
					true -> {C+1, L, S, binary};
					false -> {C+1, L, S, text}
				end;
			({xmlelement, "summary", _Attr, _S},{C, L, S, Type}) -> {C, L, S+1, Type};
			({xmlelement, "link", Attrs, _S}, {C, L, S, Type}=Acc) ->
				case xml:get_attr_s("rel", Attrs) of
					"alternate" -> {C, L+1, S, Type};
					_ -> Acc
				end;
			(_, Acc) -> Acc
		end, {0, 0, 0, text} , SubEls) of %% {content#, altlink#, summary#, content_type }
		{0,0,_,_} ->
			{invalid, "Required elements : content or alternate link " };
		{0, 1, _,_} -> valid;
		{1, _L, 0, text} -> valid;
		{1, _L, 1, binary} -> valid;
		{1, _L, 0, binary} -> {invalid,"atom:content is binary, atom:summary is REQUIRED."};
		{_C, _l, _,_} -> {invalid, "Only one content element allowed"}
	end.
			
	
check_child([], _Parent, _State)->valid;

?CHECK_CONTENT("title", "entry")
?CHECK_CONTENT("content", "entry")
?CHECK_CONTENT("summary", "entry")
?CHECK_CONTENT("rights", "entry")

?CHECK_SIMPLE_1("id", "entry")
?CHECK_SIMPLE_1("updated", "entry")
?CHECK_SIMPLE_1("published", "entry")
?CHECK_REC("source","entry", [])

?CHECK_ATTRS("category", "entry", ["term"], ["label", "scheme"])

check_child([{xmlelement, Person, Attributes, SubEls}|Rest], "entry", State) when Person == "author" orelse Person == "contributor" ->
	case lists:foldl(fun({xmlelement, "name", _, _}, {0, U, E}) -> {1, U, E};
						({xmlelement, "name", _, _}, {1, U, E}) -> {invalid, "Too many names for "++ Person};
						({xmlelement, "uri", _, _}, {N, 0, E}) -> {N, 1, E};
						({xmlelement, "uri", _, _}, {N, 1, E}) -> {invalid, "Too many URIs for "++ Person};
						({xmlelement, "email", _, _}, {N, U, 0}) -> {N, U, 1};
						({xmlelement, "email", _, _}, {N, U, 1}) -> {invalid, "Too many Emails for "++ Person};
						({xmlelement, Name, Attrs, _}, Acc)->
							case lists:member($:, Name) of 
								true -> Acc;
								false ->
									case proplists:get_value("xmlns", Attrs) of
										undefined -> {invalid, "Invalid element entry/"++Person ++"/"++Name};
										_ -> Acc
									end
							end;
						(_, _) -> {invalid, "Invalid subelements for entry/"++Person}
					end, {0,0,0}, SubEls) of
			{invalid, Error} -> {invalid, Error};
			{0, _, _} -> {invalid, "Person constructs MUST contain exactly one atom:name element."};
			_ -> valid
	end;
					

check_child([{xmlelement, "link", Attributes, _SubEls}|Rest], "entry", State)->
	case  check_attrs(Attributes, ["href"], ["rel", "type", "hreflang", "length"]) of
		ok -> 
			case proplists:get_value("rel", Attributes) of
				"alternate" ->
					K = {alt,
							proplists:get_value("type", Attributes),
							proplists:get_value("hreflang", Attributes)},
					case proplists:get_value(K, State) of 
						undefined -> 
							check_child(Rest, "entry", [{K, true}|State]);
						true -> 
							{invalid,"atom:entry elements MUST NOT contain more than one atom:link element with a rel attribute value of alternate that has the same combination of type and hreflang attribute values."}
					end;
				_ -> check_child(Rest, "entry", State)
			end;
		{invalid, Msg} -> {invalid, "Invalid attributes for entry/link : "++ Msg}
	end;
	
check_child([{xmlcdata,_SpacesOrLineFeeds}|Rest], Parent, State) -> check_child(Rest, Parent,State);
%% Different namespace check here :
check_child([{xmlelement, Name, Attrs, _Children}|Rest],Parent, State)->
	case xml:get_attr("xmlns",Attrs) of
		{value, _NS} -> check_child(Rest, Parent,State);
		_ -> {invalid, "Element " ++Parent ++"/"++ Name ++ " should not be here" }
	end.


is_binary_type({xmlelement, Name, Attrs, _Sub})->
	case proplists:get_value("type", Attrs) of
		undefined -> false;
		Type when Type == "xhtml" orelse Type == "html" orelse Type == "text" -> false;
		"text/" ++ _Data -> false;
		Other ->
			case lists:reverse(Other) of
				"lmx+" ++ Rest -> false;
				"lmx/" ++ Rest  -> false;
				_ -> true
			end
	end.

-ifdef(TEST).
top_level_simple_test()->
	El = {xmlelement, "title", [], [{xmlcdata, <<"test">>}]},
	?assertEqual(check_child([El], "entry", []), valid).

top_level_simple_fail_test()->
	El = {xmlelement, "title", [], [{xmlelement, "rien", [],[]}]},
	?assertEqual(check_child([El], "entry",[]), {invalid,
	                   "Text element : No subelements allowed for entry/title"}).	

too_many_titles_test()->
	El = {xmlelement, "title", [], [{xmlelement, "rien", [],[]}]},
	?assertEqual(check_child([El], "entry",[{"title", found}]), {invalid,
	                   "Element title has already been defined."}).
too_many_ids_test()->
	El = {xmlelement, "id", [],  [{xmlcdata, <<"1">>}]},
	?assertEqual(check_child([El], "entry",[{"id", found}]), {invalid,
	                   "Element id has already been defined."}).
atom_ns_fail_test()->
	El = {xmlelement, "entry",[], []},
	?assertEqual(check(El), {invalid,"Missing namespace for entry"}).

required_elements_fail_test()->
	El = {xmlelement, "entry",[{"xmlns", ?NS_ATOM}], []},
	?assertEqual(check(El), {invalid,
	                   "Required elements for entry are missing : id,updated"}).

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
	?assertEqual(check_child([El], "entry",[]), valid).

wrong_element_test()->
	El = {xmlelement, "toto", [], []},
	?assertEqual(check_child([El], "entry",[]),  {invalid,"Element entry/toto should not be here"}).
	
attrs_link_valid_test()->
	El = {xmlelement, "link", [{"href", "lkkj"}], []},
	?assertEqual(check_child([El], "entry",[]), valid).
	
attrs_link_no_href_test()->
	El = {xmlelement, "link", [{"rel", "alternate"}], []},
	?assertEqual(check_child([El], "entry",[]),  {invalid,
	      "Invalid attributes for entry/link : Required attributes are missing : href"}).	

attrs_link_bad_attrs_test()->
	El = {xmlelement, "link", [{"sss", "alternate"}], []},
	?assertEqual(check_child([El], "entry",[]),  {invalid,
	            "Invalid attributes for entry/link : Attribute sss should not be here "}).
	
attrs_ns_attrs_test()->
	El = {xmlelement, "link", [{"href", "lkkj"},{"t:toto", "toto"}], []},
	?assertEqual(check_child([El], "entry",[]), valid).

rec_success_test()->
	El = {xmlelement, "author", [], [
			{xmlelement, "name", [], [{xmlcdata, <<"cstar">>}]},
			{xmlelement, "email", [], [{xmlcdata, <<"test@example.com">>}]}]},
	?assertEqual(check_child([El], "entry",[]), valid).

rec_required_failed_test()->
	El = {xmlelement, "author", [], [
			{xmlelement, "email", [], [{xmlcdata, <<"test@example.com">>}]}]},
	?assertEqual(check_child([El], "entry",[]), 		{invalid,
			                   "Person constructs MUST contain exactly one atom:name element."}).

rec_fail_test()->
	El = {xmlelement, "author", [], [
			{xmlelement, "name", [], [{xmlcdata, <<"cstar">>}]},
			{xmlelement, "bademail", [], [{xmlcdata, <<"test@example.com">>}]}]},
	?assertEqual(check_child([El], "entry",[]),  {invalid,"Invalid element entry/author/bademail"}).
content_text_valid_test()->
	El = {xmlelement, "content", [{"type", "text"}], [{xmlcdata, <<"toto">>}]},
	?assertEqual(check_child([El], "entry",[]), valid).
	
content_nothing_valid_test()->
	El = {xmlelement, "content", [], [{xmlcdata, <<"toto">>}]},
	?assertEqual(check_child([El], "entry",[]), valid).
%
content_xhtml_valid_test()->
	El = {xmlelement, "content", [{"type", "xhtml"}], [{xmlelement, "div", [],[]}]},
	?assertEqual(check_child([El], "entry",[]), valid).
	
content_xhtml_fail_test()->
	El = {xmlelement, "content", [{"type", "xhtml"}], [{xmlcdata, "ddsf"}]},
	?assertEqual(check_child([El], "entry",[]), {invalid,"div as only child element for entry/content"}).
	
content_mime_valid_test()->
	El = {xmlelement, "content", [{"type", "mime"},{"src", "http://www.google.com/"}], []},
	?assertEqual(check_child([El], "entry",[]), valid).
%
content_mime_invalid_test()->
	El = {xmlelement, "content", [{"type", "mime"},{"src", "http://www.google.com/"}], [{xmlcdata, "ddsf"}]},
	?assertEqual(check_child([El], "entry",[]),  {invalid,"No src attribute entry/content"}).
	
double_alt_link_fail_test()->
	Els = [{xmlelement, "link", [{"rel", "alternate"}, {"href", "lkkj"},{"type", "toto"}, {"hreflang", "fr"}], []},
		   {xmlelement, "link", [{"rel", "alternate"}, {"href", "lkdkj"},{"type", "toto"}, {"hreflang", "fr"}], []}],
	?assertEqual(check_child(Els, "entry",[]),  {invalid,"atom:entry elements MUST NOT contain more than one atom:link element with a rel attribute value of alternate that has the same combination of type and hreflang attribute values."}).

%
double_alt_link_success_test()->
	Els = [{xmlelement, "link", [{"rel", "alternate"}, {"href", "lkkj"},{"type", "toto"}, {"hreflang", "en"}], []},
		   {xmlelement, "link", [{"rel", "alternate"}, {"href", "lkdkj"},{"type", "toto"}, {"hreflang", "fr"}], []}],
	?assertEqual(check_child(Els, "entry",[]),  valid).


binary_content_and_no_summary_fail_test()->
	El = {xmlelement, "entry", [], [
		   {xmlelement, "content", [{"type", "image/png"}], []}
		]},
	?assertEqual(check_content_link_summary(El),  {invalid,"atom:content is binary, atom:summary is REQUIRED."}).

binary_content_with_summary_test()->
	El = {xmlelement, "entry", [], [
		   {xmlelement, "content", [{"type", "image/png"}], []},
		   {xmlelement, "summary", [{"type", "text"}], [{xmlcdata, <<"toto">>}]}
		]},
	?assertEqual(check_content_link_summary(El),  valid).


binary1_xml_test()->
	?assertEqual(is_binary_type({xmlelement, "content", [{"type", "application/xml"}], []}), false).
	
binary2_png_test()->
	?assertEqual(is_binary_type({xmlelement, "content", [{"type", "image/png"}], []}), true).
	
binary3_xml_test()->
		?assertEqual(is_binary_type({xmlelement, "content", [{"type", "text/xml"}], []}), false).

binary4_xml_test()->
		?assertEqual(is_binary_type({xmlelement, "content", [{"type", "application/html+xml"}], []}), false).

binary5_xml_test()->
		?assertEqual(is_binary_type({xmlelement, "content", [], []}), false).

person_valid_test()->
	El = {xmlelement, "author", [], [{xmlelement, "name", [], [{xmlcdata, <<"cstar">>}]}, {xmlelement, "email", [], [{xmlcdata, <<"eric@cestari.info">>}]}]},
	?assertEqual(check_child([El], "entry",[]),  valid).
	
person_no_name_fail_test()->
	El = {xmlelement, "author", [], [{xmlelement, "email", [], [{xmlcdata, <<"eric@cestari.info">>}]}]},
	?assertEqual(check_child([El], "entry",[]),  {invalid,
	                   "Person constructs MUST contain exactly one atom:name element."}).

person_valid_ns1_test()->
	El = {xmlelement, "author", [], [{xmlelement, "name", [], [{xmlcdata, <<"cstar">>}]},{xmlelement, "test:toto", [], []}, {xmlelement, "email", [], [{xmlcdata, <<"eric@cestari.info">>}]}]},
	?assertEqual(check_child([El], "entry",[]),  valid).

person_valid_ns2_test()->
	El = {xmlelement, "author", [], [{xmlelement, "name", [], [{xmlcdata, <<"cstar">>}]},{xmlelement, "toto", [{"xmlns", "test"}], []}, {xmlelement, "email", [], [{xmlcdata, <<"eric@cestari.info">>}]}]},
	?assertEqual(check_child([El], "entry",[]),  valid).
	
person_too_many_emails_fail_test()->
	El = {xmlelement, "author", [], [{xmlelement, "name", [], [{xmlcdata, <<"cstar">>}]}, {xmlelement, "email", [], [{xmlcdata, <<"eric@cestari.info">>}]},{xmlelement, "email", [], [{xmlcdata, <<"eric@cestari.info">>}]}]},
	?assertEqual(check_child([El], "entry",[]),  {invalid,"Too many Emails for author"}).


test_entry()->
	lists:map(fun(FName)->
		{ok, File}=file:read_file(FName),
		Entry = binary_to_list(File),
		E = xml_stream:parse_element(Entry),
		check(E)
	end, ["tim.atom", "sam.atom"]).

-endif.
