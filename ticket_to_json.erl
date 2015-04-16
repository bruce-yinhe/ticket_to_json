%% Copyright Bruce Yinhe
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(ticket_to_json).
-export([start/1, result/0, get_issue_from_file/1, get_all_issues/3]).
-export([format_list/1]).

-define(TICKET_ID, "Id").
-define(TICKET_LABEL, "Label").
-define(TICKET_STATUS, "Status").
-define(TICKET_TYPE, "Type").
-define(TICKET_SOURCE, "Source").
-define(TICKET_PRIORITY, "Priority").
-define(TICKET_RELATED_ID, "RelatedId(s)").
-define(TICKET_CREATOR, "Creator").
-define(TICKET_CREATION_DATE, "CreationDate").
-define(TICKET_ORIGINATORS, "Originator(s)").
-define(TICKET_OTP_RELS, "OTP rel(s)").
-define(TICKET_APPLICATION, "Application").
-define(TICKET_HIGHLIGHT, "Highlight").
-define(TICKET_INCOMPATIBLE, "Incompatible").
-define(TICKET_RELEASE_NOTE, "Release note").
-define(TICKET_ASSIGNED_TO,     "AssignedTo").
-define(TICKET_FIXED_IN_REL,    "FixedInRel").
-define(TICKET_PLANNED_FOR_REL, "PlannedForRel").
-define(TICKET_TEST_CASE, "Test case").
-define(TICKET_REV, "TicketRev").
-define(TICKET_LAST_UPDATE, "LastUpdate").
-define(TICKET_NOTES, "Notes").
-define(TICKET_DESCR, "Description").
-define(TICKET_PURPOSE, "Purpose").
-define(TICKET_GIT_BRANCH, "GitBranch").
-define(TICKET_GIT_RANGE, "GitRange").
-define(DEFAULT_PRIORITY, "4").

start(Path) ->
    Project =
	#{<<"name">> => <<"Erlang/OTP">>,
	  <<"key">> => <<"ERL">>,
	  <<"components">> => [
			       <<"erts">>,
			       <<"kernel">>
			      ],
	  <<"issues">> => []
	 },

    {ok, Filenames} = file:list_dir(Path),
    
    %% Issues = get_all_issues(Filenames, Path, []),

    Issues = lists:map(fun (Filename) -> ticket_to_json:get_issue_from_file(Path++"/"++Filename) end, Filenames),

    Project1 = maps:put(
		 <<"issues">>,
		 lists:merge(Issues, maps:get(<<"issues">>, Project, [])),
		 Project
		),
    
    Projects = 
	#{<<"projects">> => [Project1]},
    Projects.

get_all_issues([], _Path, Issues)->
    Issues;
get_all_issues([Filename | Rest], Path, Issues) ->
    Issue = get_issue_from_file(Path++"/"++Filename),
    io:format("~n~s/~s", [Path, Filename]),
    get_all_issues(Rest, Path, [Issue | Issues]).

get_issue_from_file(Path) ->
    _Example_tokens = 
	[{"Id","OTP-12345"},
	 {"Label","Textfield"},
	 {"Status","solved"},
	 {"Type","bug"},
	 {"Source","external"},
	 {"Priority","1"},
	 {"RelatedId(s)","Textfield"},
	 {"Creator","lukas"},
	 {"CreationDate","2014-07-11 14:59:06"},
	 {"Originator(s)","Textfield"},
	 {"OTP rel(s)","R16B03"},
	 {"Application","erts kernel"},
	 {"Highlight","no"},
	 {"Incompatible","yes"},
	 {"Release note","Textarea"},
	 {"AssignedTo","assignee"},
	 {"FixedInRel","Textfield"},
	 {"PlannedForRel","Textfield"},
	 {"Test case","Textfield"},
	 {"Description","Textfield"},
	 {"Purpose","Textfield"},
	 {"Notes","Textfield"}],

    {ok, Real_tokens} = ticket_lib:scan(Path),
    EmptyTicket = maps:new(),
    %%format_list(Real_tokens),
    {ok, Issue} = tokens_to_ticket(Real_tokens, EmptyTicket),
    Issue.

format_list(L) when is_list(L) ->
    io:format("["),
    fnl(L),
    io:format("]").

fnl([H]) ->
    io:format("~p", [H]);
fnl([H|T]) ->
    io:format("~p,", [H]),
    fnl(T);
fnl([]) ->
    ok.

tokens_to_ticket([], Ticket) ->
    {ok, Ticket};
tokens_to_ticket([{Label, Text} | T], Ticket) ->
    case Label of
        ?TICKET_ID -> 
	    tokens_to_ticket(T, maps:put(<<"key">>, list_to_binary(Text), Ticket));
        ?TICKET_LABEL -> 
	    tokens_to_ticket(T, maps:put(<<"summary">>, unicode:characters_to_binary(Text), Ticket));
        ?TICKET_STATUS ->
            case string:to_lower(Text) of
                "new" ->
                    tokens_to_ticket(T, maps:put(<<"status">>, unicode:characters_to_binary(Text), Ticket));
                "open" ->
                    tokens_to_ticket(T, maps:put(<<"status">>, unicode:characters_to_binary(Text), Ticket));
		"reviewed" ->
                    tokens_to_ticket(T, maps:put(<<"status">>, unicode:characters_to_binary(Text), Ticket));
		"promoted" ->
                    tokens_to_ticket(T, maps:put(<<"status">>, unicode:characters_to_binary(Text), Ticket));
                "deferred" ->
                    tokens_to_ticket(T, maps:put(<<"status">>, unicode:characters_to_binary(Text), Ticket));
                "solved" ->
                    tokens_to_ticket(T, maps:put(<<"status">>, unicode:characters_to_binary(Text), Ticket));
                "closed" ->
                    tokens_to_ticket(T, maps:put(<<"status">>, unicode:characters_to_binary(Text), Ticket));
                "cancelled" ->
                    tokens_to_ticket(T, maps:put(<<"status">>, unicode:characters_to_binary(Text), Ticket));
		BadValue ->
                    NewValue = "new",
		    Details = lists:concat(["Bad Status: " ,
                                            "\"\"",
                                            BadValue,
                                            " -> ", NewValue, "\n"]),
		    New = 
			#{<<"body">> => unicode:characters_to_binary(Details),
			  <<"author">> => <<"bruce">>,
			  <<"created">> => <<"2012-08-31T17:59:02.161+0100">>
			 },
		    NewComments = [New | maps:get(<<"comments">>, Ticket, [])],
		    Ticket1 = maps:put(<<"comments">>, NewComments, Ticket),
		    tokens_to_ticket(T, maps:put(<<"status">>, unicode:characters_to_binary(Text), Ticket1))
            end;
        ?TICKET_TYPE ->
            case string:to_lower(Text) of
                "bug" ->
                    tokens_to_ticket(T, maps:put(<<"issueType">>, unicode:characters_to_binary(Text), Ticket));
                "job" ->
                    tokens_to_ticket(T, maps:put(<<"issueType">>, unicode:characters_to_binary(Text), Ticket));
                "request" ->
                    tokens_to_ticket(T, maps:put(<<"issueType">>, unicode:characters_to_binary(Text), Ticket));
		BadValue ->
                    NewValue = "bug",
		    Details = lists:concat(["Bad Type: " ,
                                            "\"\"",
                                            BadValue,
					    " -> ", NewValue, "\n"]),
		    New = 
			#{<<"body">> => unicode:characters_to_binary(Details),
			  <<"author">> => <<"bruce">>,
			  <<"created">> => <<"2012-08-31T17:59:02.161+0100">>
			 },
		    NewComments = [New | maps:get(<<"comments">>, Ticket, [])],
		    Ticket1 = maps:put(<<"comments">>, NewComments, Ticket),
		    tokens_to_ticket(T, maps:put(<<"issueType">>, unicode:characters_to_binary(Text), Ticket1))
		    
            end;
        ?TICKET_SOURCE ->
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    Source = 
		#{<<"fieldName">> => <<"Source">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:select">>
		 },
            case string:to_lower(Text) of
                "internal" ->
		    NewCustomFieldValues = [maps:put(<<"value">>, unicode:characters_to_binary(Text), Source) | CustomFieldValues],
		    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
                "external" ->
                    NewCustomFieldValues = [maps:put(<<"value">>, unicode:characters_to_binary(Text), Source) | CustomFieldValues],
		    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
                "opensource" ->
		    NewCustomFieldValues = [maps:put(<<"value">>, unicode:characters_to_binary(Text), Source) | CustomFieldValues],
		    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
		BadValue ->
                    NewValue = "internal",
		    Details = lists:concat(["Bad Source: " ,
                                            "\"\"",
                                            BadValue,
					    " -> ", NewValue, "\n"]),
		    New = 
			#{<<"body">> => unicode:characters_to_binary(Details),
			  <<"author">> => <<"bruce">>,
			  <<"created">> => <<"2012-08-31T17:59:02.161+0100">>
			 },
		    NewComments = [New | maps:get(<<"comments">>, Ticket, [])],
		    Ticket1 = maps:put(<<"comments">>, NewComments, Ticket),
		    NewCustomFieldValues = [maps:put(<<"value">>, unicode:characters_to_binary(Text), Source) | CustomFieldValues],
		    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket1))
            end;
        ?TICKET_PRIORITY ->
            case Text of
                "1" ->
                    tokens_to_ticket(T, maps:put(<<"priority">>, unicode:characters_to_binary(Text), Ticket));
                "2" ->
                    tokens_to_ticket(T, maps:put(<<"priority">>, unicode:characters_to_binary(Text), Ticket));
                "3" ->
                    tokens_to_ticket(T, maps:put(<<"priority">>, unicode:characters_to_binary(Text), Ticket));
                "4" ->
                    tokens_to_ticket(T, maps:put(<<"priority">>, unicode:characters_to_binary(Text), Ticket));
		BadValue ->
                    NewValue = ?DEFAULT_PRIORITY,
		    Details = lists:concat(["Bad Priority: " ,
                                            "\"\"",
                                            BadValue,
					    " -> ", NewValue, "\n"]),
		    New = 
			#{<<"body">> => unicode:characters_to_binary(Details),
			  <<"author">> => <<"bruce">>,
			  <<"created">> => <<"2012-08-31T17:59:02.161+0100">>
			 },
		    NewComments = [New | maps:get(<<"comments">>, Ticket, [])],
		    Ticket1 = maps:put(<<"comments">>, NewComments, Ticket),
		    tokens_to_ticket(T, maps:put(<<"priority">>, unicode:characters_to_binary(Text), Ticket1))
            end;
        ?TICKET_RELATED_ID ->
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    RelatedID =
		#{<<"fieldName">> => <<"RelatedID">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textfield">>},
	    NewCustomFieldValues = [maps:put(<<"value">>, unicode:characters_to_binary(Text), RelatedID) | CustomFieldValues],
	    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
        ?TICKET_CREATOR -> 
	    tokens_to_ticket(T, maps:put(<<"reporter">>, unicode:characters_to_binary(Text), Ticket));
        ?TICKET_CREATION_DATE ->
	    tokens_to_ticket(T, maps:put(<<"created">>, unicode:characters_to_binary(re:replace(Text, " ", "T", [{return, list}])), Ticket));
        ?TICKET_ORIGINATORS ->
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    Originators =
		#{<<"fieldName">> => <<"Originator/s">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textfield">>},
	    NewCustomFieldValues = [maps:put(<<"value">>, unicode:characters_to_binary(Text), Originators) | CustomFieldValues],
	    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
        ?TICKET_OTP_RELS -> 
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    OTP_RELS =
		#{<<"fieldName">> => <<"OTP release/s">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:multiversion">>},
	    NewCustomFieldValues = [maps:put(<<"value">>, unicode:characters_to_binary(Text), OTP_RELS) | CustomFieldValues],
	    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
        ?TICKET_APPLICATION -> 
	    Components = lists:map(fun(A)->unicode:characters_to_binary(A) end, string:tokens(Text, " ")),
	    tokens_to_ticket(T, maps:put(<<"components">>, Components, Ticket));
        ?TICKET_HIGHLIGHT ->
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    Highlight = 
		#{<<"fieldName">> => <<"Highlight">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:select">>
		 },
	    case string:to_lower(Text) of
                "yes" ->
		    NewCustomFieldValues = [maps:put(<<"value">>, unicode:characters_to_binary(Text), Highlight) | CustomFieldValues],
		    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
                "no" ->
                    NewCustomFieldValues = [maps:put(<<"value">>, unicode:characters_to_binary(Text), Highlight) | CustomFieldValues],
		    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
                BadValue ->
                    NewValue = "no",
                    Details = lists:concat(["Changed Highlight to default: " ,
                                            "\"\"",
                                            BadValue,
                                            " -> ", NewValue, "\n"]),
		    New = 
			#{<<"body">> => unicode:characters_to_binary(Details),
			  <<"author">> => <<"bruce">>,
			  <<"created">> => <<"2012-08-31T17:59:02.161+0100">>
			 },
		    NewComments = [New | maps:get(<<"comments">>, Ticket, [])],
		    Ticket1 = maps:put(<<"comments">>, NewComments, Ticket),
		    NewCustomFieldValues = [maps:put(<<"value">>, unicode:characters_to_binary(Text), Highlight) | CustomFieldValues],
		    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket1))
            end;
        ?TICKET_INCOMPATIBLE ->
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    Incompatible = 
		#{<<"fieldName">> => <<"Incompatible">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textfield">>
		 },
	    case string:to_lower(Text) of
                "no" ->
		    NewCustomFieldValues = [maps:put(<<"value">>, unicode:characters_to_binary(Text), Incompatible) | CustomFieldValues],
		    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
                "" ->
                    NewValue = "no",
                    Details = lists:concat(["Changed Incompatible to default: " ,
                                            "\"\"",
                                            " -> ", NewValue, "\n"]),
		    New = 
			#{<<"body">> => unicode:characters_to_binary(Details),
			  <<"author">> => <<"bruce">>,
			  <<"created">> => <<"2012-08-31T17:59:02.161+0100">>
			 },
		    NewComments = [New | maps:get(<<"comments">>, Ticket, [])],
		    Ticket1 = maps:put(<<"comments">>, NewComments, Ticket),
		    NewCustomFieldValues = [maps:put(<<"value">>, unicode:characters_to_binary(Text), Incompatible) | CustomFieldValues],
		    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket1)); 
                _ ->
		    NewCustomFieldValues = [maps:put(<<"value">>, unicode:characters_to_binary(Text), Incompatible) | CustomFieldValues],
		    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket))
            end;
        ?TICKET_RELEASE_NOTE ->
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    RELEASE_NOTE =
		#{<<"fieldName">> => <<"Release Notes">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textarea">>},
	    NewCustomFieldValues = [maps:put(<<"value">>, unicode:characters_to_binary(Text), RELEASE_NOTE) | CustomFieldValues],
	    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
        ?TICKET_ASSIGNED_TO ->
	    tokens_to_ticket(T, maps:put(<<"assignee">>, unicode:characters_to_binary(Text), Ticket));
        ?TICKET_FIXED_IN_REL ->	   
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    FIXED_IN_REL =
		#{<<"fieldName">> => <<"FixedInRel">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textfield">>},
	    NewCustomFieldValues = [maps:put(<<"value">>, unicode:characters_to_binary(Text), FIXED_IN_REL) | CustomFieldValues],
	    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
        ?TICKET_PLANNED_FOR_REL ->
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    PLANNED_FOR_REL =
		#{<<"fieldName">> => <<"PlannedForRel">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textfield">>},
	    NewCustomFieldValues = [maps:put(<<"value">>, unicode:characters_to_binary(Text), PLANNED_FOR_REL) | CustomFieldValues],
	    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
	?TICKET_TEST_CASE ->
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    TEST_CASE =
		#{<<"fieldName">> => <<"Test case">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textfield">>},
	    NewCustomFieldValues = [maps:put(<<"value">>, unicode:characters_to_binary(Text), TEST_CASE) | CustomFieldValues],
	    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
	?TICKET_GIT_BRANCH ->
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    GIT_BRANCH =
		#{<<"fieldName">> => <<"Git Branch">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textfield">>},
	    NewCustomFieldValues = [maps:put(<<"value">>, unicode:characters_to_binary(Text), GIT_BRANCH) | CustomFieldValues],
	    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
	?TICKET_GIT_RANGE ->
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    GIT_RANGE =
		#{<<"fieldName">> => <<"Git Range">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textfield">>},
	    NewCustomFieldValues = [maps:put(<<"value">>, unicode:characters_to_binary(Text), GIT_RANGE) | CustomFieldValues],
	    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
        ?TICKET_REV ->
	    tokens_to_ticket(T, Ticket);
        ?TICKET_LAST_UPDATE ->
	    %% Example: "$Date: 2014/07/07 11:58:02 $ $Author: bmk $"
	    LAST_UPDATE = re:replace(string:substr(Text, 8, 19), " ", "T", [{return, list}]),
	    LAST_UPDATE1 = re:replace(LAST_UPDATE, "/", "-",[global, {return, list}]),
	    tokens_to_ticket(T, maps:put(<<"updated">>, unicode:characters_to_binary(LAST_UPDATE1), Ticket));
	?TICKET_NOTES ->
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    NOTES =
		#{<<"fieldName">> => <<"Notes">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textarea">>},
	    NewCustomFieldValues = [maps:put(<<"value">>, unicode:characters_to_binary(Text), NOTES) | CustomFieldValues],
	    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
	?TICKET_DESCR ->
	    tokens_to_ticket(T, maps:put(<<"description">>, unicode:characters_to_binary(Text), Ticket));
	?TICKET_PURPOSE ->
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    PURPOSE =
		#{<<"fieldName">> => <<"Purpose">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textarea">>},
	    NewCustomFieldValues = [maps:put(<<"value">>, unicode:characters_to_binary(Text), PURPOSE) | CustomFieldValues],
	    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
	BadLabel -> 
            Details = "Bad label: " ++ BadLabel,
	    New = 
		#{<<"body">> => unicode:characters_to_binary(Details),
		  <<"author">> => <<"bruce">>,
		  <<"created">> => <<"2012-08-31T17:59:02.161+0100">>
		 },
	    NewComments = [New | maps:get(<<"comments">>, Ticket, [])],
	    tokens_to_ticket(T, maps:put(<<"comments">>, NewComments, Ticket))
    end.
    
result() ->
    #{<<"projects">> => [
			 
    			 #{<<"name">> => <<"Erlang/OTP">>,
    			   <<"key">> => <<"ERL">>,
    			   <<"components">> => [
    						<<"erts">>,
    						<<"kernel">>
    					       ],
    			   <<"issues">> => [
					    
					    #{<<"key">> => <<"OTP-12345">>,
					      <<"summary">> => <<"Textfield">>,
					      <<"status">> => <<"solved">>,
					      <<"issueType">> => <<"bug">>,
					      <<"reporter">> => <<"lukas">>,
					      <<"created">> => <<"2014-07-11T14:59:06">>,
					      <<"priority">> => <<"1">>,
					      <<"description">> => <<"Textfield">>,
					      %% <<"resolution">> => <<"">>,
					      %% <<"updated">> => <<"">>,
					      %% <<"affectedVersions">> => [],
					      <<"assignee">> => <<"assignee">>,
					      %% <<"fixedVersions">> => [],
					      <<"components">> => [
								   <<"erts">>,
								   <<"kernel">>
								  ],
					      <<"customFieldValues">> => [
									  #{<<"fieldName">> => <<"RelatedID">>,
									    <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textfield">>,
									    <<"value">> => <<"Textfield">>
									   },
									  #{<<"fieldName">> => <<"Originator/s">>,
									    <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textfield">>,
									    <<"value">> => <<"Textfield">>
									   },	
									  #{<<"fieldName">> => <<"OTP release/s">>,
									    <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:multiversion">>,
									    <<"value">> => <<"R16B03">>
									   },
									  #{<<"fieldName">> => <<"Source">>,
									    <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:select">>,
									    <<"value">> => <<"external">>
									   },
									  #{<<"fieldName">> => <<"Highlight">>,
									    <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:select">>,
									    <<"value">> => <<"no">>
									   },
									  #{<<"fieldName">> => <<"Incompatible">>,
									    <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textfield">>,
									    <<"value">> => <<"yes">>
									   },
									  #{<<"fieldName">> => <<"Release Notes">>,
									    <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textarea">>,
									    <<"value">> => <<"Textarea">>
									   },
									  #{<<"fieldName">> => <<"Test case">>,
									    <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textfield">>,
									    <<"value">> => <<"Textfield">>
									   },
									  #{<<"fieldName">> => <<"Purpose">>,
									    <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textarea">>,
									    <<"value">> => <<"Textfield">>
									   },
									  #{<<"fieldName">> => <<"Notes">>,
									    <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textarea">>,
									    <<"value">> => <<"Textfield">>
									   },
									  #{<<"fieldName">> => <<"FixedInRel">>,
									    <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textfield">>,
									    <<"value">> => <<"Textfield">>
									   },
									  #{<<"fieldName">> => <<"PlannedForRel">>,
									    <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textfield">>,
									    <<"value">> => <<"Textfield">>
									   }
									 ]
					     }
					   ]
     			  }
     			]
     }.
