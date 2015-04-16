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
-export([start/0, result/0]).

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

start() ->
    Project =
	#{<<"name">> => <<"Erlang/OTP">>,
	  <<"key">> => <<"ERL">>,
	  <<"components">> => [
			       <<"erts">>,
			       <<"kernel">>
			      ],
	  <<"issues">> => []
	 },

    Example_tokens = 
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
    
    EmptyTicket = maps:new(),
    {ok, Issue} = tokens_to_ticket(Example_tokens, EmptyTicket),
    
    Project1 = maps:put(
		 <<"issues">>, 
		 [Issue | maps:get(<<"issues">>, Project, [])],
		 Project
		),

    Projects = 
	#{<<"projects">> => [Project1]},
    Projects.


tokens_to_ticket([], Ticket) ->
    {ok, Ticket};
tokens_to_ticket([{Label, Text} | T], Ticket) ->
    case Label of
        ?TICKET_ID -> 
	    tokens_to_ticket(T, maps:put(<<"key">>, list_to_binary(Text), Ticket));
        ?TICKET_LABEL -> 
	    tokens_to_ticket(T, maps:put(<<"summary">>, list_to_binary(Text), Ticket));
        ?TICKET_STATUS ->
            case string:to_lower(Text) of
                "new" ->
                    tokens_to_ticket(T, maps:put(<<"status">>, list_to_binary(Text), Ticket));
                "open" ->
                    tokens_to_ticket(T, maps:put(<<"status">>, list_to_binary(Text), Ticket));
		"reviewed" ->
                    tokens_to_ticket(T, maps:put(<<"status">>, list_to_binary(Text), Ticket));
		"promoted" ->
                    tokens_to_ticket(T, maps:put(<<"status">>, list_to_binary(Text), Ticket));
                "deferred" ->
                    tokens_to_ticket(T, maps:put(<<"status">>, list_to_binary(Text), Ticket));
                "solved" ->
                    tokens_to_ticket(T, maps:put(<<"status">>, list_to_binary(Text), Ticket));
                "closed" ->
                    tokens_to_ticket(T, maps:put(<<"status">>, list_to_binary(Text), Ticket));
                "cancelled" ->
                    tokens_to_ticket(T, maps:put(<<"status">>, list_to_binary(Text), Ticket));
		BadValue ->
                    NewValue = "new",
		    Details = lists:concat(["Bad Status: " ,
                                            "\"\"",
                                            BadValue,
                                            " -> ", NewValue, "\n"]),
		    New = 
			#{<<"body">> => list_to_binary(Details),
			  <<"author">> => <<"bruce">>,
			  <<"created">> => <<"2012-08-31T17:59:02.161+0100">>
			 },
		    NewComments = [New | maps:get(<<"comments">>, Ticket, [])],
		    Ticket1 = maps:put(<<"comments">>, NewComments, Ticket),
		    tokens_to_ticket(T, maps:put(<<"status">>, list_to_binary(Text), Ticket1))
            end;
        ?TICKET_TYPE ->
            case string:to_lower(Text) of
                "bug" ->
                    tokens_to_ticket(T, maps:put(<<"issueType">>, list_to_binary(Text), Ticket));
                "job" ->
                    tokens_to_ticket(T, maps:put(<<"issueType">>, list_to_binary(Text), Ticket));
                "request" ->
                    tokens_to_ticket(T, maps:put(<<"issueType">>, list_to_binary(Text), Ticket));
		BadValue ->
                    NewValue = "bug",
		    Details = lists:concat(["Bad Type: " ,
                                            "\"\"",
                                            BadValue,
					    " -> ", NewValue, "\n"]),
		    New = 
			#{<<"body">> => list_to_binary(Details),
			  <<"author">> => <<"bruce">>,
			  <<"created">> => <<"2012-08-31T17:59:02.161+0100">>
			 },
		    NewComments = [New | maps:get(<<"comments">>, Ticket, [])],
		    Ticket1 = maps:put(<<"comments">>, NewComments, Ticket),
		    tokens_to_ticket(T, maps:put(<<"issueType">>, list_to_binary(Text), Ticket1))
		    
            end;
        ?TICKET_SOURCE ->
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    Source = 
		#{<<"fieldName">> => <<"Source">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:select">>
		 },
            case string:to_lower(Text) of
                "internal" ->
		    NewCustomFieldValues = [maps:put(<<"value">>, list_to_binary(Text), Source) | CustomFieldValues],
		    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
                "external" ->
                    NewCustomFieldValues = [maps:put(<<"value">>, list_to_binary(Text), Source) | CustomFieldValues],
		    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
                "opensource" ->
		    NewCustomFieldValues = [maps:put(<<"value">>, list_to_binary(Text), Source) | CustomFieldValues],
		    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
		BadValue ->
                    NewValue = "internal",
		    Details = lists:concat(["Bad Source: " ,
                                            "\"\"",
                                            BadValue,
					    " -> ", NewValue, "\n"]),
		    New = 
			#{<<"body">> => list_to_binary(Details),
			  <<"author">> => <<"bruce">>,
			  <<"created">> => <<"2012-08-31T17:59:02.161+0100">>
			 },
		    NewComments = [New | maps:get(<<"comments">>, Ticket, [])],
		    Ticket1 = maps:put(<<"comments">>, NewComments, Ticket),
		    NewCustomFieldValues = [maps:put(<<"value">>, list_to_binary(Text), Source) | CustomFieldValues],
		    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket1))
            end;
        ?TICKET_PRIORITY ->
            case Text of
                "1" ->
                    tokens_to_ticket(T, maps:put(<<"priority">>, list_to_binary(Text), Ticket));
                "2" ->
                    tokens_to_ticket(T, maps:put(<<"priority">>, list_to_binary(Text), Ticket));
                "3" ->
                    tokens_to_ticket(T, maps:put(<<"priority">>, list_to_binary(Text), Ticket));
                "4" ->
                    tokens_to_ticket(T, maps:put(<<"priority">>, list_to_binary(Text), Ticket));
		BadValue ->
                    NewValue = ?DEFAULT_PRIORITY,
		    Details = lists:concat(["Bad Priority: " ,
                                            "\"\"",
                                            BadValue,
					    " -> ", NewValue, "\n"]),
		    New = 
			#{<<"body">> => list_to_binary(Details),
			  <<"author">> => <<"bruce">>,
			  <<"created">> => <<"2012-08-31T17:59:02.161+0100">>
			 },
		    NewComments = [New | maps:get(<<"comments">>, Ticket, [])],
		    Ticket1 = maps:put(<<"comments">>, NewComments, Ticket),
		    tokens_to_ticket(T, maps:put(<<"priority">>, list_to_binary(Text), Ticket1))
            end;
        ?TICKET_RELATED_ID ->
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    RelatedID =
		#{<<"fieldName">> => <<"RelatedID">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textfield">>},
	    NewCustomFieldValues = [maps:put(<<"value">>, list_to_binary(Text), RelatedID) | CustomFieldValues],
	    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
        ?TICKET_CREATOR -> 
	    tokens_to_ticket(T, maps:put(<<"reporter">>, list_to_binary(Text), Ticket));
        ?TICKET_CREATION_DATE ->
	    tokens_to_ticket(T, maps:put(<<"created">>, list_to_binary(re:replace(Text, " ", "T", [{return, list}])), Ticket));
        ?TICKET_ORIGINATORS ->
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    Originators =
		#{<<"fieldName">> => <<"Originator/s">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textfield">>},
	    NewCustomFieldValues = [maps:put(<<"value">>, list_to_binary(Text), Originators) | CustomFieldValues],
	    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
        ?TICKET_OTP_RELS -> 
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    OTP_RELS =
		#{<<"fieldName">> => <<"OTP release/s">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:multiversion">>},
	    NewCustomFieldValues = [maps:put(<<"value">>, list_to_binary(Text), OTP_RELS) | CustomFieldValues],
	    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
        ?TICKET_APPLICATION -> 
	    Components = lists:map(fun(A)->list_to_binary(A) end, string:tokens(Text, " ")),
	    tokens_to_ticket(T, maps:put(<<"components">>, Components, Ticket));
        ?TICKET_HIGHLIGHT ->
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    Highlight = 
		#{<<"fieldName">> => <<"Highlight">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:select">>
		 },
	    case string:to_lower(Text) of
                "yes" ->
		    NewCustomFieldValues = [maps:put(<<"value">>, list_to_binary(Text), Highlight) | CustomFieldValues],
		    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
                "no" ->
                    NewCustomFieldValues = [maps:put(<<"value">>, list_to_binary(Text), Highlight) | CustomFieldValues],
		    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
                BadValue ->
                    NewValue = "no",
                    Details = lists:concat(["Changed Highlight to default: " ,
                                            "\"\"",
                                            BadValue,
                                            " -> ", NewValue, "\n"]),
		    New = 
			#{<<"body">> => list_to_binary(Details),
			  <<"author">> => <<"bruce">>,
			  <<"created">> => <<"2012-08-31T17:59:02.161+0100">>
			 },
		    NewComments = [New | maps:get(<<"comments">>, Ticket, [])],
		    Ticket1 = maps:put(<<"comments">>, NewComments, Ticket),
		    NewCustomFieldValues = [maps:put(<<"value">>, list_to_binary(Text), Highlight) | CustomFieldValues],
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
		    NewCustomFieldValues = [maps:put(<<"value">>, list_to_binary(Text), Incompatible) | CustomFieldValues],
		    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
                "" ->
                    NewValue = "no",
                    Details = lists:concat(["Changed Incompatible to default: " ,
                                            "\"\"",
                                            " -> ", NewValue, "\n"]),
		    New = 
			#{<<"body">> => list_to_binary(Details),
			  <<"author">> => <<"bruce">>,
			  <<"created">> => <<"2012-08-31T17:59:02.161+0100">>
			 },
		    NewComments = [New | maps:get(<<"comments">>, Ticket, [])],
		    Ticket1 = maps:put(<<"comments">>, NewComments, Ticket),
		    NewCustomFieldValues = [maps:put(<<"value">>, list_to_binary(Text), Incompatible) | CustomFieldValues],
		    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket1)); 
                _ ->
		    NewCustomFieldValues = [maps:put(<<"value">>, list_to_binary(Text), Incompatible) | CustomFieldValues],
		    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket))
            end;
        ?TICKET_RELEASE_NOTE ->
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    RELEASE_NOTE =
		#{<<"fieldName">> => <<"Release Notes">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textarea">>},
	    NewCustomFieldValues = [maps:put(<<"value">>, list_to_binary(Text), RELEASE_NOTE) | CustomFieldValues],
	    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
        ?TICKET_ASSIGNED_TO ->
	    tokens_to_ticket(T, maps:put(<<"assignee">>, list_to_binary(Text), Ticket));
        ?TICKET_FIXED_IN_REL ->	   
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    FIXED_IN_REL =
		#{<<"fieldName">> => <<"FixedInRel">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textfield">>},
	    NewCustomFieldValues = [maps:put(<<"value">>, list_to_binary(Text), FIXED_IN_REL) | CustomFieldValues],
	    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
        ?TICKET_PLANNED_FOR_REL ->
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    PLANNED_FOR_REL =
		#{<<"fieldName">> => <<"PlannedForRel">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textfield">>},
	    NewCustomFieldValues = [maps:put(<<"value">>, list_to_binary(Text), PLANNED_FOR_REL) | CustomFieldValues],
	    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
	?TICKET_TEST_CASE ->
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    TEST_CASE =
		#{<<"fieldName">> => <<"Test case">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textfield">>},
	    NewCustomFieldValues = [maps:put(<<"value">>, list_to_binary(Text), TEST_CASE) | CustomFieldValues],
	    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
	?TICKET_NOTES ->
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    NOTES =
		#{<<"fieldName">> => <<"Notes">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textarea">>},
	    NewCustomFieldValues = [maps:put(<<"value">>, list_to_binary(Text), NOTES) | CustomFieldValues],
	    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
	?TICKET_DESCR ->
	    tokens_to_ticket(T, maps:put(<<"description">>, list_to_binary(Text), Ticket));
	?TICKET_PURPOSE ->
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    PURPOSE =
		#{<<"fieldName">> => <<"Purpose">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textarea">>},
	    NewCustomFieldValues = [maps:put(<<"value">>, list_to_binary(Text), PURPOSE) | CustomFieldValues],
	    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket))
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
