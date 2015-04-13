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


start() ->
    Tokens = 
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
	 {"Release note","Textfield"},
	 {"AssignedTo","assignee"},
	 {"FixedInRel","Textfield"},
	 {"PlannedForRel","Textfield"},
	 {"Test case","Textfield"},
	 {"Description","Textfield"},
	 {"Purpose","Textfield"},
	 {"Notes","Textfield"}],
    
    EmptyTicket = maps:new(),
    tokens_to_ticket(Tokens, EmptyTicket).


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
                    tokens_to_ticket(T, maps:put(<<"status">>, list_to_binary(Text), Ticket))
            end;
        ?TICKET_TYPE ->
            case string:to_lower(Text) of
                "bug" ->
                    tokens_to_ticket(T, maps:put(<<"issueType">>, list_to_binary(Text), Ticket));
                "job" ->
                    tokens_to_ticket(T, maps:put(<<"issueType">>, list_to_binary(Text), Ticket));
                "request" ->
                    tokens_to_ticket(T, maps:put(<<"issueType">>, list_to_binary(Text), Ticket))
            end;
        ?TICKET_SOURCE ->
	    CustomFieldValues = maps:get(<<"customFieldValues">>, Ticket, []),
	    Source = 
		#{<<"fieldName">> => <<"Source">>,
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:select">>},
            case string:to_lower(Text) of
                "internal" ->
		    NewCustomFieldValues = [maps:put(<<"value">>, list_to_binary(Text), Source) | CustomFieldValues],
		    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
                "external" ->
                    NewCustomFieldValues = [maps:put(<<"value">>, list_to_binary(Text), Source) | CustomFieldValues],
		    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
                "opensource" ->
		    NewCustomFieldValues = [maps:put(<<"value">>, list_to_binary(Text), Source) | CustomFieldValues],
		    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket))
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
                    tokens_to_ticket(T, maps:put(<<"priority">>, list_to_binary(Text), Ticket))
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
	    tokens_to_ticket(T, maps:put(<<"created">>, list_to_binary(Text), Ticket));
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
		  <<"fieldType">> => <<"com.atlassian.jira.plugin.system.customfieldtypes:textfield">>},
	    NewCustomFieldValues = [maps:put(<<"value">>, list_to_binary(Text), OTP_RELS) | CustomFieldValues],
	    tokens_to_ticket(T, maps:put(<<"customFieldValues">>, NewCustomFieldValues, Ticket));
        ?TICKET_APPLICATION -> 
	    tokens_to_ticket(T, Ticket); %%TODO
        ?TICKET_HIGHLIGHT ->
	    tokens_to_ticket(T, Ticket); %%TODO
        ?TICKET_INCOMPATIBLE ->
	    tokens_to_ticket(T, Ticket); %%TODO
        ?TICKET_RELEASE_NOTE ->
	    tokens_to_ticket(T, Ticket); %%TODO
        ?TICKET_ASSIGNED_TO ->
	    tokens_to_ticket(T, Ticket); %%TODO
        ?TICKET_FIXED_IN_REL ->	   
	    tokens_to_ticket(T, Ticket); %%TODO
        ?TICKET_PLANNED_FOR_REL ->
       	    tokens_to_ticket(T, Ticket); %%TODO
	?TICKET_TEST_CASE ->
	    tokens_to_ticket(T, Ticket); %%TODO
	?TICKET_GIT_BRANCH ->
	    tokens_to_ticket(T, Ticket); %%TODO
	?TICKET_GIT_RANGE ->
       	    tokens_to_ticket(T, Ticket); %%TODO
	?TICKET_REV ->
      	    tokens_to_ticket(T, Ticket); %%TODO
	?TICKET_LAST_UPDATE ->
      	    tokens_to_ticket(T, Ticket); %%TODO
	?TICKET_NOTES ->
       	    tokens_to_ticket(T, Ticket); %%TODO
	?TICKET_DESCR ->
       	    tokens_to_ticket(T, Ticket); %%TODO
	?TICKET_PURPOSE ->
	    tokens_to_ticket(T, Ticket) %%TODO
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
					      <<"created">> => <<"2014-07-11">>,
					      <<"priority">> => <<"1">>,
					      <<"description">> => <<"">>,
					      <<"resolution">> => <<"">>,
					      <<"updated">> => <<"">>,
					      <<"affectedVersions">> => [],
					      <<"assignee">> => <<"assignee">>,
					      <<"fixedVersions">> => [],
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
									    <<"value">> => <<"Textfield">>
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
