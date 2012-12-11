%% ---
%% Parse csv formated data (RFC-4180)
%% ---

-module(csv_luke).
-import(lists, [reverse/1]).
-export([parse_file/3,parse_file/1,parse/3,parse/1]).

-record(ecsv,{
   state = field_start,  %%field_start|normal|quoted|post_quoted
   cols = undefined, %%how many fields per record
   current_field = [],
   current_record = [],
   fold_state,
   fold_fun  %%user supplied fold function
   }).

%% --------- Exported ------------------------------
parse_file(FileName,InitialState,Fun) ->
   {ok, Binary} = file:read_file(FileName),
    parse(Binary,InitialState,Fun).

parse_file(FileName)  ->
   {ok, Binary} = file:read_file(FileName),
    parse(Binary).

parse(Binary) ->
   R = parse(Binary,[],fun(Fold,Record) -> [Record|Fold] end),
   lists:reverse(R).

parse(Binary,InitialState,Fun) ->
   do_parse(Binary,#ecsv{fold_state=InitialState,fold_fun=Fun}).

%% --------- Field_start state ---------------------
%%whitespace, loop in field_start state
do_parse(<<32,Rest/binary>>,S = #ecsv{state=field_start,current_field=Field})->
	do_parse(Rest,S#ecsv{current_field=[32|Field]});

%%its a quoted field, discard previous whitespaces
do_parse(<<$",Rest/binary>>,S = #ecsv{state=field_start})->
	do_parse(Rest,S#ecsv{state=quoted,current_field=[]});

%%anything else, is a unquoted field
do_parse(Bin,S = #ecsv{state=field_start})->
	do_parse(Bin,S#ecsv{state=normal});

%% --------- Quoted state ---------------------
%%Escaped quote inside a quoted field
do_parse(<<$",$",Rest/binary>>,S = #ecsv{state=quoted,current_field=Field})->
	do_parse(Rest,S#ecsv{current_field=[$"|Field]});

%%End of quoted field
do_parse(<<$",Rest/binary>>,S = #ecsv{state=quoted})->
	do_parse(Rest,S#ecsv{state=post_quoted});

%%Anything else inside a quoted field
do_parse(<<X,Rest/binary>>,S = #ecsv{state=quoted,current_field=Field})->
	do_parse(Rest,S#ecsv{current_field=[X|Field]});

do_parse(<<>>, #ecsv{state=quoted})->
	throw({ecsv_exception,unclosed_quote});

%% --------- Post_quoted state ---------------------
%%consume whitespaces after a quoted field
do_parse(<<32,Rest/binary>>,S = #ecsv{state=post_quoted})->
	do_parse(Rest,S);

%%---------Comma and New line handling. ------------------
%%---------Common code for post_quoted and normal state---

%%EOF in a new line, return the records
do_parse(<<>>, #ecsv{current_record=[],fold_state=State})->
	State;
%%EOF in the last line, add the last record and continue
do_parse(<<>>,S)->
	do_parse(<<>>,new_record(S));

%% new record windows
do_parse(<<$\r,$\n,Rest/binary>>,S = #ecsv{})->
	do_parse(Rest,new_record(S));

%% new record pre Mac OSX 10
do_parse(<<$\r,Rest/binary>>,S = #ecsv{}) ->
	do_parse(Rest,new_record(S));

%% new record Unix
do_parse(<<$\n,Rest/binary>>,S = #ecsv{}) ->
	do_parse(Rest,new_record(S));

do_parse(<<$, ,Rest/binary>>,S = #ecsv{current_field=Field,current_record=Record})->
	do_parse(Rest,S#ecsv{state=field_start,
					  current_field=[],
					  current_record=[lists:reverse(Field)|Record]});

%%A double quote in any other place than the already managed is an error
do_parse(<<$",_Rest/binary>>, #ecsv{})->
	throw({ecsv_exception,bad_record});

%%Anything other than whitespace or line ends in post_quoted state is an error
do_parse(<<_X,_Rest/binary>>, #ecsv{state=post_quoted})->
 	throw({ecsv_exception,bad_record});

%%Accumulate Field value
do_parse(<<X,Rest/binary>>,S = #ecsv{state=normal,current_field=Field})->
	do_parse(Rest,S#ecsv{current_field=[X|Field]}).

%%check	the record size against the previous, and actualize state.
new_record(S=#ecsv{cols=Cols,current_field=Field,current_record=Record,fold_state=State,fold_fun=Fun}) ->
	NewRecord = list_to_tuple(lists:reverse([lists:reverse(Field)|Record])),
	if
		(tuple_size(NewRecord) =:= Cols) or (Cols =:= undefined) ->
			NewState = Fun(State,NewRecord),
			S#ecsv{state=field_start,cols=tuple_size(NewRecord),
					current_record=[],current_field=[],fold_state=NewState};

		(tuple_size(NewRecord) =/= Cols) ->
			throw({ecsv_exception,bad_record_size})
	end.

%% -------- Regression tests ------------------------
%% From the erl interpreter run csv:test() to run regression tests.
%% See eunit for more information.
-define(TEST,true).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

csv_test_() ->
	[% empty binary
	 ?_assertEqual([], parse(<<>>)),
	 % Unix LF
	 ?_assertEqual([{"1A","1B","1C"},{"2A","2B","2C"}], parse(<<"1A,1B,1C\n2A,2B,2C">>)),
	 % Unix LF with extra spaces after quoted element stripped
	 ?_assertEqual([{"1A","1B","1C"},{"2A","2B","2C"}], parse(<<"\"1A\" ,1B,1C\n2A,2B,2C">>)),
	 % Unix LF with extra spaces preserved in unquoted element
	 ?_assertEqual([{" 1A ","1B","1C"},{"2A","2B","2C"}], parse(<<" 1A ,1B,1C\n2A,2B,2C">>)),
	 % Pre Mac OSX 10 CR
	 ?_assertEqual([{"1A","1B","1C"},{"2A","2B","2C"}], parse(<<"1A,1B,1C\r2A,2B,2C">>)),
	 % Windows CRLF
	 ?_assertEqual([{"1A","1B","1C"},{"2A","2B","2C"}], parse(<<"1A,1B,1C\r\n2A,2B,2C">>)),
	 % Quoted element
	 ?_assertEqual([{"1A","1B"}], parse(<<"1A,\"1B\"">>)),
	 % Nested quoted element
	 ?_assertEqual([{"1A","\"1B\""}], parse(<<"1A,\"\"\"1B\"\"\"">>)),
	 % Quoted element with embedded LF
	 ?_assertEqual([{"1A","1\nB"}], parse(<<"1A,\"1\nB\"">>)),
	 % Missing 2nd quote
	 ?_assertThrow({ecsv_exception,unclosed_quote}, parse(<<"1A,\"1B\n">>)),
	 % Bad record size
	 ?_assertThrow({ecsv_exception,bad_record_size}, parse(<<"1A,1B\n2A,2B,2C">>))
	].

-endif.
