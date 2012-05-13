-module(csv).
%% http://stackoverflow.com/questions/1532081/csv-parser-in-erlang

-export([parse/1, parse_line/1]).

parse(File) ->
  {ok, F} = file:open(File, [read, raw]),
  parse(F, file:read_line(F), []).

parse(F, eof, Done) ->
  file:close(F),
  lists:reverse(Done);    

parse(F, Line, Done) ->
  parse(F, file:read_line(F), [parse_line(Line)|Done]).



parse_line(Line) -> parse_line(Line, []).

parse_line([], Fields) -> lists:reverse(Fields);
parse_line("," ++ Line, Fields) -> parse_field(Line, Fields);
parse_line(Line, Fields) -> parse_field(Line, Fields).

parse_field("\"" ++ Line, Fields) -> parse_field_q(Line, Fields);
parse_field(Line, Fields) -> parse_field(Line, [], Fields).

parse_field("," ++ _ = Line, Buf, Fields) -> parse_line(Line, [lists:reverse(Buf)|Fields]);
parse_field([C|Line], Buf, Fields) -> parse_field(Line, [C|Buf], Fields);
parse_field([], Buf, Fields) -> parse_line([], [lists:reverse(Buf)|Fields]).

parse_field_q(Line, Fields) -> parse_field_q(Line, [], Fields).
parse_field_q("\"\"" ++ Line, Buf, Fields) -> parse_field_q(Line, [$"|Buf], Fields);
parse_field_q("\"" ++ Line, Buf, Fields) -> parse_line(Line, [lists:reverse(Buf)|Fields]);
parse_field_q([C|Line], Buf, Fields) -> parse_field_q(Line, [C|Buf], Fields).
