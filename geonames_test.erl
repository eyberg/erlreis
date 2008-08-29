-module(geonames_test).
-include_lib("eunit/include/eunit.hrl").
 
basic_test() ->
    ?assert(1 + 1 == 2).

geoname_compile_test() ->
  inets:start(),
  {234} = geonames:geonameid_for("us").

%% well duh..
fail_test() ->
  [1,2] = lists:reverse([1,2]).
