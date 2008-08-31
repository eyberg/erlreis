-module(geonames_test).
-export([main/0]).

-load_file(geonames).

-include_lib("eunit/include/eunit.hrl").

main() ->
  compile:file(geonames),
  inets:start(),
  { StartMegaSeconds, StartSeconds, StartMicroseconds } = erlang:now(),
  test(),
  { EndMegaSeconds, EndSeconds, EndMicroseconds } = erlang:now(),
  { TestMegaSeconds, TestSeconds, TestMicroseconds } =
    { EndMegaSeconds - StartMegaSeconds,
      EndSeconds - StartSeconds,
      EndMicroseconds - StartMicroseconds },
  io:format("\033[1m\033[32m\033[40m passing test!\033[0m~n"),
  %%io:format("\033[1m\033[31m\033[40m failing test\033[0m~n"),
  io:format(" Test run time: ~B.~6..0B seconds.~n",
    [TestMegaSeconds*1000 + TestSeconds, TestMicroseconds]).

%% i don't know about testing the current time yet..
%% maybe a GMT modifier to this
timezone_for_test() ->
  {_, "Africa/Tripoli"} = geonames:timezone_for("23", "23").

city_from_zip_test() ->
  {"HE", "Wiesbaden" } = geonames:city_from_zip("65201").

countrycode_for_test() ->
  {"LY"} = geonames:countrycode_for("25", "20").

geonameid_for_test() ->
  {"6252001"} = geonames:geonameid_for("us").
