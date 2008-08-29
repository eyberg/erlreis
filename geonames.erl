-module(geonames).
-export([city_from_zip/1]).
-export([cities_by_bound/4]).

-include_lib("xmerl/include/xmerl.hrl").

-define(GEO_URL, 
      "http://ws.geonames.org/").

%% find city, state from zip code
%% takes: zip code
%% returns: city, state
city_from_zip(ZIP) ->
  PARAMS = "&postalcode=" ++ ZIP,
  URL = geonames_url("postalCodeSearch", PARAMS, "1"),
  { ok, {_Status, _Headers, Body }} = http:request(URL),
  { Xml, _Rest } = xmerl_scan:string(Body),
  [ #xmlText{value=City} ]  = xmerl_xpath:string("//name/text()", Xml),
  [ #xmlText{value=State} ] = xmerl_xpath:string("//adminCode1/text()", Xml),
  { State, City }.

%% return list of cities in bounding box of N,E,S,W
%% takes: n, s, e, w coordinates
%% returns: city
cities_by_bound(NORTH, SOUTH, EAST, WEST) ->
  PARAMS = "&north=" ++ NORTH ++ "&south=" ++ SOUTH ++ "&east=" ++ 
            EAST ++ "&west=" ++ WEST ++ "",
  URL = geonames_url("cities", PARAMS, "1"),
  { ok, {_Status, _Headers, Body }} = http:request(URL),
  { Xml, _Rest } = xmerl_scan:string(Body),
  [ #xmlText{value=City} ]  = xmerl_xpath:string("//name/text()", Xml),
  { City }.

%% build url for the search
geonames_url(SVC, PARAMS, COUNT) ->
  ?GEO_URL ++ SVC ++ "?maxRows=" ++ COUNT ++ PARAMS.
