-module(geonames).
-export([city_from_zip/1]).
-export([cities_by_bound/4]).
-export([neighboring_countries_for/1]).
-export([countrycode_for/2]).

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

%% grab a list for all the surrounding neighbors of a country
%% takes: a geonameid
%% returns: list of countries surrounding that country
%% FUTURE: make it accessible by name of the country
%% NOTE:: I locked the xpath quuery down to the first element
%% because this doesn't have a geonamedid count limit
neighboring_countries_for(COUNTRY) ->
  PARAMS = "&geonameId=" ++ COUNTRY,
  URL = geonames_url("neighbours", PARAMS, "1"),
  { ok, {_Status, _Headers, Body }} = http:request(URL),
  { Xml, _Rest } = xmerl_scan:string(Body),
  [ #xmlText{value=Countries} ] = xmerl_xpath:string("//name/text()[1]", Xml),
  { Countries }.

%% grab the country code for any longitude, latitutde combo
%% takes: latitude, longitude
%% returns: country code
countrycode_for(LONGITUDE, LATITUDE) ->
  PARAMS = "&lat=" ++ LATITUDE ++ "&lng=" ++ LONGITUDE,
  URL = geonames_url("countryCode", PARAMS, "1"),
  { ok, {_Status, _Headers, Body }} = http:request(URL),
  {_, CountryCode, _} = regexp:sub(Body, "\r\n", ""),
  { CountryCode }.
  
%% build url for the search
geonames_url(SVC, PARAMS, COUNT) ->
  ?GEO_URL ++ SVC ++ "?maxRows=" ++ COUNT ++ PARAMS.
