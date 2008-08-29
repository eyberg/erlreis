-module(geonames).
-export([city_from_zip/1]).
-export([cities_by_bound/4]).
-export([neighboring_countries_for/1]).
-export([countrycode_for/2]).
-export([timezone_for/2]).
-export([geonameid_for/1]).

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
 
%% obtains the timezone && timezoneid
%% takes: latitude, logintude
%% returns: time, timzoneid
timezone_for(LONGITUDE, LATITUDE) ->
  PARAMS = "&lat=" ++ LATITUDE ++ "&lng=" ++ LONGITUDE,
  URL = geonames_url("timezone", PARAMS, "1"),
  { ok, {_Status, _Headers, Body }} = http:request(URL),
  { Xml, _Rest } = xmerl_scan:string(Body),
  [ #xmlText{value=Time} ] = xmerl_xpath:string("//time/text()", Xml),
  [ #xmlText{value=TimeZoneId} ] = xmerl_xpath:string("//timezoneId/text()", Xml),
  { Time, TimeZoneId }.

%%grab the geonamesId for a particular country
%% takes: the country initals
%% returns: the country geonameId
geonameid_for(COUNTRY) ->
  PARAMS = "&country=" ++ COUNTRY,
  URL = geonames_url("countryInfo", PARAMS, "1"),
  { ok, {_Status, _Headers, Body }} = http:request(URL),
  { Xml, _Rest } = xmerl_scan:string(Body),
  [ #xmlText{value=GeoNameId} ] = xmerl_xpath:string("//geonameId/text()", Xml),
  { GeoNameId }.
  
%% build url for the search
geonames_url(SVC, PARAMS, COUNT) ->
  ?GEO_URL ++ SVC ++ "?maxRows=" ++ COUNT ++ PARAMS.
