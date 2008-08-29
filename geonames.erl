-module(geonames).
-export([city_from_zip/1]).
-include_lib("xmerl/include/xmerl.hrl").

-define(BASE_URL, 
      "http://ws.geonames.org/" ++
      "postalCodeSearch?" ++
      "maxRows=1&postalcode=").

city_from_zip(ISBN) ->
  URL = amazon_url_for(ISBN),
  { ok, {_Status, _Headers, Body }} = http:request(URL),
  { Xml, _Rest } = xmerl_scan:string(Body),
  [ #xmlText{value=City} ]  = xmerl_xpath:string("//name/text()", Xml),
  [ #xmlText{value=State} ] = xmerl_xpath:string("//adminCode1/text()", Xml),
  { State, City }.

amazon_url_for(ZIP) ->
  ?BASE_URL ++ ZIP.
