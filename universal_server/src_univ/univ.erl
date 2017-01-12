-module(univ).

-export([universal_server/0]).

universal_server() ->
  receive
    {become, F} ->
      F()
  end.

