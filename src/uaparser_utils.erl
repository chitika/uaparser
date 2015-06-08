-module(uaparser_utils).
-compile({parse_transform, ct_expand}).
-include("../include/uaparser.hrl").
-export([bin_to_num/1]).

-spec bin_to_num(Bin :: binary()) -> number().
bin_to_num(Binary) when is_binary(Binary) ->
    List = re:replace(Binary, "\\.", "", [{return, list}]),
    case string:to_float(List) of
        {error, no_float} -> list_to_integer(List);
        {F,_Rest} -> F
    end.
