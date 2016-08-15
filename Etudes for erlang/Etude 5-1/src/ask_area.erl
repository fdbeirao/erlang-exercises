-module(ask_area).

-export([area/0]).
%-compile(export_all).

-spec(area() -> { atom(), number() }).
area() ->
    { ok, [Shape|_] } = io:fread("R)ectangle, T)riangle or E)llipse > ", "~s"),
    case char_to_shape(Shape) of
        rectangle -> calculate(rectangle, get_dimensions("width", "height"));
        triangle  -> calculate(triangle, get_dimensions("base", "height"));
        ellipse   -> calculate(ellipse, get_dimensions("major axis", "minor axis"));
        unknown   -> { error, unknown_shape, Shape}
    end.

-spec(char_to_shape(char()) -> atom()).
char_to_shape(C) when C =:= "r"; C =:= "R" -> rectangle;
char_to_shape(C) when C =:= "t"; C =:= "T" -> triangle;
char_to_shape(C) when C =:= "e"; C =:= "E" -> ellipse;
char_to_shape(_) -> unknown.

-spec(get_number(string()) -> {atom(), number()}).
get_number(Prompt) ->
    InputPrompt = "Enter " ++ Prompt ++ " > ",
    {ok, [Input|_]} = io:fread(InputPrompt, "~s"),
    case tryparse(Input) of 
        {ok, ParsedInput} ->
            ParsedInput;
        _ ->
            get_number(Prompt)
    end.

-spec(tryparse(String) -> {atom(), number()}).
tryparse(Input) ->
    case string:to_float(Input) of
        {error, no_float} ->
            case string:to_integer(Input) of
                {error, _} ->
                    error;
                {Int, _} -> 
                    {ok, Int}
            end;
        {Float, _} ->
            {ok, Float}
    end.

-spec(get_dimensions(string(), string()) -> { number(), number() }).
get_dimensions(D1, D2) -> { get_number(D1), get_number(D2) }.

-spec(calculate(atom(), { number(), number() }) -> { atom(), number() }  
                                                |  { atom(), atom(), number() }).

calculate(_, {D1, _}) when not is_number(D1) -> { error, not_number, D1 };
calculate(_, {D1, _}) when D1 < 0 -> { error, negative_not_allowed, D1 };
calculate(_, {_, D2}) when not is_number(D2) -> { error, not_number, D2 };
calculate(_, {_, D2}) when D2 < 0 -> { error, negative_not_allowed, D2 };
calculate(Shape, { D1, D2 }) -> { ok, geom:area(Shape, D1, D2) }.
