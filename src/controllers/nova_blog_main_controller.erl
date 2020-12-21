-module(nova_blog_main_controller).
-export([
         index/1,
         not_found/1,
         internal_error/1
        ]).

index(#{req := #{method := <<"GET">>}}) ->
    {ok, Examples} = nova_blog_db:get_examples(),
    {ok, [{examples, Examples}]}.


not_found(_Req) ->
    {ok, [{error, "We could not find the page"}]}.

internal_error(_Req) ->
    {ok, [{error, "Woops. Internal server error"}]}.
