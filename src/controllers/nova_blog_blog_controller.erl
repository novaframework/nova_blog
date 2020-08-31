-module(nova_blog_blog_controller).
-export([
         index/1
        ]).

index(#{req := #{method := <<"GET">>}}) ->
    Result = nova_blog_db:get_entries(),
    {ok, [{entries, Result}]}.
