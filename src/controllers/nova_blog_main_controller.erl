-module(nova_blog_main_controller).
-export([
         index/1
        ]).

index(#{req := #{method := <<"GET">>}}) ->
    {ok, Examples} = nova_blog_db:get_examples(),
    {ok, [{examples, Examples}]}.