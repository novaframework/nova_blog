-module(nova_blog_article_controller).
-export([index/1]).

index(#{req := Req}) ->
    CurrentEntry =
        case cowboy_req:binding(id, Req, undefined) of
            undefined ->
                {ok, [Entry|_]} = nova_blog_db:get_entries(1),
                Entry;
            Id ->
                {ok, [Entry|_]} = nova_blog_db:get_entry_by_id(Id),
                Entry
        end,
    {ok, AllEntries} = nova_blog_db:get_entries(),

    {ok, [{current_article, CurrentEntry}, {articles, AllEntries}]}.
