-module(nova_blog_db).

-export([get_examples/0]).


get_examples() ->
    SQL = <<"SELECT * FROM nova_blog_example">>,
    query(SQL, []).

query1(SQL, Values) ->
    case pgo:query(SQL, Values) of
        #{command := insert,
          num_rows := 1} -> ok;
        #{command := select,
          rows := []} -> undefined;
        #{command := select,
          rows := [Row]} -> {ok, Row};
        #{command := update,
          num_rows := Num} -> {ok, Num};
        #{command := delete,
          num_rows := 1} -> ok;
        #{command := delete} -> undefined;
        {error, Error} ->
            logger:error("Error: ~p on SQL ~p Values ~p", [Error, SQL, Values]),
            {error, Error}
    end.

query(SQL, Values) ->
    case pgo:query(SQL, Values) of
        #{command := insert,
          num_rows := Num} -> {ok, Num};
        #{command := select,
          rows := Rows} -> {ok, Rows};
        #{command := update,
          num_rows := Num} -> {ok, Num};
        #{command := delete,
          num_rows := Num} -> {ok, Num};
        {error, Error} ->
            logger:error("Error: ~p on SQL ~p Values ~p", [Error, SQL, Values]),
            {error, Error}
    end.
