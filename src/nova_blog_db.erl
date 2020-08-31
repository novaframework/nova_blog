%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbas.se>
%%% @copyright (C) 2019, Niclas Axelsson
%%% @doc
%%%
%%% @end
%%% Created :  8 Aug 2019 by Niclas Axelsson <niclas@burbas.se>
%%%-------------------------------------------------------------------
-module(nova_blog_db).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         new_entry/3,
         new_author/3,
         get_author/1,
         get_authors/0,
         get_entry_by_id/1,
         get_entries/0,
         get_entries/1,
         get_latest_release/0,
         get_releases/0,
         get_release/1,
         new_release/3
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         format_status/2]).

-include_lib("nova/include/nova.hrl").
-include_lib("nova_blog/include/nova_blog.hrl").
-include_lib("nova_blog/include/nova_blog_migration.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(SERVER, ?MODULE).
-define(RtoM(Name, Record), lists:foldl(fun({I, E}, Acc) -> Acc#{E => element(I, Record)} end, #{}, lists:zip(lists:seq(2, (record_info(size, Name))), (record_info(fields, Name))))).

-record(state, {
                connection :: epgsql:connection()
               }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new_entry(Title, Text, AuthorId) ->
    gen_server:cast(?SERVER, {new_entry, Title, Text, AuthorId}).

new_author(Name, About, Email) ->
    gen_server:cast(?SERVER, {new_author, Name, About, Email}).

get_author(Email) ->
    gen_server:call(?SERVER, {get_author, Email}).

get_authors() ->
    gen_server:call(?SERVER, get_authors).

get_entries() ->
    get_entries(20).

get_entry_by_id(Id) when is_integer(Id) ->
    gen_server:call(?SERVER, {get_entry_by_id, Id});
get_entry_by_id(Id) ->
    get_entry_by_id(erlang:binary_to_integer(Id)).

get_entries(Amount) ->
    gen_server:call(?SERVER, {get_entries, Amount}).

get_latest_release() ->
    gen_server:call(?SERVER, get_latest_release).

get_releases() ->
    gen_server:call(?SERVER, get_releases).

get_release(Version) ->
    gen_server:call(?SERVER, {get_release, Version}).

new_release(Version, Changes, AuthorId) ->
    gen_server:cast(?SERVER, {new_release, Version, Changes, AuthorId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
                              {ok, State :: term(), Timeout :: timeout()} |
                              {ok, State :: term(), hibernate} |
                              {stop, Reason :: term()} |
                              ignore.
init([]) ->
    process_flag(trap_exit, true),
    PostgresConfig = application:get_env(nova_blog, postgres_config, #{}),
    {ok, Connection} = epgsql:connect(PostgresConfig),
    gen_server:cast(?SERVER, migrate),
    {ok, #state{connection = Connection}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} |
                         {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
                         {reply, Reply :: term(), NewState :: term(), hibernate} |
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_call({get_entries, Amount}, _From, State = #state{connection = Conn}) ->
    SQL = [<<"SELECT ">>,
           <<"  nova_blog_author.name AS author, ">>,
           <<"  nova_blog_author.email AS author_email, ">>,
           <<"  nova_blog_entry.id, ">>,
           <<"  nova_blog_entry.title, ">>,
           <<"  nova_blog_entry.content, ">>,
           <<"  nova_blog_entry.added ">>,
           <<"FROM ">>,
           <<"  nova_blog_entry ">>,
           <<"INNER JOIN ">>,
           <<"  nova_blog_author ">>,
           <<"ON ">>,
           <<"  nova_blog_author.id = nova_blog_entry.author_id ">>,
           <<"ORDER BY ">>,
           <<"  nova_blog_entry.added DESC ">>,
           <<"LIMIT ">>,
           <<"  $1">>],
    {ok, Columns, Rows} = epgsql:equery(Conn, SQL, [Amount]),
    Result = rows_to_map(Columns, Rows),
    {reply, {ok, Result}, State};
handle_call({get_entry_by_id, Id}, _From, State = #state{connection = Conn}) ->
    SQL = [<<"SELECT ">>,
           <<"  nova_blog_author.name AS author, ">>,
           <<"  nova_blog_author.email AS author_email, ">>,
           <<"  nova_blog_entry.id, ">>,
           <<"  nova_blog_entry.title, ">>,
           <<"  nova_blog_entry.content, ">>,
           <<"  nova_blog_entry.added ">>,
           <<"FROM ">>,
           <<"  nova_blog_entry ">>,
           <<"INNER JOIN ">>,
           <<"  nova_blog_author ">>,
           <<"ON ">>,
           <<"  nova_blog_author.id = nova_blog_entry.author_id ">>,
           <<"WHERE ">>,
           <<"  nova_blog_entry.id = $1 ">>,
           <<"ORDER BY ">>,
           <<"  nova_blog_entry.added DESC ">>],
    {ok, Columns, Rows} = epgsql:equery(Conn, SQL, [Id]),
    Result = rows_to_map(Columns, Rows),
    {reply, {ok, Result}, State};
handle_call(get_latest_release, _From, State = #state{connection = Conn}) ->
    SQL = [<<"SELECT ">>,
           <<"  version, ">>,
           <<"  description, ">>,
           <<"  added ">>,
           <<"FROM ">>,
           <<"  nova_blog_release ">>,
           <<"ORDER BY ">>,
           <<"  added DESC ">>,
           <<"LIMIT 1">>],
    {ok, Columns, Rows} = epgsql:squery(Conn, SQL),
    Result = rows_to_map(Columns, Rows),
    {reply, {ok, Result}, State};
handle_call(get_releases, _From, State = #state{connection = Conn}) ->
    SQL = [<<"SELECT ">>,
           <<"  version, ">>,
           <<"  description, ">>,
           <<"  added ">>,
           <<"FROM ">>,
           <<"  nova_blog_release ">>,
           <<"ORDER BY ">>,
           <<"  added DESC ">>],
    {ok, Columns, Rows} = epgsql:squery(Conn, SQL),
    Result = rows_to_map(Columns, Rows),
    {reply, {ok, Result}, State};
handle_call(get_authors, _From, State = #state{connection = Conn}) ->
    SQL = [<<"SELECT ">>,
           <<"  id, ">>,
           <<"  name, ">>,
           <<"  description, ">>,
           <<"  email ">>,
           <<"FROM ">>,
           <<"  nova_blog_author">>],
    {ok, Columns, Rows} = epgsql:squery(Conn, SQL),
    Result = rows_to_map(Columns, Rows),
    {reply, {ok, Result}, State};
handle_call({get_author, Email}, _From, State = #state{connection = Conn}) ->
    SQL = [<<"SELECT ">>,
           <<"  id, ">>,
           <<"  name, ">>,
           <<"  description, ">>,
           <<"  email ">>,
           <<"FROM ">>,
           <<"  nova_blog_author ">>,
           <<"WHERE ">>,
           <<"  email=$1">>],
    {ok, Columns, Rows} = epgsql:equery(Conn, SQL, [Email]),
    Result = rows_to_map(Columns, Rows),
    {reply, {ok, Result}, State};
handle_call({get_release, Version}, _From, State = #state{connection = Conn}) ->
    SQL = [<<"SELECT ">>,
           <<"  version, ">>,
           <<"  description, ">>,
           <<"  added ">>,
           <<"FROM ">>,
           <<"  nova_blog_release ">>,
           <<"WHERE ">>,
           <<"  version = $1">>],
    {ok, Columns, Rows} = epgsql:equery(Conn, SQL, [Version]),
    Result = rows_to_map(Columns, Rows),
    {reply, {ok, Result}, State};
handle_call({get_forum_threads}, _From, State = #state{connection = Conn}) ->
    SQL = [<<"SELECT ">>,
           <<"  * ">>,
           <<"FROM ">>,
           <<"  nova_blog_forum ">>,
           <<"WHERE ">>,
           <<"  parent = '' ">>,
           <<"LIMIT 0, 50">>],
    {ok, Columns, Rows} = epgsql:equery(Conn, SQL, []),
    Result = rows_to_map(Columns, Rows),
    {reply, {ok, Result}, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_cast({new_entry, Title, Text, AuthorId}, State = #state{connection = Conn}) ->
    SQL = [<<"INSERT INTO ">>,
           <<"  nova_blog_entry ">>,
           <<"  (">>,
           <<"    title, ">>,
           <<"    content, ">>,
           <<"    author_id ">>,
           <<"  ) VALUES (">>,
           <<"    $1, ">>,
           <<"    $2, ">>,
           <<"    $3 ">>,
           <<"  )">>],
    case epgsql:equery(Conn, SQL, [Title, Text, AuthorId]) of
        {ok, _Count} ->
            ?DEBUG("Insert of new entry succeeded");
        Return ->
            ?WARNING("Could not insert new entry. Got return: ~p", [Return])
        end,
    {noreply, State};
handle_cast({new_author, Name, About, Email}, State = #state{connection = Conn}) ->
    SQL = [<<"INSERT INTO ">>,
           <<"  nova_blog_author ">>,
           <<"  (">>,
           <<"    name, ">>,
           <<"    email, ">>,
           <<"    description ">>,
           <<"  ) VALUES ( ">>,
           <<"    $1, ">>,
           <<"    $2, ">>,
           <<"    $3 ">>,
           <<"  )">>],
    case epgsql:equery(Conn, SQL, [Name, Email, About]) of
        {ok, _Count} ->
            ?DEBUG("Insert of new author succeeded");
        Return ->
            ?WARNING("Could not insert new author. Got return: ~p", [Return])
    end,
    {noreply, State};
handle_cast({new_release, Version, Changes, AuthorId}, State = #state{connection = Conn}) ->
    SQL = [<<"INSERT INTO ">>,
           <<"  nova_blog_release ">>,
           <<"  (">>,
           <<"    version, ">>,
           <<"    description, ">>,
           <<"    author_id ">>,
           <<"  ) VALUES (">>,
           <<"    $1, ">>,
           <<"    $2, ">>,
           <<"    $3 ">>,
           <<"  )">>],
    case epgsql:equery(Conn, SQL, [Version, Changes, AuthorId]) of
        {ok, _Count} ->
            ?DEBUG("Insert of new release succeeded");
        Return ->
            ?WARNING("Could not insert new release. Got return: ~p", [Return])
    end,
    {noreply, State};
handle_cast(migrate, State = #state{connection = Conn}) ->
    epgsql:squery(Conn, ?NOVA_BLOG_TABLES),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
                                      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
rows_to_map(Columns, Rows) ->
    Columns1 = lists:map(fun(#column{name = Name}) -> Name end, Columns),
    lists:map(fun(Row) ->
                      Row1 = erlang:tuple_to_list(Row),
                      maps:from_list(lists:zip(Columns1, Row1))
              end, Rows).
