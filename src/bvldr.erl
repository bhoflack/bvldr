-module(bvldr).
-behaviour(gen_server).

-export([start_link/0, create/0, start/1, list/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Public API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create() ->
    gen_server:call(?MODULE, create_container).

start(Ref) ->
    gen_server:cast(?MODULE, {start_container, Ref}).

list() ->
    gen_server:call(?MODULE, list_containers).

%% Server functions
init([]) -> {ok, []}.

handle_call(create_container, _From, State) ->
    {ok, Response} = docker_container:create([
     {'Hostname', <<"">>},
     {'User', <<"">>},
     {'Memory', 0},
     {'MemorySwap', 0},
     {'AttachStdin', false},
     {'AttachStdout', true},
     {'AttachStderr', true},
     {'Privileged', false},
     {'Tty', false},
     {'OpenStdin', false},
     {'StdinOnce', false},
     {'Env', null},
     {'Dns', null},
     {'Image', <<"ubuntu">>},
     {'Volumes', []},
     {'VolumesFrom', <<"">>},
     {'WorkingDir', <<"">>},
     {'Cmd', [<<"/bin/bash">>, <<"-c">>, <<"while true; do echo Hello world; sleep 1; done">>]}
    ]),
    {reply, Response, State};

handle_call(list_containers, _From, State) ->
    {ok, Containers} = docker_container:containers(),
    {reply, Containers, State};

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({start_container, Ref}, State) ->
    {ok, _Response} = docker_container:start(Ref, []),
    {noreply, State};

handle_cast(_Msg, State) ->
   {noreply, State}.
 
handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
