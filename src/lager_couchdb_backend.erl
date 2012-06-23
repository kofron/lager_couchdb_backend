% lager_couchdb_backend.erl
% @doc A backend for lager which uses couchdb for persistence.  
-module(lager_couchdb_backend).
-behaviour(gen_event).

-export([init/1, 
	 handle_call/2, 
	 handle_event/2, 
	 handle_info/2, 
	 terminate/2,
	 code_change/3]).

-record(state, {level, host, port, db}).

init(Args) ->
    Level = config_val(level, Args, info),
    Host = config_val(host, Args, "127.0.0.1"),
    Port = config_val(port, Args, 5984),
    Db = config_val(db_name, Args, "lager"),
    {ok, #state{level = Level, host = Host, port = Port, db=Db}}.

handle_call({set_loglevel, NewLevel}, SD) ->
    NewState = SD#state{level=lager_util:level_to_num(NewLevel)},
    {ok, ok, NewState};
handle_call(get_loglevel, #state{level=Lvl}=SD) ->
    {ok, Lvl, SD}.
    
handle_event({log, Level, {Date, Time}, Message}, #state{ level = L } = State) when Level =< L->
  {ok, do_log(Level, Date, Time, Message, State)};
  
handle_event(_Event, State) ->
  {ok, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Config extractor shamelessly lifted from 
%% AMQP backend.
config_val(C, Params, Default) ->
  case lists:keyfind(C, 1, Params) of
    {C, V} ->
	  V;
          _ -> Default
  end.

%% The meat of the backend.  Uses couchbeam and is pretty simple
do_log(Level, Date, Time, Message, #state{host=H,port=P,db=D}=SD) ->
    Server = couchbeam:server_connection(H,P),
    Url = make_url(H,P,D),
    JsonMsg = to_json(Level, Date, Time, Message),
    io:format("sending ~p~n",[JsonMsg]),
    Resp = httpc:request(post, {Url,
    			 [],
                         "application/json",
                         JsonMsg},
                         [],
                         []),
   io:format("got response: ~p~n",[Resp]),
    SD.

make_url(Host,Port,DbName) ->
lists:flatten(io_lib:format("http://~s:~B/~s",[Host,Port,DbName])).

to_json(Level, Date, Time, Message) ->
 FString = "{\"node\":\"~s\", 
             \"message\":\"~s\",
             \"date\":\"~s\",
             \"time\":\"~s\",
             \"level\":\"~s\"}",
 Node = node_string(),
 LevelStr = make_level_str(Level),
 lists:flatten(io_lib:format(FString,[Node,Message,Date,Time,LevelStr])).

node_string() ->
 erlang:atom_to_list(node()).

make_level_str(LevelNum) ->
 erlang:atom_to_list(lager_util:num_to_level(LevelNum)).
