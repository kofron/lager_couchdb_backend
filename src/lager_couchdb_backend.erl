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
    {ok, Db} = couchbeam:open_db(Server,D),
    RawDoc = {[]},
    FinalDoc = couchbeam_doc:extend([
				     {<<"level">>,
				      Level},
				     {<<"time">>,
				      erlang:list_to_binary(lists:flatten(Time))},
				     {<<"date">>,
				      erlang:list_to_binary(lists:flatten(Date))},
				     {<<"node">>,
				      erlang:atom_to_binary(node(),latin1)},
				     {<<"message">>, 
				      erlang:list_to_binary(lists:flatten(Message))}
				    ],RawDoc),
    couchbeam:save_doc(Db, FinalDoc),
    SD.
