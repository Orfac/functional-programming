-module(chat_server).

-behaviour(gen_server).

-import(utils, [list_contains/2]).

%% API
-export([start_link/0, start/0, stop/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-include("chat_types.hrl").

-record(client, {tag = term(), pid = pid(), username = user_name()}).
-record(state, {clients = [] :: [client()], rooms :: rooms()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() -> start_link().

stop() -> gen_server:stop(?MODULE).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{}}.

handle_call(Request, From, State) ->
  io:format("server: received call: (Request: ~p, From: ~p, State: ~p) ~n", [Request, From, State]),
  #client{tag = }
  {Pid, Tag} = From,
  #state{clients = Clients} = State,
  case Request of
    {join, RoomName, Username} ->
      {Reply, NewState} = join_room(RoomName, Username, Pid),
      {reply, Reply, NewState};
%%      case client_with_pid_exists(Pid, Clients) of
%%        true -> {reply, already_joined, State};
%%        false ->
%%          erlang:monitor(process, Pid),
%%          NewClients = [{Pid, Username}|Clients],
%%          {reply, joined, State#state{clients=NewClients}};
%%        _ -> this_shouldnt_happen
%%      end;
    {say, Text} ->
      io:format("Some user said some text!", []),
      case get_username(Pid, Clients) of
        false -> {reply, you_are_not_a_user_yet, State};
        Username ->
          send_message_to_clients(Username, Text, Clients),
%%          {reply, you_said_something, State#state{messages=[{Username,Text} | Messages]}}
          {reply, you_said_something, State}
      end;
    get_clients -> {reply, State#state.clients, State};
    stop -> {stop, normal, shutdown_ok, State};
    _ -> {reply, wat, State}
  end.

join_room(State, RoomName, Username, Pid) ->
  case not dict:is_key(RoomName, State#state.rooms) of
    true -> {room_not_present, State};
    false ->
      case client_is_in_room() of
        true ->
          ok;
        false ->
          {room_not_present, State};
        _ -> none
      end;
    _ -> none
  end.

%%client_is_in_room(Pid, RoomName, State) ->
%%  State#state.

handle_cast(Request, State) ->
  case Request of
    {part, Pid} ->
      io:format("~p wants to part, removing it from clients~n", [Pid]),
      no_reply_with_state_without_pid(State, Pid);
    _ ->
      io:format("server: received cast: ~p~n", [Request]),
      {noreply, State}
  end.

no_reply_with_state_without_pid(State, Pid) ->
  OldClients = State#state.clients,
  NewClients = get_clients_without_pid(Pid, OldClients),
  {noreply, State#state{clients = NewClients}}.

handle_info(Info, State) ->
  case Info of
    {'DOWN', _Ref, process, Pid, Why} ->
      io:format("~p has died because of ~p, removing it from clients~n", [Pid, Why]),
      no_reply_with_state_without_pid(State, Pid);
    _ ->
      io:format("server: received info: ~p~n", [Info]),
      {noreply, State}
  end.

terminate(_Reason, _State) ->
  io:format("server: terminating~n"),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

client_has_pid(Client, Pid) ->
  {Client_pid, _Name} = Client,
  Client_pid == Pid.

client_with_pid_exists(Pid, Clients) ->
  lists:any(fun(C) -> client_has_pid(C, Pid) end, Clients).

get_clients_without_pid(Pid, Clients) ->
  lists:filter(fun(C) -> not client_has_pid(C, Pid) end, Clients).

get_username(Pid, Clients) ->
  case lists:keyfind(Pid, 1, Clients) of
    {_Pid, Username} -> Username;
    false -> false
  end.

get_client_pid(Client) ->
  case Client of
    {Pid, _Username} -> Pid
  end.

send_message_to_clients(Username, Text, Clients) ->
  Pids = lists:map(fun get_client_pid/1, Clients),
  SendMessageToPid = fun(Pid) ->
    io:format("server: sending ~p to ~p~n", [{said, {Username, Text}}, Pid]),
    Pid ! {said, {Username, Text}}
                     end,
  lists:foreach(SendMessageToPid, Pids).