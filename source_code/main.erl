-module(main).
-author("licht741").

%% API
-export([start/0,
  receive_new_data/2,
  handle_new_data/2, send_to_database/2]).

-include("node.hrl").

receive_new_data({Exchange, Company}, _Value) ->
  Data = receiver:get_current_data(Exchange, Company),
  {{Exchange, Company}, Data}.

handle_new_data(_State, Value) ->
  receiver:print_data(Value),
  {_State, Value}.

send_to_database(DB_Pid, Value) ->
  receiver:send_to_database(DB_Pid, Value),
  {DB_Pid, Value}.

start() ->
  inets:start(),
%%   Database
   {ok, DB_Pid} = receiver:connect_to_database("127.0.0.1", 8087),

%%   Entry
  {ok, Entry}  = frp_api:start_node(fun(_State, _Value) -> {_State, _Value} end),

%%   Receivers
  {ok, RecG}   = frp_api:start_node(fun(State, _Value) -> main:receive_new_data(State, _Value) end, {"NASDAQ", "GOOG"}),
  {ok, RecA}   = frp_api:start_node(fun(State, _Value) -> main:receive_new_data(State, _Value) end, {"NASDAQ", "AAPL"}),

%%   Handlers
  {ok, HandG}  = frp_api:start_node(fun(State, Value) -> main:handle_new_data(State, Value) end),
  {ok, HandA}  = frp_api:start_node(fun(State, Value) -> main:handle_new_data(State, Value) end),

%% Sender to database
  {ok, DBSend} = frp_api:start_node(fun(State, Value) ->
                                        receiver:send_to_database(State, Value),
                                        {State, Value}
                                    end),

%%   constructing our network
  Network = frp_api:create_network(),
  Nodes = [{entry, Entry}, {rec_g, RecG}, {rec_a, RecA}, {han_g, HandG}, {han_a, HandA}],
  frp_api:add_nodes(Network, Nodes),
%%   network:add_node(Network, {send_db, DBSend}),

  frp_api:add_listeners(Network, entry, [rec_g, rec_a]),
  frp_api:add_listener(Network, rec_g, han_g),
  frp_api:add_listener(Network, rec_a, han_a),

%%   loop
  frp_api:timer({Network, entry}, some_event, 10000),

  ok.
