-module(receiver).
-author("licht741").

-record(market_data, {
  id          :: non_neg_integer(),
  company     :: nonempty_string(),
  exchange    :: nonempty_string(),
  cost        :: float(),
  check_time  :: nonempty_string()
}).

-type market_data() :: #market_data{}.

-export([get_current_data/2,
         print_data/1,
         send_to_database/2,
         connect_to_database/2]).
-export_type([market_data/0]).

%% API
-spec get_current_data(nonempty_string(), nonempty_string()) -> market_data() | invalid_input_value.
get_current_data(Exchange, Company) ->
  Prefix = "http://finance.google.com/finance/info?client=ig&q=",
  URL = Prefix ++ Company ++ ":" ++ Exchange,
  Body = response_body(httpc:request(get, {URL, []}, [], [])),
  CrntData = create_structure(Body),
  CrntData.

-spec print_data(market_data()) -> ok.
print_data(Data) ->
  Company = Data#market_data.company,
  Cost    = Data#market_data.cost,
  Date    = Data#market_data.check_time,
  io:format("~s(~s) - ~w~n", [Company, Date, Cost]),
  ok.

connect_to_database(Address, Port) ->
  {ok, Pid} = riakc_pb_socket:start_link(Address, Port),
  {ok, Pid}.

send_to_database(DB_Pid, Data) ->
  Bucket  = list_to_binary(Data#market_data.company),
  Key     = list_to_binary(Data#market_data.check_time),
  DataObj = riakc_obj:new(Bucket, Key, term_to_binary(Data)),
  riakc_pb_socket:put(DB_Pid, DataObj),
  ok.




%% additional functions

-spec parsing_data(nonempty_string()) -> list().
parsing_data(S) ->
  X = re:replace(S, "\n", "", [global, {return, list}]),
  Data = string:substr(X, 6, string:len(X) - 7),
  Tokens = string:tokens(Data, ","),
  [ lists:nth(1, Tokens),
    lists:nth(2, Tokens),
    lists:nth(3, Tokens),
    lists:nth(4, Tokens),
    lists:nth(11, Tokens)
  ].

is_correct_data({Id, Company, Exc, Cost, CheckT})
  when is_integer(Id),
       is_list(Company),
       is_list(Exc),
       is_float(Cost),
       is_list(CheckT) ->
  true;
is_correct_data(_M) -> false.

-spec create_structure(nonempty_string()) -> market_data().
create_structure(S) ->
  L         = parsing_data(S),
  ClStr     = re:replace(L, "\"", " ", [global, {return, list}]),
  Tokens    = string:tokens(ClStr, " "),
  Id        = list_to_integer(lists:nth(3, Tokens)),
  Company   = lists:nth(6, Tokens),
  Exchange  = lists:nth(9, Tokens),
  Cost      = list_to_float(lists:nth(12, Tokens)),
  CheckTime = lists:nth(15, Tokens),
  case is_correct_data({Id, Company, Exchange, Cost, CheckTime}) of
    true ->
      #market_data{id         = Id,
                   company    = Company,
                   exchange   = Exchange,
                   cost       = Cost,
                   check_time = CheckTime};
    false ->
      invalid_input_value
  end.

response_body({ok, { _, _, Body}}) -> Body.
