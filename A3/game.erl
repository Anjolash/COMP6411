-module(game).
-export([start/1, messagePass/5, send_message/4, time_stamp/4, determine_winner/3]).

start(PlayerFile) ->
    io:fwrite(" *** Game Started ***\n"),
    {ok, PlayerInfo} = file:consult(PlayerFile),
    Wins = maps:new(),
    TokensList = [{Person, Tokens} || {Person, Tokens} <- PlayerInfo],
    TokensMap = maps:from_list(TokensList),
    [print_list(Person, Tokens) || {Person, Tokens} <- PlayerInfo],
    register(master, self()),
    player:game_loop(PlayerInfo, {Wins, TokensMap}, 0, 0).

print_list(Person, Tokens) ->
    io:format("~w: Tokens: ~w~n", [Person, Tokens]).

messagePass(From, Sender, Receiver, GameId, Time) ->
    time_stamp(Sender, Receiver, self(), Time),
    Mid = From,
    Lst1 = [{Mid, Sender}],
    M1 = maps:from_list(Lst1),
    send_message(From, M1, GameId, Time).

send_message(From, M1, GameId, Time) ->
    receive
        {knock, Head, Tail, Time, MessagePass_id} -> 
            timer:sleep(rand:uniform(100)),
            Move = random_rps(),
            From ! {move, Move, Head, Tail, Time, GameId},
            MessagePass_id ! {send_move, Move, Head, Tail, Time, GameId};
        {send_move, Move1, Head, Tail, Time, GameId} ->
            timer:sleep(rand:uniform(100)),
            Move2 = random_rps(),
            Winner = determine_winner(Head, Tail, {Move1, Move2}),
            From ! {game, Move1, Move2, Head, Tail, Time, Winner, GameId}
    after 5000 -> 
        Fid = From,
        H = maps:get(Fid, M1),
        io:fwrite("~nProcess ~w has received no calls for ~w seconds, ending...~n", [H, 5]),
        exit(0)
    end,
    send_message(From, M1, GameId, Time).

time_stamp(Head, Tail, MessagePass_id, Time) ->   
    Function = fun(X) -> 
        MessagePass_id ! {knock, Head, X, Time, MessagePass_id}
    end,
    lists:foreach(Function, Tail).

random_rps() ->
    Choices = [rock, paper, scissors],
    lists:nth(rand:uniform(length(Choices)), Choices).

determine_winner(Player1, Player2, {Move1, Move2}) ->
    case {Move1, Move2} of
        {rock, scissors} -> Player1;
        {scissors, rock} -> Player2;
        {paper, rock} -> Player1;
        {rock, paper} -> Player2;
        {scissors, paper} -> Player1;
        {paper, scissors} -> Player2;
        _ -> draw
    end.
