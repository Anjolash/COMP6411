-module(player).
-export([game_loop/4, post_feedback/5, generate_opponents/5]).

game_loop(PlayerInfo, {Wins, Tokens}, GameCount, GameResultsCount) ->
    NewGameCount = lists:foldl(fun({Player, _}, Acc) ->
        generate_opponents(Player, PlayerInfo, self(), Acc, Tokens)
    end, GameCount, PlayerInfo),
    post_feedback(Wins, Tokens, PlayerInfo, NewGameCount, GameResultsCount).

generate_opponents(Player, PlayerInfo, Master_id, GameCount, Tokens) ->
    case maps:get(Player, Tokens) of
        0 -> GameCount; 
        _ ->
            Opponents = [OtherPlayer || {OtherPlayer, _} <- PlayerInfo, OtherPlayer =/= Player, maps:get(OtherPlayer, Tokens) > 0],
            lists:foldl(fun(Opponent, Acc) ->
                Time = erlang:system_time(millisecond),
                NewGameId = Acc + 1,
                spawn(game, messagePass, [Master_id, Player, [Opponent], NewGameId, Time]),
                Acc + 1 
            end, GameCount, Opponents)
    end.

post_feedback(Wins, Tokens, PlayerInfo, GameCount, GameResultsCount) ->
    receive
        {move, _Move, Head, Tail, _Time, GameId} ->
            case maps:get(Tail, Tokens) of
                0 ->
                    io:fwrite("Player ~w has 0 tokens and cannot participate in the game.~n", [Tail]),
                    post_feedback(Wins, Tokens, PlayerInfo, GameCount, GameResultsCount);
                _ ->
                    maybe_print(new_game, "~w -> ~w", [Head, Tail, GameId]),
                    post_feedback(Wins, Tokens, PlayerInfo, GameCount, GameResultsCount)
            end;
        {game, Move1, Move2, Head, Tail, _Time, Winner, GameId} ->
            maybe_print(result, "$ Game result (ID: ~w): ~w made move ~w, ~w made move ~w. Winner: ~w~n", [GameId, Head, Move1, Tail, Move2, Winner]),
            NewWins = update_wins(Winner, Wins),
            NewTokens = update_tokens(Winner, [Head, Tail], Tokens),
            NewGameResultsCount = GameResultsCount + 1,
            case check_tokens(NewTokens) of
                true -> 
                    WinnerName = find_winner(NewTokens),
                    maybe_print(final, "~nGame over. We have a winner!.~n", []),
                    maybe_print(final, "Total games: ~w~n", [NewGameResultsCount]),
                    maybe_print(final, "Winner: ~w~n", [WinnerName]),
                    print_wins(NewWins),
                    print_tokens(NewTokens),
                    halt(0);
                false ->
                    game_loop(PlayerInfo, {NewWins, NewTokens}, GameCount, NewGameResultsCount)
            end
    after 10000 ->
        maybe_print(timeout, "~nMaster has received no replies for 10 seconds, ending...~n", []),
        maybe_print(timeout, "Total number of game results: ~w~n", [GameResultsCount]),
        print_wins(Wins),
        print_tokens(Tokens),
        halt(0)
    end.

update_wins(draw, Wins) ->
    Wins;
update_wins(Player, Wins) ->
    CurrentWins = maps:get(Player, Wins, 0),
    maps:put(Player, CurrentWins + 1, Wins).

update_tokens(draw, _, Tokens) ->
    Tokens;
update_tokens(Winner, [Player1, Player2], Tokens) ->
    Loser = if Winner == Player1 -> Player2; Winner == Player2 -> Player1 end,
    CurrentTokens = maps:get(Loser, Tokens, 0),
    NewTokens = if CurrentTokens > 0 -> CurrentTokens - 1; true -> 0 end,
    maps:put(Loser, NewTokens, Tokens).

check_tokens(Tokens) ->
    AlivePlayers = maps:fold(fun(_, Value, Acc) -> if Value > 0 -> Acc + 1; true -> Acc end end, 0, Tokens),
    AlivePlayers =:= 1.

find_winner(Tokens) ->
    maps:fold(fun(Key, Value, Acc) -> if Value > 0 -> Key; true -> Acc end end, none, Tokens).

print_wins(Wins) ->
    maybe_print(final, "~nFinal Win Counts:~n", []),
    maps:fold(fun(Key, Value, _) -> io:fwrite("~w: ~w wins~n", [Key, Value]) end, ok, Wins).

print_tokens(Tokens) ->
    maybe_print(final, "~nFinal Token Counts:~n", []),
    maps:fold(fun(Key, Value, _) -> io:fwrite("~w: ~w tokens~n", [Key, Value]) end, ok, Tokens).

maybe_print(Type, Format, Args) ->
    Master = whereis(master),
    case self() of
        Master ->
            case Type of
                new_game ->
                    [Head, Tail, GameId] = Args,
                    io:fwrite("+ [~w] New game for ~w -> ~w~n", [GameId, Head, Tail]);
                result ->
                    [GameId, Head, Move1, Tail, Move2, Winner] = Args,
                    io:fwrite("$ Game result (ID: ~w): ~w made move ~w, ~w made move ~w. Winner: ~w~n", [GameId, Head, Move1, Tail, Move2, Winner]);
                _ ->
                    io:fwrite(Format, Args)
            end;
        _ -> ok
    end.
