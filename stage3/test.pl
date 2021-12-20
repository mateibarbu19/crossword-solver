fill_ans2(_, _, _, _, [], Data, Data) :- writeln('aici4').
fill_ans2(H, W, (X, Y), Dir, L, FilledData, Data) :-
    writeln('aici3').
    % (Dir == d ->
    %     (A = X, B is Y + 1);
    %     (A is X + 1, B = Y)),
    % NewP = (A, B),
    % Pos is A * W + B,
    % nth0(Pos, Data, L),
    % (L = H ->
    %     NewData = Data;
    %     !, fail),
    % fill_ans2(H, W, NewP, Dir, T, FilledData, NewData).