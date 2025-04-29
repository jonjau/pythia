:- use_module(library(clpz)).
:- use_module(library(lists)).

change_step(Ctx, Id, Vals1, Vals2) :-
    record(Ctx, _, SeqNum1, _, Id, Vals1),
    record(Ctx, _, SeqNum2, _, Id, Vals2),
    number_chars(Num1, SeqNum1),
    number_chars(Num2, SeqNum2),
    Num2 #= Num1 + 1,
    Vals1 \= Vals2.

change_path(Ctx, Id, Vals, Vals, []) :-
    record(Ctx, _, _, _, Id, Vals).

change_path(Ctx, Id, Vals1, Vals2, [Step|Steps]) :-
    % Enforce step exists before constructing step term
    change_step(Ctx, Id, Vals1, ValsMid),  
    Step = [Vals1, ValsMid],
    change_path(Ctx, Id, ValsMid, Vals2, Steps).