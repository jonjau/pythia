:- use_module('./data/dimlink.pl').
:- use_module('./data/transaction.pl').
:- use_module('./data/edge.pl').
:- use_module('./data/arc.pl').
:- use_module(library(clpz)).
:- use_module(library(lists)).

change_step(RType, Ctx, Id, Vals1, Vals2) :-
    record(RType, Ctx, _, _, SeqNum1, Id, Vals1),
    record(RType, Ctx, _, _, SeqNum2, Id, Vals2),
    number_chars(Num1, SeqNum1),
    number_chars(Num2, SeqNum2),
    Num2 #= Num1 + 1,
    Vals1 \= Vals2.

change_path(RType, Ctx, Id, Vals, Vals, []) :-
    record(RType, Ctx, _, _, _, Id, Vals).

change_path(RType, Ctx, Id, Vals1, Vals2, [Step|Steps]) :-
    % Enforce step exists before constructing step term
    change_step(RType, Ctx, Id, Vals1, ValsMid),  
    Step = [Vals1, ValsMid],
    change_path(RType, Ctx, Id, ValsMid, Vals2, Steps).


record(
    "dimlink",
    Context, EditTime, RecStatus, SeqNum,
    [Id],
    [DRef, IRef, BegPeriod, EndPeriod]
) :-
dimlink(Id, DRef, IRef, BegPeriod, EndPeriod, Context, EditTime, RecStatus, SeqNum).

record(
    "transaction",
    Context, EditTime, RecStatus, SeqNum,
    [Id1, Id2],
    [DRef, IRef, BegPeriod, EndPeriod]
) :-
transaction(Id1, Id2, DRef, IRef, BegPeriod, EndPeriod, Context, EditTime, RecStatus, SeqNum).

