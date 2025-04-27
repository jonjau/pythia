% BEGIN fact-imports

:- use_module('./data/dimlink.pl').
:- use_module('./data/transaction.pl').
:- use_module('./data/edge.pl').
:- use_module('./data/arc.pl').

% END fact-imports

:- use_module(library(clpz)).
:- use_module(library(lists)).

change_step(Ctx, Id, Vals1, Vals2) :-
    record(Ctx, _, _, SeqNum1, Id, Vals1),
    record(Ctx, _, _, SeqNum2, Id, Vals2),
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

% TODO: use a templating language to create these glue predicates:
% if we try to get scryer-prolog to return an object like values(DRef, IRef, BegPeriod, EndPeriod)
% it will not work, so I've opted to use the built-in prolog list.

% BEGIN record-predicates

record(Context, EditTime, RecStatus, SeqNum, Id, [DRef, IRef, BegPeriod, EndPeriod]) :-
    dimlink(Id, DRef, IRef, BegPeriod, EndPeriod, Context, EditTime, RecStatus, SeqNum).

record(Context, EditTime, RecStatus, SeqNum, Id, [DRef, IRef, BegPeriod, EndPeriod]) :-
    transaction(Id, DRef, IRef, BegPeriod, EndPeriod, Context, EditTime, RecStatus, SeqNum).

% END record-predicates