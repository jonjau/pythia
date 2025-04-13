:- use_module(library(clpz)).
:- use_module(library(lists)).

:- dynamic(dimlink/9).

dimlink("M1", "ID1", "J1", "2023-02-08", "2023-02-10", "Test1", "2024-02-18 08:16:11", "D", "0"). 
dimlink("M1", "ID1", "J1", "2023-02-09", "2023-02-10", "Test1", "2024-02-18 08:17:11", "E", "1"). 
dimlink("M2", "ID2", "J1", "2023-02-08", "2023-02-11", "Test1", "2024-02-18 08:20:11", "D", "0").
dimlink("M2", "ID2", "J1", "2023-02-08", "2023-02-11", "Test1", "2024-02-18 08:20:12", "D", "1"). 
dimlink("M2", "ID2", "J1", "2023-02-08", "2023-02-11", "Test1", "2024-02-18 08:20:13", "D", "2"). 
dimlink("M2", "ID2", "J2", "2023-02-09", "2023-02-11", "Test1", "2024-02-18 08:20:14", "D", "3"). 
dimlink("M2", "ID2", "J3", "2023-02-08", "2023-02-11", "Test1", "2024-02-18 08:20:15", "E", "4").
dimlink("M3", "ID2", "J2", "2023-02-08", "2023-02-09", "Test1", "2024-02-18 08:20:14", "O", "0"). 
dimlink("M4", "ID1", "J2", "2023-02-08", "2023-02-09", "Test1", "2024-02-18 09:17:11", "O", "0").
dimlink("M5", "ID1", "J3", "2023-02-08", "2023-02-09", "Test1", "2024-02-18 09:17:11", "D", "0").
dimlink("M5", "ID1", "J3", "2023-02-08", "2023-02-09", "Test1", "2024-02-18 09:17:11", "V", "1").

table("dimlink", "MRef", ["DRef", "IRef", "BegPeriod", "EndPeriod"]).

% TODO: use a templating language to create these glue predicates:
% if we try to get scryer-prolog to return an object like values(DRef, IRef, BegPeriod, EndPeriod)
% it will not work, so I've opted to use the built-in prolog list.
record(Context, EditTime, SeqNum, RecStatus, Id, [DRef, IRef, BegPeriod, EndPeriod]) :-
    dimlink(Id, DRef, IRef, BegPeriod, EndPeriod, Context, EditTime, RecStatus, SeqNum).

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

:- dynamic(edge/2).
edge(3, 4).
edge(4, 10).


arc(A, C) :-
    edge(A, B),
    edge(B, C).
