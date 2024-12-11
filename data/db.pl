:- use_module(library(clpz)).

:- dynamic(dimlink/9).

dimlink("Test1", "M1", "ID1", "J1", "2023-02-08", "2023-02-10", "2024-02-18 08:16:11", "D", "0"). 
dimlink("Test1", "M1", "ID1", "J1", "2023-02-09", "2023-02-10", "2024-02-18 08:17:11", "E", "1"). 
dimlink("Test1", "M2", "ID2", "J1", "2023-02-08", "2023-02-11", "2024-02-18 08:20:11", "D", "0").
dimlink("Test1", "M2", "ID2", "J1", "2023-02-08", "2023-02-11", "2024-02-18 08:20:12", "D", "1"). 
dimlink("Test1", "M2", "ID2", "J1", "2023-02-08", "2023-02-11", "2024-02-18 08:20:13", "D", "2"). 
dimlink("Test1", "M2", "ID2", "J2", "2023-02-09", "2023-02-11", "2024-02-18 08:20:14", "D", "3"). 
dimlink("Test1", "M2", "ID2", "J3", "2023-02-08", "2023-02-11", "2024-02-18 08:20:15", "E", "4").
dimlink("Test1", "M3", "ID2", "J2", "2023-02-08", "2023-02-09", "2024-02-18 08:20:14", "O", "0"). 
dimlink("Test1", "M4", "ID1", "J2", "2023-02-08", "2023-02-09", "2024-02-18 09:17:11", "O", "0").
dimlink("Test1", "M5", "ID1", "J3", "2023-02-08", "2023-02-09", "2024-02-18 09:17:11", "D", "0").
dimlink("Test1", "M5", "ID1", "J3", "2023-02-08", "2023-02-09", "2024-02-18 09:17:11", "V", "1").

table("dimlink", "MgrLinkRef", ["DimIdRef", "InvHeadRef", "BegPeriod", "EndPeriod"]).

% TODO: use a templating language to create these glue predicates:
record(Context, EditTime, SeqNum, RecStatus, Id, [DRef, IRef, BegPeriod, EndPeriod]) :-
    dimlink(Context, Id, DRef, IRef, BegPeriod, EndPeriod, EditTime, RecStatus, SeqNum).

step_change(Ctx, Id, Vals1, Vals2) :-
    record(Ctx, _, SeqNum1, _, Id, Vals1),
    record(Ctx, _, SeqNum2, _, Id, Vals2),
    number_chars(Num1, SeqNum1),
    number_chars(Num2, SeqNum2),
    Num2 #= Num1 + 1.

leap_change(Ctx, Id, Vals1, Vals2) :-
    record(Ctx, _, SeqNum1, _, Id, Vals1),
    record(Ctx, _, SeqNum2, _, Id, Vals2),
    number_chars(Num1, SeqNum1),
    number_chars(Num2, SeqNum2),
    Num2 #> Num1.

:- dynamic(edge/2).
edge(3, 4).
edge(4, 10).


arc(A, C) :-
    edge(A, B),
    edge(B, C).
