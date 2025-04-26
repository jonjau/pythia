% TODO: use a templating language to create these glue predicates:
% if we try to get scryer-prolog to return an object like values(DRef, IRef, BegPeriod, EndPeriod)
% it will not work, so I've opted to use the built-in prolog list.
record(Context, EditTime, SeqNum, RecStatus, Id, [DRef, IRef, BegPeriod, EndPeriod]) :-
    dimlink(Id, DRef, IRef, BegPeriod, EndPeriod, Context, EditTime, RecStatus, SeqNum).
