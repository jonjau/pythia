:- module(arc, [arc/2]).

:- use_module('./edge.pl').

arc(A, C) :-
    edge(A, B),
    edge(B, C).