{% for import_path in import_paths -%}
:- use_module({{import_path}}).
{% endfor -%}
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

{#~ % If we try to get scryer-prolog to return an object like values(DRef, IRef, BegPeriod, EndPeriod)
% it will not work, so I've opted to use the built-in prolog list. ~#}

{% for record in record_types -%}
record(
    "{{record.name}}",
    {{record.metadata_fields}},
    [{{record.id_fields}}],
    [{{record.data_fields}}]
) :-
{{record.name}}({{record.id_fields}}, {{record.data_fields}}, {{record.metadata_fields}}).

{% endfor -%}