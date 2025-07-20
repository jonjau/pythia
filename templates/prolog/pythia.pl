{% for import_path in import_paths -%}
:- use_module('{{import_path}}').
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
    {%- for metadata_field in record.metadata_fields %}
    {{ metadata_field }},
    {%- endfor %}
    [{% for id_field in record.id_fields -%} {{ id_field }} {%- if !loop.last -%} , {% endif %} {%- endfor %}],
    [{% for data_field in record.data_fields -%} {{data_field}} {%- if !loop.last -%} , {% endif %} {%- endfor %}]
) :-
{{record.name}}(
    {% for id_field in record.id_fields -%} {{ id_field }}, {% endfor -%}
    {% for data_field in record.data_fields -%} {{ data_field }}, {% endfor -%}
    {% for metadata_field in record.metadata_fields -%} {{ metadata_field }} {%- if !loop.last -%} , {% endif %}{% endfor %}
).

{% endfor -%}