:- discontiguous(record/{{ 5 }}).{#~ 5 because context, seqnum, name, id_fields, data_fields) ~#}
:- dynamic({{ record_type.name }}/{{ 2 + record_type.id_fields.len() + record_type.data_fields.len() }}).
{% for fact in facts -%}
{{ record_type.name }}({% for value in fact.values -%} {{ value }} {%- if !loop.last %}, {% endif %}{%- endfor %}).
{% endfor -%}