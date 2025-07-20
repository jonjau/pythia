:- dynamic({{ record_type.name }}/{{ record_type.metadata_fields.len() + record_type.id_fields.len() + record_type.data_fields.len() }}).
{% for fact in facts -%}
{{ record_type.name }}({% for value in fact.values -%} {{ value }} {%- if !loop.last %}, {% endif %}{%- endfor %}).
{% endfor -%}
{#~ 3 because name, id_fields, data_fields) ~#}
:- discontiguous(record/{{ record_type.metadata_fields.len() + 3 }}).