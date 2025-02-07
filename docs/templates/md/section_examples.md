**Example{% if examples|length > 1 %}s{% endif %}:**{{- "" -}}

{% for example in examples %}
    {%- if loop.first %}{{ "\n\n" }}{% endif -%}
    {% set example_id = schema.html_id ~ "_ex" ~ loop.index %}
    {%- if not examples_as_yaml -%}
        {{- "" }}```json
        {{- "\n" }}{{ example }}
        {{- "\n" }}```
    {%- else -%}
        {{- "" }}```yaml
        {{- "\n" }}{{ example | yaml_example }}
        {{- "\n" }}```
    {%- endif -%}
    {{ "\n" }}
{% endfor %}
