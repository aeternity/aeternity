{% set depth = 0 %}
{{ schema.keywords.get("title").literal | default("Schema Docs") | md_heading(depth) }}
{% set contentBase %}
{% with schema=schema, skip_headers=False, depth=depth %}
    {% include "content.md" %}
{% endwith %}
{% endset %}

{{ md_get_toc() }}

{{ contentBase }}

----------------------------------------------------------------------------------------------------------------------------
{% if config.with_footer -%}
Generated using [json-schema-for-humans](https://github.com/coveooss/json-schema-for-humans){% if config.footer_show_time %} on {{ get_local_time() }}{% endif %}

{% endif -%}
