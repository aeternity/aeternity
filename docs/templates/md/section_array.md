{{ schema | md_array_restrictions | md_generate_table }}

{% if schema.array_items_def or schema.tuple_validation_items %}
{{ schema | md_array_items_restrictions | md_generate_table }}
{% endif %}

{% if schema.array_items_def %}
{% filter md_heading(depth+1, schema.array_items_def.html_id) %}
{% with schema=schema.array_items_def %}{%- include "breadcrumbs.md" %}{% endwith %}
{% endfilter %}
{% with schema=schema.array_items_def, skip_headers=False, depth=depth+1, skip_required=True %}
    {% include "content.md" %}
{% endwith %}
{% endif %}

{% if schema.tuple_validation_items %}
{% for item in schema.tuple_validation_items %}
    {% filter md_heading(depth+1) %}
    {% with schema=item %}{%- include "breadcrumbs.md" %}{% endwith %}
    {% endfilter %}
    {% with schema=item, skip_headers=False, depth=depth+1, skip_required=True %}
        {% include "content.md" %}
    {% endwith %}
{% endfor %}
{% endif %}

{% if schema.kw_contains and schema.kw_contains.literal != {} %}
{{ "At least one of the items must be" | md_heading(depth+1) }}
{% with schema=schema.kw_contains, skip_headers=False, depth=depth+1, skip_required=True %}
    {% include "content.md" %}
{% endwith %}
{% endif %}

{% if schema.array_additional_items_def %}
{{ "Additional items must be" | md_heading(depth+1) }}
{% with schema=schema.array_additional_items_def, skip_headers=False, depth=depth+1, skip_required=True %}
    {% include "content.md" %}
{% endwith %}
{% endif %}
