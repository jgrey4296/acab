{# module.rst -*- mode: Jinja2 -*- #}
{% import "python/utils.jinja" as utils %} 

{% if obj.display %}
   {% set visible_children = obj.children|selectattr("display")|list %}
   {% set own_page_children = visible_children|selectattr("type", "in", own_page_types)|list %}
   
   {{- utils.smartref(obj) }}
   
.. py:{{ obj.type }}:: {{ obj.short_name }}{% if obj.args %}({{ obj.args }}){% endif %}

   {% for (args, return_annotation) in obj.overloads %}
      {{ " " * (obj.type | length) }}   {{ obj.short_name }}{% if args %}({{ args }}){% endif %}
   {% endfor %}
   
   {% if obj.bases %}
      {% if "show-inheritance" in autoapi_options %}
   Bases: {% for base in obj.bases %}{{ base|link_objs }}{% if not loop.last %}, {% endif %}{% endfor %}
      {% endif %} {# show-inheritance #}

      {% if "show-inheritance-diagram" in autoapi_options and obj.bases != ["object"] %}
   .. autoapi-inheritance-diagram:: {{ obj.obj["full_name"] }}
      :parts: 1
         {% if "private-members" in autoapi_options %}
      :private-bases:
         {% endif %} {# private-members #}

      {% endif %} {# show-inheritance-diagram #}
   {% endif %} {# obj.bases #}
   
   {% if obj.docstring %}
   {{ obj.docstring|indent(3) }}
   {% endif %}
   
   {% for obj_item in visible_children %}
      {% if obj_item.type not in own_page_types %}
   {{ obj_item.render()|indent(3) }}
      {% endif %}
   {% endfor %}
{% endif %} {# obj.display #}
