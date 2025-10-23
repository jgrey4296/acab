{% if obj.display %}
   {% if is_own_page %}
{{ obj.id }}
{{ "=" * obj.id | length }}

   {% endif %}
{% block method %}
   {% set method_name %}
   {% if is_own_page %}{{obj.id}}{% else %}{{obj.short_name}}{% endif %}
   {% endset %}
   {% set typevars %}
   {% if obj.typevars %}[{{obj.typevars}}]{% endif %}
   {% endset %}
   
.. py:method:: {{method_name}}{{typevars}}({{ obj.args }}) -> {{ obj.return_annotation or "<Unknown>"}}
   {% for (args, return_annotation) in obj.overloads %}
               {%+ if is_own_page %}{{ obj.id }}{% else %}{{ obj.short_name }}{% endif %}({{ args }}){% if return_annotation is not none %} -> {{ return_annotation }}{% endif %}
               
   {% endfor %}
   {% for property in obj.properties %}
   :{{ property }}:
   {% endfor %}
   
   {% if obj.docstring %}

   {{ obj.docstring|indent(3) }}
   {% endif %}

{% endblock method %}
{% endif %}
