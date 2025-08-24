{# module.rst -*- mode: Jinja2 -*- #}
{% import "python/debug_obj.rst.jinja" as jgdb %}
{% import "python/utils.jinja" as utils %} 

{% if obj.display %}
   {% set visible_children   = obj.children|selectattr("display")|list %} 
   {% set mod_ns = namespace(children=[]) %} 
      
   {{- utils.smartref(obj) }}
   
   {{ utils.heading(obj.id, above=True) }}
   
.. py:module:: {{ obj.name }}

   {% if obj.docstring -%}
.. autoapi-nested-parse::

   {{ obj.docstring|indent(3)}}
   {% endif %} {# obj.docstring #}
      
   {# SUBMODULES #}
   {% block submodules -%} 
   {%+ include "python/vis_submods.rst.jinja" %}   
   {% endblock submodules %} 
   {# -------------------------------------------------- #}

   {% block summary -%}
      {% if visible_children %}
            {% set visible_data = visible_children|selectattr("type", "equalto", "data")|list %} 
            {% set visible_classes = visible_children|selectattr("type", "equalto", "class")|list %}
            {% include "python/vis_types.rst.jinja" %}        
            {% include "python/vis_enums.rst.jinja" %}          
            {% include "python/vis_protos.rst.jinja" %}          
            {# {% include "python/vis_attrs.rst.jinja" %}            #}
            {% include "python/vis_excs.rst.jinja" %}            
            {% include "python/vis_funcs.rst.jinja" %}           
            {% include "python/vis_classes.rst.jinja" %}         
      {% endif %}
   {% endblock summary %} 
      
   {# -------------------------------------------------- #}
   {% if mod_ns.children %} 
{{ obj.type|title }} Contents
{{ "=" * obj.type|length }}=========

      {% for obj_item in mod_ns.children %} 
{{ obj_item.render()|indent(0) }}
      {% endfor %}
   {% endif %} 
   
{% endif %}
