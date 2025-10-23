#!/usr/bin/env python3
# Configuration file for the Sphinx documentation builder.
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use pl.Path.resolve to make it absolute, like shown here.
#
import os
import sys
import pathlib as pl
local_mod = str(pl.Path('../').resolve())
sys.path.insert(0, local_mod)

# (Relative to this file):
templates_path   = ['_templates']
html_static_path = ['_static']

# Relative to static dir, or fully qualified urls
html_css_files = ["custom.css"]
html_js_files  = []
# html_style = "custom.css"

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = [
    '**/flycheck_*.py',
    "**/__tests/*",
    "_docs/_templates/*",
    "README.md",
]

root_doc = "index"
# -- Project information -----------------------------------------------------

project   = 'acab'
copyright = '2024, John Grey'
author    = 'John Grey'
release   = '0.1.1'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = [
    'sphinx.ext.doctest',
    # 'sphinx.ext.autodoc',
    # 'sphinx.ext.autosummary',
    'sphinx.ext.napoleon',
    'sphinx.ext.extlinks',
    "sphinx.ext.duration",
    'sphinx_rtd_theme',
    "sphinx.ext.graphviz",
    'myst_parser',
    "autoapi.extension",
    "sphinx.ext.coverage",
    "sphinx.ext.imgconverter",
    "sphinx.ext.intersphinx",
    "sphinx.ext.viewcode",
    ]
# -- Options for HTML output -------------------------------------------------
html_theme       = "sphinx_rtd_theme"

html_theme         = "sphinx_rtd_theme"
html_theme_options = {}
html_sidebars      = {}


html_theme_options.update({
    'logo_only'                   : False,
    'prev_next_buttons_location'  : 'bottom',
    'style_external_links'        : False,
    'vcs_pageview_mode'           : '',
    'style_nav_header_background' : 'grey',
    # TOC options:
    'collapse_navigation'         : True,
    'sticky_navigation'           : True,
    'navigation_depth'            : 4,
    'includehidden'               : True,
    'titles_only'                 : False

})

# -- Extension Options -------------------------------------------------
# https://sphinx-autoapi.readthedocs.io/en/latest/reference/config.html
autoapi_keep_files        = False
autoapi_generate_api_docs = True
autoapi_add_toctree_entry = False
autoapi_type              = "python"
autoapi_template_dir      = "_docs/_templates/autoapi"
autoapi_root              = "_docs/autoapi"
autoapi_dirs              = ['.']
autoapi_file_patterns     = ["*.py", "*.pyi"]
autoapi_ignore            = [*exclude_patterns, "*_docs/conf.py"]
autoapi_member_order      = "groupwise"
autoapi_options           = [
    # 'imported-members',
    # "inherited-members",
    # 'show-inheritance-diagram',
    'members',
    'undoc-members',
    'private-members',
    'special_members',
    'show-inheritance',
    'show-module-summary',
]

def filter_contains(val:list|str, *needles:str) -> bool:
    match val:
        case str():
            return any(x in val for x in needles)
        case list():
            joined = " ".join(val)
            return any(x in joined for x in needles)
        case _:
            return False

def autoapi_prepare_jinja_env(jinja_env: jinja2.Environment) -> None:
    jinja_env.add_extension("jinja2.ext.debug")
    jinja_env.tests['contains'] = filter_contains
