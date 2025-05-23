# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
import os
import sys

sys.path.insert(0, os.path.abspath("src"))


# -- Project information -----------------------------------------------------

project = "Halligame"
# copyright = '2025, Cole Glotfelty, Michael Daniels, Will Cordray'
author = "Cole Glotfelty, Michael Daniels, Will Cordray"

html_show_copyright = False


# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    "sphinx.ext.autodoc",
    "sphinx.ext.napoleon",
    "myst_parser",
    "sphinxarg.ext",
]

# Add any paths that contain templates here, relative to this directory.
# templates_path = ['../../_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = [
    "../../_build",
    "../../src/*/_build",
    "../../src/*/_build/*",
    "Thumbs.db",
    ".DS_Store",
    "../../.venv",
    "../../vendor",
]


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
# html_theme = 'alabaster'
html_theme = "furo"

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
# html_static_path = ['../../_static']

autodoc_member_order = "groupwise"

# autodoc_type_aliases = {
# }

autodoc_typehints = "both"

autodoc_default_options = {"private-members": True}
# autodoc_typehints = 'description'

napoleon_include_private_with_doc = True
napoleon_include_special_with_doc = True
