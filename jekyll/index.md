---
layout: default
title: hyperSpec
---
## About `hyperSpec`

`hyperSpec` is an R package to handle hyperspectral data, i.e. spectra plus further information such
as spatial information, time, concentrations, etc.  
Such data are frequently encountered in Raman, IR, NIR, UV/VIS, NMR, mass spectroscopy, AAS / AES,
...  

`hyperSpec` acts as an interface for the convenient handling of the spectra, using the powerful
chemometric data analysis provided by R and other R packages.

## News
{% for post in site.posts limit:5%}
- {{ post.date | date_to_string }} &raquo; <a href="{{ post.url }}">{{ post.title }}</a>
{% endfor %}


### Excerpts
{% for post in site.posts limit:5 %}
{% include excerpt.md %}
{% endfor %}
