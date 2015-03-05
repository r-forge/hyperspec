---
layout: post
title: Deprecate logbook
tags: news deprecated fileio
---

The next step has been taken to deprecate the loogbook (see the explanation in `vignette
("introduction")`).

From now on, `new ("hyperSpec", ...)` yields a warning if data for the logbook is handed over, and
the file import functions do not create logbook entries by default any more.
