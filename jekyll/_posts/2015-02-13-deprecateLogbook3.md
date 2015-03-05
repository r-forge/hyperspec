---
layout: post
title: Deprecate logbook III
tags: news deprecated 
---

The next step has been taken to deprecate the loogbook (see the explanation in `vignette
("introduction")`).

Functions `logbook` and `logentry` have now been removed. 
The `@logbook` slot still exists but cannot be accessed any more by getter and setter functions. 
It should stay an empty `data.frame` from now on. 
