---
title: "Loading Direct Draw Surface (.dds) Files in Haskell"
date: 05 June 2019
description: "Just a hint to an obscure portion of the friday library that supports loading and exporting of Microsoft .dds files."
tags:
- haskell
- graphics
---

A short post for today.

I was on the verge of implementing my own DDS library in Haskell after numerous unfruitful searches. Alas, I came across a somewhat obscure portion of the `friday` library on Hackage and found out that DDS _is_ a supported image type thanks to bindings to the underlying DevIL C library.

[Here's the relevant module for your convenience](https://hackage.haskell.org/package/friday-0.1/docs/Vision-Image-Storage.html){target="_blank"}.

Some of you may be asking, why would one need to load DDS files in Haskell? Although OpenGL, and therefore non-DDS image formats, align more with the spirit of Haskell, it is still useful to load DDS for say, a texture extractor/parser/editor for some old game, or, in my case, Korean MMOs, where its use is still rampant to this day.
