# hs-vexflow

## Dedication

> "A painter paints pictures on canvas. But musicians paint their pictures on
> silence."
>
> ― Leopold Stokowski

Personally, I don't see why we can't do both.


## Overview

`hs-vexflow` is a few things:

* A high-level DSL for describing western music.
* A little language that parses into that DSL, for when you want to write music
    but don't have Haskell handy. Maybe if you're writing [blog posts](https;//everygoodboydeservesfruit.com).
* A set of rendering primitives to turn the DSL into
    [vexflow](http://www.vexflow.com/)-compatible Javascript.

It aims to supplant [vextab](http://vexflow.com/vextab/), because vextab is
clearly written by an insane person.


## Example

```music
treble A 4/4 Cm7[0,1,2,30:8,30:8]:4 %:2 Bb Bbb
```




