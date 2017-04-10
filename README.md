# charty

Native charts library for Elm

So... you have some data you want to visualize? We can help you!

This library aims to provide native (as in "no js interop needed") SVG charts for your Elm app.
Truth be told, for the moment only line charts are supported. That will change soon :-)

Please check out the [live demo](https://juanedi.github.io/charty/) and
[documentation](http://package.elm-lang.org/packages/juanedi/charty/1.0.0) to learn how to use it.

## API overview

Usage is pretty simple: use `LineChart.draw` as follows to generate SVG markup, which can be embedded in your `view` function:

```elm

sampleDataset : LineChart.Dataset
sampleDataset =
    [ [ ( 100000, 3 ), ( 100001, 4 ) ]
    , [ ( 100000, 1 ), ( 100001, 2.5 ) ]
    ]

view : Model -> Html Msg
view model =
    Html.div
      []
      [ Html.p [] [ Html.text "Wow!" ]
      , LineChart.view LineChart.defaults dataset
      ]
```

Of course, the dataset can be part of your model so that the chart is redrawn every time your data changes.
The `examples` directory shows this behaviour.
