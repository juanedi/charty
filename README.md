# charty

Native charts library for Elm

So... you have some data you want to visualize? We can help you!

This library aims to provide native (as in "no js interop needed") SVG charts for your Elm app.
Truth be told, for the moment only line and pie charts are supported. That will change soon :-)

Please check out the [live demo](https://juanedi.github.io/charty/) and
[documentation](http://package.elm-lang.org/packages/juanedi/charty/latest) to learn how to use it.

## API overview

Usage is pretty simple. The library exposes view-only functions, so there is no need to modify your application's model to keep track of the chart's internal state (there isn't one).

For exampe, you can use `LineChart.draw` as follows to generate SVG markup, which can be embedded in your `view` function:

```elm

sampleDataset : LineChart.Dataset
sampleDataset =
    [ { label = "Series 1"
      , data = [ ( 100000, 3 ), ( 100001, 4 ), ( 100002, 3 ) ]
      }
    , { label = "Series 2"
      , data = [ ( 100000, 1 ), ( 100001, 2.5 ), ( 100002, 3 ) ]
      }
    ]


view : Model -> Html Msg
view model =
    Html.div
      []
      [ Html.p [] [ Html.text "Wow!" ]
      , LineChart.view LineChart.defaults dataset
      ]
```

Of course, the dataset itself can be part of your model so that the chart is redrawn every time your data changes.
The `examples` directory shows this behaviour.

Please read the docs for detailed usage instructions of other charts too.
