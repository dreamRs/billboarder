billboarder 0.4.0
==================

* Update of the billboard.js library to 3.6.3, see the [release note](https://netil.medium.com/billboard-js-3-6-release-official-react-wrapper-new-enhancements-2dbf1ffc4d1c)
* Update of the billboard.js library to 3.5.1, see the [release note](https://netil.medium.com/billboard-js-3-5-release-new-boost-options-7c951e576076)
* Update of the billboard.js library to 3.4.0, see the [release note](https://netil.medium.com/billboard-js-3-4-release-new-polar-chart-83d604c8706c)
* Update of the billboard.js library to 3.2.0, see the [release note](https://netil.medium.com/billboard-js-3-2-release-sparkline-tableview-plugins-more-32217ddc869a)
* Update of the billboard.js library to 3.1.1, see the [release note](https://netil.medium.com/billboard-js-3-1-release-subchart-data-loading-enhancements-e98be5eebdcb)
* Fixed bug with export outside shiny.
* Fixed a bug in `bb_scatterplot()` when groups have length one.



billboarder 0.3.1
==================

* Update of the billboard.js library to 3.0.1, see the [release note](https://netil.medium.com/billboard-js-3-0-release-d3-js-v6-support-new-candlestick-type-9bd74af6a753)



billboarder 0.3.0
==================

* Update of the billboard.js library to 2.2.2, see the [release note](https://netil.medium.com/billboard-js-2-2-0-release-gauge-enhancements-pie-outerradius-more-407118914fbc)
* New theme available: datalab.
* New function `set_color_palette()` to define default colors used in charts.
* It's now possible to use `aes()` directly to define mapping of aesthetics.
* Deprecated `bb_proxy_transform()`.



billboarder 0.2.9
==================

* Update of the billboard.js library to 1.12.8, see the [release note](https://netil.medium.com/billboard-js-692f4db55b0b).
* `bb_labs()` lower part of caption was cut and added argument `caption_href` to add a link.



billboarder 0.2.8
==================

* Update of the billboard.js library to 1.11.1, see the [release note](https://netil.medium.com/billboard-js-1-11-0-release-lazy-render-new-textoverlap-plugin-more-831e33efcbe5).
* `bb_export()` : new function to export chart to PNG in Shiny applications with proxy or by adding a "download" button on the chart.
* `bb_linechart()` have two new arguments: `dasharray` (use dash in lines) and `width` to control lines size.
* New vignette to describe lines options, use `vignette("lines-options", package = "billboarder")` to display it.



billboarder 0.2.7
==================

* Upgrade billboard library to 1.10.0, see the [release note](https://netil.medium.com/billboard-js-1-10-release-bubble-dimension-axis-culling-more-132d343a46cc)
* Upgrade billboard library to 1.9.0, see the [release note](https://netil.medium.com/billboard-js-1-9-release-introducing-plugins-more-636ada3a7881)

## Breaking changes

* Bubbles (when using `size` aesthetic in `bb_scatterplot`) are now computed in JavaScript, argument `range` is deprecated.



billboarder 0.2.6
==================

* Upgrade billboard library to 1.8.0 (color on over, linear gradient for area charts, multiline text labels, bar width), see the [release note](https://netil.medium.com/billboard-js-1-8-0-released-today-3ed432f2d9d4)
* New function *bauge* : a gauge that automatically updates itself in Shiny apps.


billboarder 0.2.5
==================

* Upgrade billboard library to 1.7.0 (new theme, multiple axes, normalized staking data), see the [release note](https://netil.medium.com/billboard-js-1-7-0-release-4944a2eb59fe)



billboarder 0.2.4
==================

* Upgrade billboard library to 1.6.2
* New function `set_theme` to choose global theme for charts.



billboarder 0.2.3
==================

* Upgrade billboard library to 1.5.1
* Support for radar chart: `bb_radarchart`.
* Linked tooltip, see [this Gist](https://gist.github.com/pvictor/49fdb05d362acca8d6b94d69345a5046) for a demo.



billboarder 0.2.2
==================

* Upgrade billboard library to 1.4.1
* Support for `area-line-range` and `area-spline-range` in `bb_linechart`



billboarder 0.2.1
==================

* Upgrade billboard library to 1.2.0, and D3V4 to 4.12.2
* New proxy methods : `bb_proxy_data_colors` and `bb_proxy_data_names`.
* New aesthetic for scatterplots : `size`, to make bubble clouds.



billboarder 0.1.0
==================

* New charts type : lollipop, histogram and density plot.
* More methods for proxy and more examples, see `proxy_example` for more details.
* Upgrade to billboard.js 1.1.1.
* New functions `bbaes` and `bb_aes` to map data on chart, see the corresponding vignette.
