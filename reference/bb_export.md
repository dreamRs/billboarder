# Export a Billboard to PNG

Export a Billboard to PNG

## Usage

``` r
bb_export(bb, filename = NULL, download_label = "Export (.png)", ...)
```

## Arguments

- bb:

  A
  [`billboarder`](https://dreamrs.github.io/billboarder/reference/billboarder.md)
  `htmlwidget` object or a
  [`billboarderProxy`](https://dreamrs.github.io/billboarder/reference/billboarder-shiny.md)
  `htmlwidget` object.

- filename:

  A string of the filename, excluding extension (will be `".png"`).

- download_label:

  Label to appear on the link to download PNG.

- ...:

  Additional arguments (not used).

## Value

A `billboard` `htmlwidget` object.

## Note

This function has two uses:

- **in shiny:** you can export to PNG with an `observeEvent` by using
  [`billboarderProxy`](https://dreamrs.github.io/billboarder/reference/billboarder-shiny.md).

- **in markdown and in shiny:** add a button to download chart as PNG.

## Examples

``` r
# Add a button to download as PNG:

data("equilibre_mensuel")
billboarder() %>% 
  bb_linechart(
    data = equilibre_mensuel,
    mapping = bbaes(date, solde),
    type = "spline"
  ) %>% 
  bb_x_axis(
    tick = list(format = "%Y-%m", fit = FALSE)
  ) %>% 
  bb_export(
    filename = "my-awesome-chart",
    download_label = "Click to download"
  )

  
  

{"x":{"bb_opts":{"interaction":{"inputType":{"touch":false}},"axis":{"x":{"type":"timeseries","tick":{"format":"%Y-%m","fit":false}}},"line":{"classes":["billboarder-line-solde"]},"data":{"x":"date","json":{"date":["2007-01-01","2007-02-01","2007-03-01","2007-04-01","2007-05-01","2007-06-01","2007-07-01","2007-08-01","2007-09-01","2007-10-01","2007-11-01","2007-12-01","2008-01-01","2008-02-01","2008-03-01","2008-04-01","2008-05-01","2008-06-01","2008-07-01","2008-08-01","2008-09-01","2008-10-01","2008-11-01","2008-12-01","2009-01-01","2009-02-01","2009-03-01","2009-04-01","2009-05-01","2009-06-01","2009-07-01","2009-08-01","2009-09-01","2009-10-01","2009-11-01","2009-12-01","2010-01-01","2010-02-01","2010-03-01","2010-04-01","2010-05-01","2010-06-01","2010-07-01","2010-08-01","2010-09-01","2010-10-01","2010-11-01","2010-12-01","2011-01-01","2011-02-01","2011-03-01","2011-04-01","2011-05-01","2011-06-01","2011-07-01","2011-08-01","2011-09-01","2011-10-01","2011-11-01","2011-12-01","2012-01-01","2012-02-01","2012-03-01","2012-04-01","2012-05-01","2012-06-01","2012-07-01","2012-08-01","2012-09-01","2012-10-01","2012-11-01","2012-12-01","2013-01-01","2013-02-01","2013-03-01","2013-04-01","2013-05-01","2013-06-01","2013-07-01","2013-08-01","2013-09-01","2013-10-01","2013-11-01","2013-12-01","2014-01-01","2014-02-01","2014-03-01","2014-04-01","2014-05-01","2014-06-01","2014-07-01","2014-08-01","2014-09-01","2014-10-01","2014-11-01","2014-12-01","2015-01-01","2015-02-01","2015-03-01","2015-04-01","2015-05-01","2015-06-01","2015-07-01","2015-08-01","2015-09-01","2015-10-01","2015-11-01","2015-12-01","2016-01-01","2016-02-01","2016-03-01","2016-04-01","2016-05-01","2016-06-01","2016-07-01","2016-08-01","2016-09-01","2016-10-01","2016-11-01","2016-12-01","2017-01-01","2017-02-01","2017-03-01","2017-04-01","2017-05-01","2017-06-01"],"solde":[4798,5102,5560,4610,5634,6739,5756,7089,5283,3924,601,1649,4300,4302,3021,3764,5798,5160,4746,5510,3683,1448,3429,2839,2689,3364,4435,2739,4535,1931,1318,2051,1245,-438,1090,770,-398,1128,2134,2526,2130,5281,4591,5888,3598,553,2293,788,3726,3945,4623,5060,5327,6038,6133,5325,4242,3815,3841,4799,5485,-476,4997,3506,4531,3719,3250,3611,4105,3701,4423,4462,3521,2148,1805,3285,5885,5753,5550,6226,4877,4092,2585,2734,5842,4298,4535,3996,6454,6126,8120,7754,5315,4306,5788,4754,5130,3008,4330,4083,5981,6039,6262,6833,5141,4884,5925,6280,5078,4942,4242,3688,5283,6643,4259,3378,2711,759,629,32,-735,3536,6372,5351,5882,3456]},"types":{"solde":"spline"}},"point":{"show":false},"export":{"filename":"my-awesome-chart","download_label":"Click to download"}},"data":null},"evals":[],"jsHooks":[]}  

# In shiny, you can use proxy :

if (interactive()) {
  library(shiny)
  library(billboarder)
  
  ui <- fluidPage(
    fluidRow(
      column(
        width = 8, offset = 2,
        tags$h1("Export billboard as PNG via Proxy"),
        billboarderOutput(outputId = "mybb"),
        actionButton(
          inputId = "export", 
          label = "Export", 
          icon = icon("download")
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    output$mybb <- renderBillboarder({
      data("prod_par_filiere")
      billboarder() %>%
        bb_barchart(
          data = prod_par_filiere[, c("annee", "prod_hydraulique")],
          color = "#102246"
        ) %>%
        bb_y_grid(show = TRUE)
    })
    
    observeEvent(input$export, {
      billboarderProxy(shinyId = "mybb") %>% 
        bb_export(filename = "my-billboard-chart")
    })
    
  }
  
  shinyApp(ui, server)
}
```
