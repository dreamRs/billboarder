
// HTMLWidgets billboard ----

HTMLWidgets.widget({

  name: 'billboarder',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    var chart;

    return {

      renderValue: function(x) {

        bb_opts = x.bb_opts;
        // bindto element
        bb_opts.bindto = '#' + el.id;

        // Sizing
        var elpar = document.getElementById(el.id); //.parentElement
        var w = elpar.clientWidth;
        var h = elpar.clientHeight;
        bb_opts.size = {};
        bb_opts.size.width = w;
        bb_opts.size.height = h;

        // Generate billboard chart
        chart = bb.generate(bb_opts);
        
        // bold title
        var sheet = window.document.styleSheets[0];
        sheet.insertRule('.bb-title { font-weight: bold; }', sheet.cssRules.length);
        
        
        //console.log(chart.data());

      },
      
      getChart: function(){
        return chart;
      },

      resize: function(width, height) {

        // code to re-render the widget with a new size
        var elpar = document.getElementById(el.id);  //.parentElement
        var w = elpar.clientWidth;
        var h = elpar.clientHeight;
        console.log(h);
        chart.resize({width: w, height: h});

      }

    };
  }
});


// From Friss tuto (https://github.com/FrissAnalytics/shinyJsTutorials/blob/master/tutorials/tutorial_03.Rmd)
function get_billboard(id){
  
  // Get the HTMLWidgets object
  var htmlWidgetsObj = HTMLWidgets.find("#" + id);
  
  // Use the getChart method we created to get the underlying C3 chart
  var bbObj = htmlWidgetsObj.getChart();

  return(bbObj);
}



// Shiny ----

if (HTMLWidgets.shinyMode) {
  
  // load
  Shiny.addCustomMessageHandler('update-billboard-data',
    function(data) {
      var chart = get_billboard(data.id);
      // chart.unload();
      chart.load(data.data);
  });
  
}


