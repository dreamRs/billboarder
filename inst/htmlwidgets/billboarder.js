
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
        
        
        if (HTMLWidgets.shinyMode) {
          bb_opts.data.onclick = function(d, element) {
            Shiny.onInputChange(el.id + '_click', d);
          };
          bb_opts.data.onover = function(d, element) {
            Shiny.onInputChange(el.id + '_over', d);
          };
          bb_opts.data.onselected = function(d) {
            Shiny.onInputChange(el.id + '_selected', d);
          };
          bb_opts.data.onunselected = function(d) {
            Shiny.onInputChange(el.id + '_selected', d);
          };
          
          if (typeof bb_opts.zoom != 'undefined') {
            bb_opts.zoom.onzoom = function(domain) {
              Shiny.onInputChange(el.id + '_zoom', domain);
            };
          }
        }
        

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
  var bbObj ;
  
  if (typeof htmlWidgetsObj != 'undefined') {
    bbObj = htmlWidgetsObj.getChart();
  }

  return(bbObj);
}



// Shiny ----

if (HTMLWidgets.shinyMode) {
  
  // data = load
  Shiny.addCustomMessageHandler('update-billboard-data',
    function(data) {
      var chart = get_billboard(data.id);
      // chart.unload();
      chart.load(data.data);
  });
  
  // load
  Shiny.addCustomMessageHandler('update-billboard-load',
    function(data) {
      var chart = get_billboard(data.id);
      chart.load(data.data);
  });
  
  // unload
  Shiny.addCustomMessageHandler('update-billboard-unload',
    function(data) {
      var chart = get_billboard(data.id);
      chart.unload(data.data);
  });
  
  // focus
  Shiny.addCustomMessageHandler('update-billboard-focus',
    function(data) {
      var chart = get_billboard(data.id);
      if (data.data.ids.length > 0) {
        chart.focus(data.data.ids);
      } else {
        chart.focus();
      }
  });
  // focus
  Shiny.addCustomMessageHandler('update-billboard-defocus',
    function(data) {
      var chart = get_billboard(data.id);
      if (data.data.ids.length > 0) {
        chart.defocus(data.data.ids);
      } else {
        chart.defocus();
      }
  });
}


