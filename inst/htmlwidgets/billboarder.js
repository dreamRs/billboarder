
// HTMLWidgets billboard ----

HTMLWidgets.widget({

  name: 'billboarder',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    var chart,
      bb_opts;

    return {

      renderValue: function(x) {

        bb_opts = x.bb_opts;
        // bindto element
        bb_opts.bindto = '#' + el.id;
        
        
        // Shiny interaction
        if (HTMLWidgets.shinyMode) {
          
          // Click
          if (typeof bb_opts.data.onclick == 'undefined') {
            bb_opts.data.onclick = function(d, element) {
              Shiny.onInputChange(el.id + '_click', d);
            };
          }
          
          // Hover
          if (typeof bb_opts.data.onover == 'undefined') {
            bb_opts.data.onover = function(d, element) {
              Shiny.onInputChange(el.id + '_over', d);
            };
          }
          
          // Selected
          if (typeof bb_opts.data.onselected == 'undefined') {
            bb_opts.data.onselected = function(d) {
              Shiny.onInputChange(el.id + '_selected', d);
            };
          }
          
          // Unselected
          if (typeof bb_opts.data.onunselected == 'undefined') {
            bb_opts.data.onunselected = function(d) {
              Shiny.onInputChange(el.id + '_selected', d);
            };
          }
          
          // Zoom
          if (typeof bb_opts.zoom != 'undefined') {
            if (typeof bb_opts.zoom.onzoom == 'undefined') {
              bb_opts.zoom.onzoom = function(domain) {
                Shiny.onInputChange(el.id + '_zoom', domain);
              };
            }
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
        
        
        // Caption
        if (typeof bb_opts.caption != 'undefined') {
          d3.select('#' + el.id + ' svg').selectAll(".bb-caption").remove();
          d3.select('#' + el.id + ' svg')
          .append("text")
          .attr("class", "bb-caption")
          .attr("x", w)
          .attr("y", h)
          //.attr("startOffset", "100%")
          .attr("text-anchor", "end")
          .text(bb_opts.caption.text);
        }
        
        // bold title
        //var sheet = window.document.styleSheets[0];
        //sheet.insertRule('.bb-title { font-weight: bold; }', sheet.cssRules.length);
        var css = '.bb-title { font-weight: bold; }',
          head = document.head || document.getElementsByTagName('head')[0],
          style = document.createElement('style');
        style.type = 'text/css';
        if (style.styleSheet){
          style.styleSheet.cssText = css;
        } else {
          style.appendChild(document.createTextNode(css));
        }
        head.appendChild(style);
        
        if (typeof bb_opts.billboarderspecials != 'undefined') {
          if (typeof bb_opts.billboarderspecials.opacity != 'undefined') {
            var cssopacity = '.bb-area { opacity: ' + bb_opts.billboarderspecials.opacity + ' !important; }',
              styleopacity = document.createElement('style');
            styleopacity.type = 'text/css';
            if (styleopacity.styleSheet){
              styleopacity.styleSheet.cssText = cssopacity;
            } else {
              styleopacity.appendChild(document.createTextNode(cssopacity));
            }
            head.appendChild(styleopacity);
          }
        }
        
        
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
        //console.log(h);
        chart.resize({width: w, height: h});
        
        // Caption
        if (typeof bb_opts.caption != 'undefined') {
          d3.select('#' + el.id + ' svg').selectAll(".bb-caption").remove();
          d3.select('#' + el.id + ' svg')
          .append("text")
          .attr("class", "bb-caption")
          .attr("x", w)
          .attr("y", h)
          .attr("startOffset", "100%")
          .attr("text-anchor", "end")
          .text(bb_opts.caption.text);
        }

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


