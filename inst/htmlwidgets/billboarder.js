
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

        //console.log(x.bb_opts);
        if (typeof x.bb_opts.data == 'undefined') {
          bb_opts = x.bb_empty;
        } else {
          bb_opts = x.bb_opts;
        }
        
        // bindto element
        bb_opts.bindto = '#' + el.id;
        
        
        // Shiny interaction
        if (HTMLWidgets.shinyMode) {
          
          // Click
          if (typeof bb_opts.data.onclick == 'undefined') {
            bb_opts.data.onclick = function(d, element) {
              var chartclick = get_billboard(el.id);
              //console.log(chartclick.categories());
              d.category = chartclick.categories()[d.index];
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
        

        
        // Custom legend .contents.templat
        if (typeof bb_opts.legend !== 'undefined') {
          if (typeof bb_opts.legend.contents !== 'undefined') {
            if (typeof bb_opts.legend.contents.template !== 'undefined') {
              //var custom_legend = document.createElement("div");
              //custom_legend.setAttribute("id", el.id + "_custom_legend");
              //document.getElementById(el.id)
    		      //  .insertAdjacentElement("beforeend", custom_legend);
    		      //bb_opts.legend.contents.bindto = "#" + el.id + "_custom_legend";
            }
          }
        }

        // Generate billboard chart
        chart = bb.generate(bb_opts);
        
        // 

        
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
        
        // Billboarder specials
        if (typeof bb_opts.billboarderspecials != 'undefined') {
          if (typeof bb_opts.billboarderspecials.opacity != 'undefined') {
            var cssopacity = '#' + el.id + ' .bb-area { opacity: ' + bb_opts.billboarderspecials.opacity + ' !important; }',
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
        
        // Custom style
        if (typeof bb_opts.customstyle != 'undefined') {
          if (typeof bb_opts.customstyle.custom_style != 'undefined') {
            var customcss = bb_opts.customstyle.custom_style,
              stylecustom = document.createElement('style');
            if (Array.isArray(customcss)) {
              customcss = customcss.map(function(x) {return '#' + el.id + " " + x}).join(" ");
            } else {
              customcss = '#' + el.id + " " + customcss;
            }
            //console.log(customcss);
            stylecustom.type = 'text/css';
            if (stylecustom.styleSheet){
              stylecustom.styleSheet.cssText = customcss;
            } else {
              stylecustom.appendChild(document.createTextNode(customcss));
            }
            head.appendChild(stylecustom);
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
  
  // Use the getChart method we created to get the underlying billboard chart
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
      if (typeof chart != 'undefined') {
        // chart.unload();
        chart.load(data.data);
      }
  });
  
  // load
  Shiny.addCustomMessageHandler('update-billboard-load',
    function(data) {
      var chart = get_billboard(data.id);
      if (typeof chart != 'undefined') {
        //console.log(data.data);
        chart.load(data.data);
      }
  });
  
  // unload (not used)
  Shiny.addCustomMessageHandler('update-billboard-unload',
    function(data) {
      //var chart = get_billboard(data.id);
      //var d = data.data;
      //console.log(isEmpty(d));
      //if (!isEmpty(d)) {
      //  chart.unload(d);
      //} else {
      //  chart.unload();
      //}
  });
  
  // focus
  Shiny.addCustomMessageHandler('update-billboard-focus',
    function(data) {
      var chart = get_billboard(data.id);
      if (typeof chart != 'undefined') {
        if (data.data.ids.length > 0) {
          chart.focus(data.data.ids);
        } else {
          chart.focus();
        }
      }
  });
  // defocus
  Shiny.addCustomMessageHandler('update-billboard-defocus',
    function(data) {
      var chart = get_billboard(data.id);
      if (typeof chart != 'undefined') {
        if (data.data.ids.length > 0) {
          chart.defocus(data.data.ids);
        } else {
          chart.defocus();
        }
      }
  });
  // Axis labels
  Shiny.addCustomMessageHandler('update-billboard-axis_labels',
    function(data) {
      var chart = get_billboard(data.id);
      if (typeof chart != 'undefined') {
        chart.axis.labels(data.data);
      }
  });
  // X values
  Shiny.addCustomMessageHandler('update-billboard-xs',
    function(data) {
      var chart = get_billboard(data.id);
      if (typeof chart != 'undefined') {
        chart.xs(data.data);
      }
  });
  // categories
  Shiny.addCustomMessageHandler('update-billboard-categories',
    function(data) {
      var chart = get_billboard(data.id);
      //console.log(data.data);
      if (typeof chart != 'undefined') {
        chart.categories(data.data[0]);
      }
  });
  // Transform / change chart type
  Shiny.addCustomMessageHandler('update-billboard-transform',
    function(data) {
      var chart = get_billboard(data.id);
      // console.log(data.data.type);
      if (typeof chart != 'undefined') {
        chart.transform(data.data.type, data.data.targetIds);
      }
  });
  // Regions
  Shiny.addCustomMessageHandler('update-billboard-region',
    function(data) {
      var chart = get_billboard(data.id);
      // console.log(data.data.type);
      if (typeof chart != 'undefined') {
        chart.regions(data.data);
      }
  });
  // Groups
  Shiny.addCustomMessageHandler('update-billboard-groups',
    function(data) {
      var chart = get_billboard(data.id);
      // console.log(data.data.type);
      if (typeof chart != 'undefined') {
        chart.groups(data.data);
      }
  });
  // Show legend
  Shiny.addCustomMessageHandler('update-billboard-legend-show',
    function(data) {
      var chart = get_billboard(data.id);
      // console.log(data.data.type);
      if (typeof chart != 'undefined') {
        console.log(data.data);
        if (data.data.targetIds !== null) {
          chart.legend.show(data.data.targetIds);
        } else {
          chart.legend.show();
        }
      }
  });
  // Hide legend
  Shiny.addCustomMessageHandler('update-billboard-legend-hide',
    function(data) {
      var chart = get_billboard(data.id);
      // console.log(data.data.type);
      if (typeof chart != 'undefined') {
        //console.log(data.data);
        if (data.data.targetIds !== null) {
          chart.legend.hide(data.data.targetIds);
        } else {
          chart.legend.hide();
        }
      }
  });
  // Show tooltip
  Shiny.addCustomMessageHandler('update-billboard-tooltip-show',
    function(data) {
      var chart = get_billboard(data.id);
      // console.log(data.data.type);
      if (typeof chart != 'undefined') {
        console.log(data.data);
        chart.tooltip.show(data.data);
      }
  });
  // Hide tooltip
  Shiny.addCustomMessageHandler('update-billboard-tooltip-hide',
    function(data) {
      var chart = get_billboard(data.id);
      // console.log(data.data.type);
      if (typeof chart != 'undefined') {
        chart.tooltip.hide();
      }
  });
  // Hide
  Shiny.addCustomMessageHandler('update-billboard-hide',
    function(data) {
      var chart = get_billboard(data.id);
      // console.log(data.data.type);
      if (typeof chart != 'undefined') {
        chart.hide(data.data.targetIdsValue, data.data.options);
      }
  });
  // Show
  Shiny.addCustomMessageHandler('update-billboard-show',
    function(data) {
      var chart = get_billboard(data.id);
      // console.log(data.data.type);
      if (typeof chart != 'undefined') {
        chart.show(data.data.targetIdsValue, data.data.options);
      }
  });
  // Data names
  Shiny.addCustomMessageHandler('update-billboard-data-names',
    function(data) {
      var chart = get_billboard(data.id);
      // console.log(data.data.type);
      if (typeof chart != 'undefined') {
        chart.data.names(data.data.names);
      }
  });
  // Data colors
  Shiny.addCustomMessageHandler('update-billboard-data-colors',
    function(data) {
      var chart = get_billboard(data.id);
      // console.log(data.data.type);
      if (typeof chart != 'undefined') {
        chart.data.colors(data.data.colors);
      }
  });
  Shiny.addCustomMessageHandler('update-billboard-flow',
    function(data) {
      var chart = get_billboard(data.id);
      // console.log(data.data.type);
      if (typeof chart != 'undefined') {
        chart.flow(data.data);
      }
  });
  // Export
  Shiny.addCustomMessageHandler('update-billboard-export',
    function(data) {
      var chart = get_billboard(data.id);
      // console.log(data.id);
      if (typeof chart != 'undefined') {
        var dataUrl = chart.export("image/png");
        var link = document.createElement("a");
        console.log(dataUrl);
        link.download = "export.png";
        link.href = dataUrl;
        link.target = "_blank";
        link.innerHTML = "Download chart as image";
      
        document.body.appendChild(link);
      }
  });
}



function bubblez(d) {
  console.log(d); 
  return Math.sqrt(d.value * 2);
}





// Utils -----

var hasOwnProperty = Object.prototype.hasOwnProperty;

function isEmpty(obj) {

    // null and undefined are "empty"
    if (obj === null) return true;

    // Assume if it has a length property with a non-zero value
    // that that property is correct.
    if (obj.length > 0)    return false;
    if (obj.length === 0)  return true;

    // If it isn't an object at this point
    // it is empty, but it can't be anything *but* empty
    // Is it empty?  Depends on your application.
    if (typeof obj !== "object") return true;

    // Otherwise, does it have any properties of its own?
    // Note that this doesn't handle
    // toString and valueOf enumeration bugs in IE < 9
    for (var key in obj) {
        if (hasOwnProperty.call(obj, key)) return false;
    }

    return true;
}

