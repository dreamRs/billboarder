// HTMLWidgets billboard ----

var HTMLWidgets = window.HTMLWidgets || {};
var bb = window.bb || {};
var d3 = window.d3 || {};

HTMLWidgets.widget({
  name: "billboarder",

  type: "output",

  factory: function(el, width, height) {

    var chart, bb_opts;

    return {
      renderValue: function(x) {

        if (typeof x.bb_opts.data == "undefined") {
          bb_opts = x.bb_empty;
        } else {
          bb_opts = x.bb_opts;
        }

        // bindto element
        bb_opts.bindto = "#" + el.id;

        // Shiny interaction
        if (HTMLWidgets.shinyMode) {
          var Shiny = window.Shiny || {};

          // Click
          if (typeof bb_opts.data.onclick == "undefined") {
            bb_opts.data.onclick = function(d, element) {
              var chartclick = get_billboard(el.id);
              //console.log(chartclick.categories());
              d.category = chartclick.categories()[d.index];
              Shiny.onInputChange(el.id + "_click", d);
            };
          }

          // Hover
          if (typeof bb_opts.data.onover == "undefined") {
            bb_opts.data.onover = function(d, element) {
              Shiny.onInputChange(el.id + "_over", d);
            };
          }

          // Selected
          if (typeof bb_opts.data.onselected == "undefined") {
            bb_opts.data.onselected = function(d) {
              Shiny.onInputChange(el.id + "_selected", d);
            };
          }

          // Unselected
          if (typeof bb_opts.data.onunselected == "undefined") {
            bb_opts.data.onunselected = function(d) {
              Shiny.onInputChange(el.id + "_selected", d);
            };
          }

          // Zoom
          if (typeof bb_opts.zoom != "undefined") {
            if (typeof bb_opts.zoom.onzoom == "undefined") {
              bb_opts.zoom.onzoom = function(domain) {
                Shiny.onInputChange(el.id + "_zoom", domain);
              };
            }
          }
        }

        // Sizing
        var w = el.clientWidth;
        var h = el.clientHeight;
        bb_opts.size = {};
        bb_opts.size.width = w;
        bb_opts.size.height = h;

        // Custom legend .contents.templat
        if (typeof bb_opts.legend !== "undefined") {
          if (typeof bb_opts.legend.contents !== "undefined") {
            if (typeof bb_opts.legend.contents.template !== "undefined") {
              //var custom_legend = document.createElement("div");
              //custom_legend.setAttribute("id", el.id + "_custom_legend");
              //document.getElementById(el.id)
              //  .insertAdjacentElement("beforeend", custom_legend);
              //bb_opts.legend.contents.bindto = "#" + el.id + "_custom_legend";
            }
          }
        }
        
        if (typeof bb_opts.export !== "undefined") {
          bb_opts.onrendered = function(ctx) {
            setTimeout(function() {
              ctx.export("image/png", function(dataUrl) {
                var link = document.getElementById(el.id + "-export");
                if (typeof bb_opts.export.filename !== "undefined") {
                  link.download = bb_opts.export.filename + ".png";
                } else {
                  link.download = `export-${Date.now()}.png`;
                }
                link.innerHTML = bb_opts.export.download_label;
                link.href = dataUrl;
                link.style.display = "inline-block";
                //if (HTMLWidgets.shinyMode) {
                //  Shiny.onInputChange("export_bb", dataUrl);
                //}
              });
            }, 300);
          };
        }



        // Generate billboard chart
        chart = bb.generate(bb_opts);


        // bold title
        //var sheet = window.document.styleSheets[0];
        //sheet.insertRule('.bb-title { font-weight: bold; }', sheet.cssRules.length);
        var css = ".bb-title { font-weight: bold; }",
          head = document.head || document.getElementsByTagName("head")[0],
          style = document.createElement("style");
        style.type = "text/css";
        if (style.styleSheet) {
          style.styleSheet.cssText = css;
        } else {
          style.appendChild(document.createTextNode(css));
        }
        head.appendChild(style);

        // Billboarder specials
        if (typeof bb_opts.billboarderspecials != "undefined") {
          if (typeof bb_opts.billboarderspecials.opacity != "undefined") {
            var cssopacity =
                "#" +
                el.id +
                " .bb-area { opacity: " +
                bb_opts.billboarderspecials.opacity +
                " !important; }",
              styleopacity = document.createElement("style");
            styleopacity.type = "text/css";
            if (styleopacity.styleSheet) {
              styleopacity.styleSheet.cssText = cssopacity;
            } else {
              styleopacity.appendChild(document.createTextNode(cssopacity));
            }
            head.appendChild(styleopacity);
          }
        }

        // Custom style
        if (typeof bb_opts.customStyle != "undefined") {
            var customcss = bb_opts.customStyle,
              stylecustom = document.createElement("style");
            if (Array.isArray(customcss)) {
              customcss = customcss
                .map(function(x) {
                  return "#" + el.id + " " + x;
                })
                .join(" ");
            } else {
              customcss = "#" + el.id + " " + customcss;
            }
            //console.log(customcss);
            stylecustom.type = "text/css";
            if (stylecustom.styleSheet) {
              stylecustom.styleSheet.cssText = customcss;
            } else {
              stylecustom.appendChild(document.createTextNode(customcss));
            }
            head.appendChild(stylecustom);
        }
        
        // Caption
        if (typeof bb_opts.caption != "undefined") {
          d3.select("#" + el.id + " svg")
              .selectAll(".bb-caption")
              .remove();
          d3.select("#" + el.id + " svg")
              .append("text")
              .attr("class", "bb-caption")
              .attr("x", w)
              .attr("y", h)
              //.attr("startOffset", "100%")
              .attr("text-anchor", "end")
              .text(bb_opts.caption.text);
        }
        
        
      },

      getChart: function() {
        return chart;
      },

      resize: function(width, height) {
        var container = document.getElementById(el.id); 
        if (container) {
          // code to re-render the widget with a new size	
          var w = container.clientWidth;	
          var h = container.clientHeight;	
          chart.resize({ width: w, height: h });	
  
          // Caption	
          if (typeof bb_opts.caption != "undefined") {	
            d3.select("#" + el.id + " svg")	
              .selectAll(".bb-caption")	
              .remove();	
            d3.select("#" + el.id + " svg")	
              .append("text")	
              .attr("class", "bb-caption")	
              .attr("x", w)	
              .attr("y", h)	
              .attr("startOffset", "100%")	
              .attr("text-anchor", "end")	
              .text(bb_opts.caption.text);	
          }
        }
      }
    };
  }
});

// From Friss tuto (https://github.com/FrissAnalytics/shinyJsTutorials/blob/master/tutorials/tutorial_03.Rmd)
function get_billboard(id) {
  // Get the HTMLWidgets object
  var htmlWidgetsObj = HTMLWidgets.find("#" + id);

  // Use the getChart method we created to get the underlying billboard chart
  var bbObj;

  if (typeof htmlWidgetsObj != "undefined") {
    bbObj = htmlWidgetsObj.getChart();
  }

  return bbObj;
}

// Shiny ----

if (HTMLWidgets.shinyMode) {
  var Shiny = window.Shiny || {};

  // data = load
  Shiny.addCustomMessageHandler("update-billboard-data", function(message) {
    var chart = get_billboard(message.id);
    if (typeof chart != "undefined") {
      // chart.unload();
      chart.load(message.data);
    }
  });

  // load
  Shiny.addCustomMessageHandler("update-billboard-load", function(message) {
    var chart = get_billboard(message.id);
    if (typeof chart != "undefined") {
      chart.load(message.data);
    }
  });

  // unload (not used)
  Shiny.addCustomMessageHandler("update-billboard-unload", function(message) {
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
  Shiny.addCustomMessageHandler("update-billboard-focus", function(message) {
    var chart = get_billboard(message.id);
    if (typeof chart != "undefined") {
      if (message.data.ids.length > 0) {
        chart.focus(message.data.ids);
      } else {
        chart.focus();
      }
    }
  });
  // defocus
  Shiny.addCustomMessageHandler("update-billboard-defocus", function(message) {
    var chart = get_billboard(message.id);
    if (typeof chart != "undefined") {
      if (message.data.ids.length > 0) {
        chart.defocus(message.data.ids);
      } else {
        chart.defocus();
      }
    }
  });
  // Axis labels
  Shiny.addCustomMessageHandler("update-billboard-axis_labels", function(
    message
  ) {
    var chart = get_billboard(message.id);
    if (typeof chart != "undefined") {
      chart.axis.labels(message.data);
    }
  });
  // X values
  Shiny.addCustomMessageHandler("update-billboard-xs", function(message) {
    var chart = get_billboard(message.id);
    if (typeof chart != "undefined") {
      chart.xs(message.data);
    }
  });
  // categories
  Shiny.addCustomMessageHandler("update-billboard-categories", function(
    message
  ) {
    var chart = get_billboard(message.id);
    if (typeof chart != "undefined") {
      chart.categories(message.data[0]);
    }
  });
  // Transform / change chart type
  Shiny.addCustomMessageHandler("update-billboard-transform", function(
    message
  ) {
    var chart = get_billboard(message.id);
    if (typeof chart != "undefined") {
      chart.transform(message.data.type, message.data.targetIds);
    }
  });
  // Regions
  Shiny.addCustomMessageHandler("update-billboard-region", function(message) {
    var chart = get_billboard(message.id);
    if (typeof chart != "undefined") {
      chart.regions(message.data);
    }
  });
  // Groups
  Shiny.addCustomMessageHandler("update-billboard-groups", function(message) {
    var chart = get_billboard(message.id);
    if (typeof chart != "undefined") {
      chart.groups(message.data);
    }
  });
  // Show legend
  Shiny.addCustomMessageHandler("update-billboard-legend-show", function(
    message
  ) {
    var chart = get_billboard(message.id);
    if (typeof chart != "undefined") {
      if (message.data.targetIds !== null) {
        chart.legend.show(message.data.targetIds);
      } else {
        chart.legend.show();
      }
    }
  });
  // Hide legend
  Shiny.addCustomMessageHandler("update-billboard-legend-hide", function(
    message
  ) {
    var chart = get_billboard(message.id);
    if (typeof chart != "undefined") {
      if (message.data.targetIds !== null) {
        chart.legend.hide(message.data.targetIds);
      } else {
        chart.legend.hide();
      }
    }
  });
  // Show tooltip
  Shiny.addCustomMessageHandler("update-billboard-tooltip-show", function(
    message
  ) {
    var chart = get_billboard(message.id);
    if (typeof chart != "undefined") {
      chart.tooltip.show(message.data);
    }
  });
  // Hide tooltip
  Shiny.addCustomMessageHandler("update-billboard-tooltip-hide", function(
    message
  ) {
    var chart = get_billboard(message.id);
    if (typeof chart != "undefined") {
      chart.tooltip.hide();
    }
  });
  // Hide
  Shiny.addCustomMessageHandler("update-billboard-hide", function(message) {
    var chart = get_billboard(message.id);
    if (typeof chart != "undefined") {
      chart.hide(message.data.targetIdsValue, message.data.options);
    }
  });
  // Show
  Shiny.addCustomMessageHandler("update-billboard-show", function(message) {
    var chart = get_billboard(message.id);
    if (typeof chart != "undefined") {
      chart.show(message.data.targetIdsValue, message.data.options);
    }
  });
  // Data names
  Shiny.addCustomMessageHandler("update-billboard-data-names", function(
    message
  ) {
    var chart = get_billboard(message.id);
    if (typeof chart != "undefined") {
      chart.data.names(message.data.names);
    }
  });
  // Data colors
  Shiny.addCustomMessageHandler("update-billboard-data-colors", function(
    message
  ) {
    var chart = get_billboard(message.id);
    if (typeof chart != "undefined") {
      chart.data.colors(message.data.colors);
    }
  });
  Shiny.addCustomMessageHandler("update-billboard-flow", function(message) {
    var chart = get_billboard(message.id);
    if (typeof chart != "undefined") {
      chart.flow(message.data);
    }
  });
  // Export
  Shiny.addCustomMessageHandler("update-billboard-export", function(message) {
    var chart = get_billboard(message.id);
    if (typeof chart != "undefined") {
      chart.export("image/png", function(dataUrl) {
        download(message.data.filename + ".png", dataUrl);
      });
    }
  });
}

// Utils -----

function download(filename, dataImage) {
  var element = document.createElement("a");
  element.setAttribute("href", dataImage);
  element.setAttribute("download", filename);

  element.style.display = "none";
  document.body.appendChild(element);

  element.click();

  document.body.removeChild(element);
}
