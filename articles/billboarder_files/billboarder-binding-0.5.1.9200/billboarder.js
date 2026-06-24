// HTMLWidgets billboard ----

/* global HTMLWidgets, bb, Shiny */

HTMLWidgets.widget({
  name: "billboarder",
  type: "output",

  factory: function(el, width, height) {
    let chart = null;
    let bb_opts = null;

    const head = document.head || document.getElementsByTagName("head")[0];

    function safeGetChartSize() {
      return {
        width: el.clientWidth || width || 0,
        height: el.clientHeight || height || 0
      };
    }

    function upsertStyle(styleId, cssText) {
      let styleEl = document.getElementById(styleId);

      if (!styleEl) {
        styleEl = document.createElement("style");
        styleEl.id = styleId;
        styleEl.type = "text/css";
        head.appendChild(styleEl);
      }

      if (styleEl.styleSheet) {
        styleEl.styleSheet.cssText = cssText;
      } else {
        styleEl.textContent = cssText;
      }
    }

    function prefixCssWithWidgetId(css, widgetId) {
      if (Array.isArray(css)) {
        return css.map(function(rule) {
          return "#" + widgetId + " " + rule;
        }).join("\n");
      }

      return "#" + widgetId + " " + css;
    }

    function cloneEventPayload(payload) {
      return JSON.parse(JSON.stringify(payload));
    }

    function getCategoriesFromChart(ctx) {
      if (ctx && typeof ctx.categories === "function") {
        return ctx.categories() || [];
      }

      return [];
    }

    function addCategoryToDatum(ctx, datum) {
      const cloned = cloneEventPayload(datum);
      const categories = getCategoriesFromChart(ctx);

      cloned.category =
        Array.isArray(categories) && typeof cloned.index !== "undefined"
          ? categories[cloned.index] ?? null
          : null;

      return cloned;
    }

    function shinySetInput(name, value) {
      if (HTMLWidgets.shinyMode && typeof Shiny !== "undefined") {
        Shiny.onInputChange(name, {
          value: value,
          nonce: Date.now()
        });
      }
    }

    function attachDefaultShinyCallbacks(opts) {
      opts.data = opts.data || {};

      if (typeof opts.data.onclick === "undefined") {
        opts.data.onclick = function(d) {
          shinySetInput(el.id + "_click", addCategoryToDatum(this, d));
        };
      }

      if (typeof opts.data.onover === "undefined") {
        opts.data.onover = function(d) {
          shinySetInput(el.id + "_over", addCategoryToDatum(this, d));
        };
      }

      if (typeof opts.data.onselected === "undefined") {
        opts.data.onselected = function(d) {
          shinySetInput(el.id + "_selected", d);
        };
      }

      if (typeof opts.data.onunselected === "undefined") {
        opts.data.onunselected = function(d) {
          shinySetInput(el.id + "_unselected", d);
        };
      }

      if (opts.zoom && typeof opts.zoom.onzoom === "undefined") {
        opts.zoom.onzoom = function(domain) {
          shinySetInput(el.id + "_zoom", domain);
        };
      }
    }

    function attachExportCallback(opts) {
      if (typeof opts.export === "undefined") {
        return;
      }

      const userOnRendered = opts.onrendered;

      opts.onrendered = function() {
        if (typeof userOnRendered === "function") {
          userOnRendered.apply(this, arguments);
        }

        const ctx = this;

        setTimeout(function() {
          if (!ctx || typeof ctx.export !== "function") {
            return;
          }

          ctx.export("image/png", function(dataUrl) {
            const link = document.getElementById(el.id + "-export");

            if (!link) {
              return;
            }

            link.download =
              (typeof opts.export.filename !== "undefined"
                ? opts.export.filename
                : "export-" + Date.now()) + ".png";

            link.innerHTML = opts.export.download_label || "Export (.png)";
            link.href = dataUrl;
            link.style.display = "inline-block";
          });
        }, 300);
      };
    }

    function applyWidgetSizing(opts) {
      const size = safeGetChartSize();

      opts.size = opts.size || {};
      opts.size.width = size.width;
      opts.size.height = size.height;
    }

    function applyBillboarderSpecialStyles(opts) {
      if (
        opts.billboarderspecials &&
        typeof opts.billboarderspecials.opacity !== "undefined"
      ) {
        const css =
          "#" +
          el.id +
          " .bb-area { opacity: " +
          opts.billboarderspecials.opacity +
          " !important; }";

        upsertStyle(el.id + "-billboarder-opacity-style", css);
      }
    }

    function applyCustomStyles(opts) {
      if (typeof opts.customStyle === "undefined") {
        return;
      }

      const css = prefixCssWithWidgetId(opts.customStyle, el.id);
      upsertStyle(el.id + "-billboarder-custom-style", css);
    }

    function destroyExistingChart() {
      if (chart && typeof chart.destroy === "function") {
        chart.destroy();
      }
      chart = null;
    }

    function resizeChartToContainer() {
      if (!chart || typeof chart.resize !== "function") {
        return;
      }

      const container = document.getElementById(el.id);

      if (!container) {
        return;
      }

      chart.resize({
        width: container.clientWidth,
        height: container.clientHeight
      });
    }

    function handleFlexdashboardResize() {
      if (typeof window.FlexDashboard === "undefined") {
        return;
      }

      window.requestAnimationFrame(function() {
        window.requestAnimationFrame(function() {
          if (!chart) {
            return;
          }

          if (typeof chart.flush === "function") {
            chart.flush();
          }

          resizeChartToContainer();
        });
      });
    }

    function resolveOptions(x) {
      if (x && x.bb_opts && typeof x.bb_opts.data !== "undefined") {
        return x.bb_opts;
      }

      return x.bb_empty || {};
    }

    return {
      renderValue: function(x) {
        bb_opts = resolveOptions(x);

        bb_opts = bb_opts || {};
        bb_opts.data = bb_opts.data || {};
        bb_opts.bindto = "#" + el.id;

        if (HTMLWidgets.shinyMode) {
          attachDefaultShinyCallbacks(bb_opts);
        }

        applyWidgetSizing(bb_opts);
        attachExportCallback(bb_opts);

        destroyExistingChart();
        chart = bb.generate(bb_opts);

        handleFlexdashboardResize();
        applyBillboarderSpecialStyles(bb_opts);
        applyCustomStyles(bb_opts);
      },

      getChart: function() {
        return chart;
      },

      resize: function() {
        resizeChartToContainer();
      }
    };
  }
});

// Access underlying billboard instance ----

function get_billboard(id) {
  const htmlWidgetsObj = HTMLWidgets.find("#" + id);

  if (typeof htmlWidgetsObj === "undefined" || htmlWidgetsObj === null) {
    return undefined;
  }

  return htmlWidgetsObj.getChart();
}

// Shiny ----

if (HTMLWidgets.shinyMode) {
  function withChart(message, callback) {
    const chart = get_billboard(message.id);

    if (!chart) {
      return;
    }

    callback(chart, message.data, message);
  }

  Shiny.addCustomMessageHandler("update-billboard-data", function(message) {
    withChart(message, function(chart, data) {
      chart.load(data);
    });
  });

  Shiny.addCustomMessageHandler("update-billboard-load", function(message) {
    withChart(message, function(chart, data) {
      chart.load(data);
    });
  });

  Shiny.addCustomMessageHandler("update-billboard-unload", function(message) {
    withChart(message, function(chart, data) {
      if (typeof chart.unload !== "function") {
        return;
      }

      if (data && Object.keys(data).length > 0) {
        chart.unload(data);
      } else {
        chart.unload();
      }
    });
  });

  Shiny.addCustomMessageHandler("update-billboard-focus", function(message) {
    withChart(message, function(chart, data) {
      if (data && Array.isArray(data.ids) && data.ids.length > 0) {
        chart.focus(data.ids);
      } else {
        chart.focus();
      }
    });
  });

  Shiny.addCustomMessageHandler("update-billboard-defocus", function(message) {
    withChart(message, function(chart, data) {
      if (data && Array.isArray(data.ids) && data.ids.length > 0) {
        chart.defocus(data.ids);
      } else {
        chart.defocus();
      }
    });
  });

  Shiny.addCustomMessageHandler("update-billboard-axis_labels", function(message) {
    withChart(message, function(chart, data) {
      if (chart.axis && typeof chart.axis.labels === "function") {
        chart.axis.labels(data);
      }
    });
  });

  Shiny.addCustomMessageHandler("update-billboard-xs", function(message) {
    withChart(message, function(chart, data) {
      chart.xs(data);
    });
  });

  Shiny.addCustomMessageHandler("update-billboard-categories", function(message) {
    withChart(message, function(chart, data) {
      chart.categories(Array.isArray(data) ? data[0] : data);
    });
  });

  Shiny.addCustomMessageHandler("update-billboard-region", function(message) {
    withChart(message, function(chart, data) {
      chart.regions(data);
    });
  });

  Shiny.addCustomMessageHandler("update-billboard-groups", function(message) {
    withChart(message, function(chart, data) {
      chart.groups(data);
    });
  });

  Shiny.addCustomMessageHandler("update-billboard-legend-show", function(message) {
    withChart(message, function(chart, data) {
      if (data && data.targetIds !== null) {
        chart.legend.show(data.targetIds);
      } else {
        chart.legend.show();
      }
    });
  });

  Shiny.addCustomMessageHandler("update-billboard-legend-hide", function(message) {
    withChart(message, function(chart, data) {
      if (data && data.targetIds !== null) {
        chart.legend.hide(data.targetIds);
      } else {
        chart.legend.hide();
      }
    });
  });

  Shiny.addCustomMessageHandler("update-billboard-tooltip-show", function(message) {
    withChart(message, function(chart, data) {
      chart.tooltip.show(data);
    });
  });

  Shiny.addCustomMessageHandler("update-billboard-tooltip-hide", function(message) {
    withChart(message, function(chart) {
      chart.tooltip.hide();
    });
  });

  Shiny.addCustomMessageHandler("update-billboard-hide", function(message) {
    withChart(message, function(chart, data) {
      chart.hide(data.targetIdsValue, data.options);
    });
  });

  Shiny.addCustomMessageHandler("update-billboard-show", function(message) {
    withChart(message, function(chart, data) {
      chart.show(data.targetIdsValue, data.options);
    });
  });

  Shiny.addCustomMessageHandler("update-billboard-data-names", function(message) {
    withChart(message, function(chart, data) {
      chart.data.names(data.names);
    });
  });

  Shiny.addCustomMessageHandler("update-billboard-data-colors", function(message) {
    withChart(message, function(chart, data) {
      chart.data.colors(data.colors);
    });
  });

  Shiny.addCustomMessageHandler("update-billboard-flow", function(message) {
    withChart(message, function(chart, data) {
      chart.flow(data);
    });
  });

  Shiny.addCustomMessageHandler("update-billboard-export", function(message) {
    withChart(message, function(chart, data) {
      if (typeof chart.export !== "function") {
        return;
      }

      chart.export("image/png", function(dataUrl) {
        download(data.filename + ".png", dataUrl);
      });
    });
  });
}

// Utils ----

function download(filename, dataImage) {
  const element = document.createElement("a");
  element.setAttribute("href", dataImage);
  element.setAttribute("download", filename);
  element.style.display = "none";

  document.body.appendChild(element);
  element.click();
  document.body.removeChild(element);
}
