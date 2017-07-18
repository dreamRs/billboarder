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
        var elpar = document.getElementById(el.id).parentElement;
        var w = elpar.clientWidth;
        var h = elpar.clientHeight;
        bb_opts.size = {};
        bb_opts.size.width = w;
        bb_opts.size.height = h;

        // Generate billboard chart
        chart = bb.generate(bb_opts);

      },

      resize: function(width, height) {

        // code to re-render the widget with a new size
        var elpar = document.getElementById(el.id).parentElement;
        var w = elpar.clientWidth;
        var h = elpar.clientHeight;
        console.log(h);
        chart.resize({width: w, height: h});

      }

    };
  }
});
