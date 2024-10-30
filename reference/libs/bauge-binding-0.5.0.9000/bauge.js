HTMLWidgets.widget({

  name: 'bauge',

  type: 'output',

  factory: function(el, width, height) {

    var bauge = null;

    return {

      renderValue: function(x) {

        if (bauge === null) {
          x.bindto = '#' + el.id;
          bauge = bb.generate(x);
        } else {
          bauge.config("gauge.max", x.gauge.max);
          bauge.config("gauge.min", x.gauge.min);
          bauge.load(x.data);
        }

      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
