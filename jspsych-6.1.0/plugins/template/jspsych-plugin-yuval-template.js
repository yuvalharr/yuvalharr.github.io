/*
 * Example plugin template
 */

jsPsych.plugins["PLUGIN-YUVAL"] = (function() {

  var plugin = {};

  plugin.info = {
    name: "PLUGIN-YUVAL",
    parameters: {
      parameter_name: {
        type: jsPsych.plugins.parameterType.INT, // BOOL, STRING, INT, FLOAT, FUNCTION, KEYCODE, SELECT, HTML_STRING, IMAGE, AUDIO, VIDEO, OBJECT, COMPLEX
        default: undefined
      },
      parameter_name: {
        type: jsPsych.plugins.parameterType.IMAGE,
        default: undefined
      }
    }
  }

  plugin.trial = function(display_element, trial) {

    var html_content = '<p>This is the first paragraph</p>';
    html_content += '<p>This is the second paragraph</p>';
    display_element.innerHTML = html_content;

    // data saving
    var trial_data = {
      parameter_name: 'parameter value'
    };

    // end trial
    jsPsych.finishTrial(trial_data);
  };

  return plugin;
})();
