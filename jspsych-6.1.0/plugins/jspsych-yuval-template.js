/*
 * Example plugin template
 */

jsPsych.plugins["yuval-template"] = (function() {

  var plugin = {};

  plugin.info = {
    name: "yuval-template",
    description: '',
    parameters: {
    }
  }

  plugin.trial = function(display_element, trial) {

    var html_content = '<p>This is the first paragraph</p>';
    html_content += '<p>This is the second paragraph</p>';
    display_element.innerHTML = html_content;

    // end trial
    jsPsych.finishTrial(trial_data);
  };

  return plugin;
})();
