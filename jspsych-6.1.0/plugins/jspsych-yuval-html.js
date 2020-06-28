jsPsych.plugins['yuval-html'] = (function(){

  var plugin = {};

  plugin.info = {
    name: 'yuval-html',
    parameters: {
    }
  }

  plugin.trial = function(display_element, trial){
    var html_content = '<p>This is the first paragraph</p>';
    html_content += '<p>This is the second paragraph</p>';
    display_element.innerHTML = html_content;
    jsPsych.finishTrial();
  }

  return plugin;

})();