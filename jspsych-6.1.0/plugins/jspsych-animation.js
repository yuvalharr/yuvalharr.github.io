/**
 * jsPsych plugin for showing animations and recording keyboard responses
 * Josh de Leeuw
 *
 * documentation: docs.jspsych.org
 */

jsPsych.plugins["animation"] = (function() {

  var plugin = {};

  /* YH - registers 'stimuli' for later loading. 'animation' is name of trial and 'image' type of file YH */
  jsPsych.pluginAPI.registerPreload('animation', 'stimuli', 'image');

  plugin.info = {
    name: 'animation',
    description: '',
    parameters: {
      stimuli: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Stimuli',
        default: undefined,
        array: true,
        description: 'The images to be displayed.'
      },
      frame_time: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Frame time',
        default: 250,
        description: 'Duration to display each image.'
      },
      frame_isi: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Frame gap',
        default: 0,
        description: 'Length of gap to be shown between each image.'
      },
      sequence_reps: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Sequence repetitions',
        default: 1,
        description: 'Number of times to show entire sequence.'
      },
      choices: {
        type: jsPsych.plugins.parameterType.KEYCODE,
        pretty_name: 'Choices',
        default: jsPsych.ALL_KEYS,
        array: true,
        description: 'Keys subject uses to respond to stimuli.'
      },
      prompt: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Prompt',
        default: null,
        description: 'Any content here will be displayed below stimulus.'
      }
    }
  }

  plugin.trial = function(display_element, trial) {

    var interval_time = trial.frame_time + trial.frame_isi;
    var animate_frame = -1;
    var reps = 0;
    var startTime = performance.now();
    var animation_sequence = [];
    var responses = [];
    var rt =[]; // YH - added to add rt data
    var current_stim = "";

    //show_next_frame(); // Trying to make image appear first with no problems

    var animate_interval = setInterval(function() { /* YH- setInterval-> executes function every interval */
      var showImage = true;
      display_element.innerHTML = ''; // clear everything
      animate_frame++;
      if (animate_frame == trial.stimuli.length) { // YH - if all stims are done showing -> 
        animate_frame = 0;                         // turn animate_frame back to 0
        reps++;                                    // and add 1 rep representing a new repetition -YH
        if (reps >= trial.sequence_reps) { // YH- if all reps are done -> 
          endTrial();                      // execute endTrial func that finishes jspsych trial (written at the end of code)
          clearInterval(animate_interval); // and kill animate_interval from running any more- YH
          showImage = false;
        }
      }
      if (showImage) {
        show_next_frame();
      }
    }, interval_time); /* <---- YH - interval time from comment above is stated here - YH */

    function show_next_frame() {
      // show image
      // YH - display the image that is in the [animate_frame] spot in the 'stimuli' array 
      display_element.innerHTML = '<img src="'+trial.stimuli[animate_frame]+'" id="jspsych-animation-image"></img>'

      current_stim = trial.stimuli[animate_frame];

      // record when image was shown
      animation_sequence.push({
        "stimulus": trial.stimuli[animate_frame],
        "time": performance.now() - startTime
      });

      if (trial.prompt !== null) { // YH - if 'prompt' is not empty ->
        display_element.innerHTML += trial.prompt; // display it
      }

      if (trial.frame_isi > 0) { // YH - if there is frame_isi -> hide image at frame_time and write the blanks also into the data
        jsPsych.pluginAPI.setTimeout(function() { // YH -executes this function after waiting for trial.frame_time (below)
          display_element.querySelector('#jspsych-animation-image').style.visibility = 'hidden';
          current_stim = 'blank';
          // record when blank image was shown
          animation_sequence.push({
            "stimulus": 'blank',
            "time": performance.now() - startTime
          });
        }, trial.frame_time);
      }
    }

    var after_response = function(info) {
      
      rt.push(
        performance.now() - startTime
      );

      responses.push({
        key_press: info.key,
        rt: info.rt,
        stimulus: current_stim
      });

      

      
      
      // after a valid response, the stimulus will have the CSS class 'responded'
      // which can be used to provide visual feedback that a response was recorded
      display_element.querySelector('#jspsych-animation-image').className += ' responded';

      endTrial();
      clearInterval(animate_interval); // and kill animate_interval from running any more- YH
      showImage = false;
    }

    // hold the jspsych response listener object in memory
    // so that we can turn off the response collection when
    // the trial ends
    var response_listener = jsPsych.pluginAPI.getKeyboardResponse({
      callback_function: after_response,
      valid_responses: trial.choices,
      rt_method: 'performance',
      persist: false, // changed by YH - 'false' means that the key press will trigger the function only on the first press
      allow_held_key: false
    });

    // YH - add stimuli number to data of trial - YH
    var stimuli_txt = trial.stimuli[0];
    var stimuli_num = stimuli_txt.replace(/[^0-9]/g,''); // extract only num from stim name
    var stimuli_side = stimuli_txt.substring(7, 9).replace(/[^a-zA-Z]+/g, ''); // extract only R or L from stim name
    

    function endTrial() {

      jsPsych.pluginAPI.cancelKeyboardResponse(response_listener);

      var trial_data = {
        "animation_sequence": JSON.stringify(animation_sequence),
        "responses": JSON.stringify(responses),
        "rt": JSON.stringify(rt[0]), //YH added
        //"stimuli_txt": stimuli_txt, // YH added
        "stimulus": stimuli_num, // YH added 
        "stimulus_side": stimuli_side // YH added

      };

      jsPsych.finishTrial(trial_data);
    }
  };

  return plugin;
})();
