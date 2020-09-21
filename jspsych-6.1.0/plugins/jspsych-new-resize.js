/**
* jspsych-resize
* Steve Chao
*
* plugin for controlling the real world size of the display
*
* documentation: docs.jspsych.org
*

YH ------- made by me to try and take card width from virtual-chin and use for resize here --------
**/

jsPsych.plugins["new-resize"] = (function() {

    var plugin = {};
  
    plugin.info = {
      name: 'new-resize',
      description: '',
      parameters: {
        item_height: {
          type: jsPsych.plugins.parameterType.INT,
          pretty_name: 'Item height',
          default: 1,
          description: 'The height of the item to be measured.'
        },
        item_width: {
          type: jsPsych.plugins.parameterType.INT,
          pretty_name: 'Item width',
          default: 1,
          description: 'The width of the item to be measured.'
        },
        prompt: {
          type: jsPsych.plugins.parameterType.STRING,
          pretty_name: 'Prompt',
          default: null,
          description: 'The content displayed below the resizable box and above the button.'
        },
        pixels_per_unit: {
          type: jsPsych.plugins.parameterType.INT,
          pretty_name: 'Pixels per unit',
          default: 100,
          description: 'After the scaling factor is applied, this many pixels will equal one unit of measurement.'
        },
        starting_size: {
          type: jsPsych.plugins.parameterType.INT,
          pretty_name: 'Starting size',
          default: 100,
          description: 'The initial size of the box, in pixels, along the larget dimension.'
        },
        button_label: {
          type: jsPsych.plugins.parameterType.STRING,
          pretty_name: 'Button label',
          default:  'Continue',
          description: 'Label to display on the button to complete calibration.'
        },
        card_width_px: {
          type: jsPsych.plugins.parameterType.INT,
          pretty_name: 'Card width px',
          default: 388,
          description: 'The width of the card in px (after user scaling), taken from virtual-chin trial'
        }
      }
    }
  
    plugin.trial = function(display_element, trial) {
  
      var aspect_ratio = trial.item_width / trial.item_height;
  
      // variables to determine div size
      if(trial.item_width >= trial.item_height){
        var start_div_width = trial.starting_size;
        var start_div_height = Math.round(trial.starting_size / aspect_ratio);
      } else {
        var start_div_height = trial.starting_size;
        var start_div_width = Math.round(trial.starting_size * aspect_ratio);
      }
  
      var scale_div = display_element.querySelector('#jspsych-resize-div');
  
      // scales the stimulus
      var scale_factor;
      
      var pixels_unit_screen;
      
        //final_width_px = scale_div.offsetWidth;
        //final_height_px = scale_div.offsetHeight;
        final_width_px = trial.card_width_px; // YH -- here takes final card width in px from virtual-chin trial. for now - assign manually -- YH
        pixels_unit_screen = final_width_px / trial.item_width;
  
        scale_factor = pixels_unit_screen / trial.pixels_per_unit;
        document.getElementById("jspsych-content").style.transform = "scale(" + scale_factor + ")";
      

      end_trial();
  
  
      // function to end trial
      function end_trial() {
  
        // clear the screen
        display_element.innerHTML = '';
  
        // finishes trial
  
        var trial_data = {
          //'final_height_px': final_height_px,
          'final_width_px': final_width_px,
          'scale_factor': scale_factor,
          'item_width': trial.item_width,
          'pixels_unit_screen': pixels_unit_screen,
          'pixels_per_unit': trial.pixels_per_unit

        }
  
        jsPsych.finishTrial(trial_data);
      }
    };
  
    return plugin;
  })();
  