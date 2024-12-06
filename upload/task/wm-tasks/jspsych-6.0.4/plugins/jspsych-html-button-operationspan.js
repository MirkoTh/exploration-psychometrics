/**
 * jspsych-html-button-response
 * Josh de Leeuw
 *
 * plugin for displaying a stimulus and getting a keyboard response
 *
 * documentation: docs.jspsych.org
 *
 **/

jsPsych.plugins["html-button-operationspan"] = (function () {

  var plugin = {};

  plugin.info = {
    name: 'html-button-operationspan',
    description: '',
    parameters: {
      stimulus: {
        type: jsPsych.plugins.parameterType.HTML_STRING,
        pretty_name: 'Stimulus',
        default: undefined,
        description: 'The HTML string to be displayed'
      },
      choices: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Choices',
        default: undefined,
        array: true,
        description: 'The labels for the buttons.'
      },
      button_html: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Button HTML',
        default: '<button class="jspsych-btn-numpad" style = "width:120px; font-size: 20px" >%choice%</button>',
        array: true,
        description: 'The html of the button. Can create own style.'
      },
      prompt: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Prompt',
        default: "Press &uarr; for true and &darr; for false.",
        description: 'Any content here will be displayed under the button.'
      },
      stimulus_duration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Stimulus duration',
        default: null,
        description: 'How long to hide the stimulus.'
      },
      trial_duration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Trial duration',
        default: null,
        description: 'How long to show the trial.'
      },
      margin_vertical: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Margin vertical',
        default: '0px',
        description: 'The vertical margin of the button.'
      },
      margin_horizontal: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Margin horizontal',
        default: '8px',
        description: 'The horizontal margin of the button.'
      },
      response_ends_trial: {
        type: jsPsych.plugins.parameterType.BOOL,
        pretty_name: 'Response ends trial',
        default: true,
        description: 'If true, then trial will end when user responds.'
      },
      equation_accuracy: {
        type: jsPsych.plugins.parameterType.BOOL,
        pretty_name: 'Accuracy of Equation',
        default: true,
        description: 'Is the currently presented equation correct of false?'
      },
      trial_id_processing: {
        type: jsPsych.plugins.parameterType.array,
        pretty_name: 'Processing Position',
        default: null,
        description: 'Processing position within one OS trial'
      },
      trial_id_recall: {
        type: jsPsych.plugins.parameterType.array,
        pretty_name: 'Trial Counter',
        default: null,
        description: 'Counter over OS span trials'
      },
      participant_id: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'ID of participant',
        default: null,
        description: 'ID of participant'
      },
      is_practice: {
        type: jsPsych.plugins.parameterType.INT,
        default: undefined,
        description: 'practice or experimental trials?'
      },
      is_local: {
        type: jsPsych.plugins.parameterType.BOOL,
        default: undefined,
        description: 'printed locally or on the server?'
      },
      set_size: {
        type: jsPsych.plugins.parameterType.INT,
        default: undefined,
        description: 'how many letters to remember on the current trial?'
      }
    }
  }

  var data_cumulative;
  data_cumulative = [];


  plugin.trial = function (display_element, trial) {

    // display stimulus
    var html = '<div id="jspsych-html-button-response-stimulus">' + trial.stimulus + '</div>';

    //display buttons
    /* var buttons = [];
    if (Array.isArray(trial.button_html)) {
      if (trial.button_html.length == trial.choices.length) {
        buttons = trial.button_html;
      } else {
        console.error('Error in html-button-response plugin. The length of the button_html array does not equal the length of the choices array');
      }
    } else {
      for (var i = 0; i < trial.choices.length; i++) {
        buttons.push(trial.button_html);
      }
    }
    html += '<div id="jspsych-html-button-response-btngroup">';
    for (var i = 0; i < trial.choices.length; i++) {
      var str = buttons[i].replace(/%choice%/g, trial.choices[i]);
      html += '<div class="jspsych-html-button-response-button" style="display: inline-block; margin:' + 20 + ' ' + 20 + '" id="jspsych-html-button-response-button-' + i + '" data-choice="' + i + '">' + str + '</div>';
    } */
    html += '</div>';

    //show prompt if there is one
    if (trial.prompt !== null) {
      html += trial.prompt;
    }
    display_element.innerHTML = html;

    // start time
    var start_time = Date.now();

    // add event listeners to buttons
    /* for (var i = 0; i < trial.choices.length; i++) {
      display_element.querySelector('#jspsych-html-button-response-button-' + i).addEventListener('click', function (e) {
        var choice = e.currentTarget.getAttribute('data-choice'); // don't use dataset for jsdom compatibility
        if ((trial.equation_accuracy) && (choice == 0)) {
          acc = 1
        } else if ((!trial.equation_accuracy) && (choice == 1)) {
          acc = 1
        } else {
          acc = 0
        }
        after_response(acc);
      });
    } */

    document.addEventListener("keydown", handle_operation_keypress, false);
    // this is weirdly coded
    // choice refers to accuracy
    async function handle_operation_keypress(e) {
      if (e.keyCode == 38 || e.keyCode == 40) {
        document.removeEventListener("keydown", handle_operation_keypress, false);
        var os_response;
        if (e.keyCode == 38) {
          os_response = Boolean(true);
        } else if (e.keyCode == 40) {
          os_response = Boolean(false);
        }

        if ((trial.equation_accuracy == os_response)) {
          choice = 1
        } else {
          choice = 0
        }
        after_response(choice);
      }

    }




    // store response
    var response = {
      rt: null,
      button: null
    };

    // function to handle responses by the subject
    function after_response(choice) {

      // measure rt

      var end_time = Date.now();
      var rt = end_time - start_time;
      response.button = choice;
      response.rt = rt;
      response.accuracy = choice;

      // after a valid response, the stimulus will have the CSS class 'responded'
      // which can be used to provide visual feedback that a response was recorded
      display_element.querySelector('#jspsych-html-button-response-stimulus').className += ' responded';

      // disable all the buttons after a response
      var btns = document.querySelectorAll('.jspsych-html-button-response-button button');
      for (var i = 0; i < btns.length; i++) {
        //btns[i].removeEventListener('click');
        btns[i].setAttribute('disabled', 'disabled');
      }

      if (trial.response_ends_trial) {
        end_trial();
      }
    };

    // function to end trial when it is time
    function end_trial() {

      // kill any remaining setTimeout handlers
      jsPsych.pluginAPI.clearAllTimeouts();

      // gather the data to store for the trial
      var trial_data = {
        participant_id: trial.participant_id,
        session_id: session_id,
        is_practice: trial.is_practice,
        trial_id_recall: trial.trial_id_recall,
        processing_position: trial.trial_id_processing,
        rt: response.rt,
        accuracy: response.accuracy
      };
      data_cumulative.push(trial_data);

      // clear the display
      display_element.innerHTML = '';

      // move on to the next trial
      jsPsych.finishTrial(trial_data);

      if (trial.is_local) {
      } else if (!trial.is_local) {
        var file_name = "OS_processing_" + session_id + "_" + trial.participant_id + ".json";
        saveData(JSON.stringify(trial_data), file_name, "OS");
        if (trial.trial_id_processing + 1 == trial.set_size) {
          let dataSaved = false;
          var file_name_cum = "OS_processing_allinone_" + session_id + "_" + trial.participant_id + ".json";
          saveSeveralDataOverwrite(data_cumulative, file_name_cum, "OS");
          checkDataSaving();
        }

      }
    };

    // hide image if timing is set
    if (trial.stimulus_duration !== null) {
      jsPsych.pluginAPI.setTimeout(function () {
        display_element.querySelector('#jspsych-html-button-response-stimulus').style.visibility = 'hidden';
      }, trial.stimulus_duration);
    }

    // end trial if time limit is set
    if (trial.trial_duration !== null) {
      jsPsych.pluginAPI.setTimeout(function () {
        document.removeEventListener("keydown", handle_operation_keypress, false);
        response.rt = trial.trial_duration // default to trial duration if no response within time window available
        response.accuracy = 0; // default if no response within time window available
        end_trial();
      }, trial.trial_duration);
    }

  };

  return plugin;
})();
