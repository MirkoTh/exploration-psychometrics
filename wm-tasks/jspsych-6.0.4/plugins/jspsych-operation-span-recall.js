/*
 * Example plugin template
 */

jsPsych.plugins["operation-span-recall"] = (function () {

  var plugin = {};

  jsPsych.pluginAPI.registerPreload('visual-search-circle', 'target', 'image');
  jsPsych.pluginAPI.registerPreload('visual-search-circle', 'foil', 'image');
  jsPsych.pluginAPI.registerPreload('visual-search-circle', 'fixation_image', 'image');


  plugin.info = {
    name: 'operation-span-recall',
    description: '',
    parameters: {
      trial_duration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Trial duration',
        default: null,
        description: 'How long to show the trial.'
      },
      size_cells: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Size Cells',
        default: 70,
        description: 'Size of the boxes for the recall options.'
      },
      correct_order: {
        type: jsPsych.plugins.parameterType.INT,
        default: undefined,
        description: 'Record the correct array'
      },
      response_options: {
        type: jsPsych.plugins.parameterType.array,
        default: undefined,
        description: 'letters to be displayed in response grid'
      },
      nrows_matrix: {
        type: jsPsych.plugins.parameterType.INT,
        default: undefined,
        description: 'Number rows of the response grid'
      },
      ncols_matrix: {
        type: jsPsych.plugins.parameterType.INT,
        default: undefined,
        description: 'Number cols of the response grid'
      },
      trial_id_recall: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Memory Trial ID',
        default: null,
        description: "counter over memory trials"
      },
      is_local: {
        type: jsPsych.plugins.parameterType.BOOL,
        pretty_name: 'save data locally',
        default: true,
        description: "should data be saved locally or on server"
      },
      participant_id: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Participant ID',
        default: "not-saved-properly",
        description: "the participant id to store stuff"
      },
      is_practice: {
        type: jsPsych.plugins.parameterType.INT,
        default: undefined,
        description: 'practice or experimental trials?'
      }
    }
  }

  var data_cumulative;
  data_cumulative = [];


  plugin.trial = function (display_element, trial) {

    // making matrix:
    var grid = 3;
    var recalledGrid = [];
    var correctLetters = trial.correct_order
    var display = " ";

    var setSize = correctLetters.length
    var leftOver = trial.nrows_matrix * trial.ncols_matrix - setSize


    function indexOfArray(val, array) {
      var
        hash = {},
        indexes = {},
        i, j;
      for (i = 0; i < array.length; i++) {
        hash[array[i]] = i;
      }
      return (hash.hasOwnProperty(val)) ? hash[val] : -1;
    };


    var numbertobutton = new Object;
    for (i = 0; i < trial.response_options.length; i++) {
      numbertobutton[i.toString()] = trial.response_options[i]
    }

    recordClick = function (data) {
      var tt = data.getAttribute('id')
      var tt = ("#" + tt)
      display_element.querySelector(tt).className = 'jspsych-operation-span-recall';
      var recalledN = (data.getAttribute('data-choice'));
      recalledGrid.push(numbertobutton[recalledN])
      var div = document.getElementById('recall_space');
      display += numbertobutton[recalledN] + " "
      div.innerHTML = display;
    }

    clearSpace = function (data) {
      recalledGrid = recalledGrid.slice(0, (recalledGrid.length - 1))
      var div = document.getElementById('recall_space');
      display = display.slice(0, (display.length - 2))
      div.innerHTML = display
    }

    var matrix = [];
    for (var i = 0; i < trial.nrows_matrix; i++) {
      m1 = i;
      for (var h = 0; h < trial.ncols_matrix; h++) {
        m2 = h;
        matrix.push([m1, m2])
      }
    };

    matrix.splice(trial.response_options.length, matrix.length);

    var paper_size = [(trial.ncols_matrix * (trial.size_cells + 30)), ((trial.nrows_matrix * (trial.size_cells + 20)) + 100)];

    display_element.innerHTML = '<div id="jspsych-html-button-response-btngroup" style= "position: relative; width:' + paper_size[0] + 'px; height:' + paper_size[1] + 'px"></div>';
    var paper = display_element.querySelector("#jspsych-html-button-response-btngroup");

    paper.innerHTML += '<div class="recall-space" style="position: absolute; top:' + 0 + 'px; left:' + (paper_size[0] / 2 - 310) + 'px; width:600px; height:64px" id="recall_space">' + recalledGrid + '</div>';


    var buttons = trial.response_options;

    for (var i = 0; i < matrix.length; i++) {
      var str = buttons[i]
      paper.innerHTML += '<div class="jspsych-operation-span-recall" style="position: absolute; top:' + (matrix[i][0] * (trial.size_cells + 20) + 80) + 'px; left:' + matrix[i][1] * (trial.size_cells + 30) + 'px; width:' + (trial.size_cells - 6) + 'px; height:' + (trial.size_cells - 6) + 'px"; id="jspsych-spatial-span-grid-button-' + i + '" data-choice="' + i + '" onclick="recordClick(this)">' + str + '</div>';
    }


    display_element.innerHTML += '<div class="jspsych-btn-numpad" style="display: inline-block; margin:' + 10 + ' ' + 2 + '" id="jspsych-html-button-response-button-clear" onclick="clearSpace(this)">Backspace</div>';

    display_element.innerHTML += '<div class="jspsych-btn-numpad" style="display: inline-block; margin:' + 10 + ' ' + 40 + '" id="jspsych-html-button-response-button">Continue</div>';


    var start_time = Date.now();

    display_element.querySelector('#jspsych-html-button-response-button').addEventListener('click', function (e) {
      var acc = 0
      for (var i = 0; i < correctLetters.length; i++) {
        if (recalledGrid[i] == correctLetters[i]) {
          acc += 1
        }
      }
      after_response(acc);
    });

    var response = {
      rt: null,
      button: null
    };


    function after_response(choice) {

      // measure rt
      var end_time = Date.now();
      var rt = end_time - start_time;
      var choiceRecord = choice;
      response.button = choice;
      response.rt = rt;

      // after a valid response, the stimulus will have the CSS class 'responded'
      // which can be used to provide visual feedback that a response was recorded
      //display_element.querySelector('#jspsych-html-button-response-stimulus').className += ' responded';

      // disable all the buttons after a response
      var btns = document.querySelectorAll('.jspsych-html-button-response-button button');
      for (var i = 0; i < btns.length; i++) {
        //btns[i].removeEventListener('click');
        btns[i].setAttribute('disabled', 'disabled');
      }

      clear_display();
      end_trial();
    };

    if (trial.trial_duration !== null) {
      jsPsych.pluginAPI.setTimeout(function () {
        clear_display();
        end_trial();
      }, trial.trial_duration);
    }

    function clear_display() {
      display_element.innerHTML = '';
    }


    function end_trial() {

      // kill any remaining setTimeout handlers
      jsPsych.pluginAPI.clearAllTimeouts();

      // gather the data to store for the trial
      var trial_data = {
        participant_id: trial.participant_id,
        is_practice: trial.is_practice,
        trial_id_recall: trial.trial_id_recall,
        rt: response.rt,
        recall: recalledGrid,
        stimuli: correctLetters,
        accuracy: response.button
      };
      // move on to the next trial
      jsPsych.finishTrial(trial_data);

      // save for our purposes
      var data_recall_clean = {
        participant_id: participant_id,
        is_practice: trial.is_practice,
        trial_id_recall: trial.trial_id_recall,
        set_size: trial["data"]["set_size"],
        stimuli: correctLetters,
        response: recalledGrid,
        n_correct: response.button,
        rt: response.rt
      };
      data_cumulative.push(data_recall_clean);

      if (trial.is_local) {
      } else if (!trial.is_local) {
        var file_name = "OS_recall_" + trial.participant_id + ".json";
        saveData(JSON.stringify(data_recall_clean), file_name, "OS");
        var file_name_cum = "OS_recall_allinone_" + trial.participant_id + ".json";
        saveSeveralDataOverwrite(data_cumulative, file_name_cum, "OS");
        setTimeout(undefined, 1000);
      }
    }
  };

  return plugin;
})();
