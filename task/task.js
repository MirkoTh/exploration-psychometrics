// task code for exploration reliablity study

// coded by Kristin Witte and github copilot (and chatgpt)

// settings

var quickrunthrough = 1

// variables we need
var session = 1

var totalScore = 0;
var Ntrials
var Nblocks
var scoreThisBlock = 0;
var rewards; Nblocks; 
var Ntrials = 10; 
var currentBlock;
var trial = 0;
var currentBlock = 0
var reward = 0;
var fixedChoices
var fixedChoicesCollect
var rewardCollect
var NtrialsCollect
var score = 0
var comprehensionAttemptsH = 1
var comprehensionAttemptsS = 1
var comprehensionAttemptsR = 1
var understood = false
var horizonRewards
var samRewards
var restlessRewards

// slot machines (image + button)
var machine1 = document.getElementById('mach_div1');
var machine2 = document.getElementById('mach_div2');
var machine3 = document.getElementById('mach_div3');
var machine4 = document.getElementById('mach_div4');
// "play" buttons below the slot machine images
var machButton1 = document.getElementById('machine1');
var machButton2 = document.getElementById('machine2');
var machButton3 = document.getElementById('machine3');
var machButton4 = document.getElementById('machine4');
var slot_machines = document.getElementById('slot_machines');

// pre-load rewards
$.getJSON("rewardsHorizon"+ session+".json", function(data) {
    horizonRewards = data;
  })
$.getJSON("fixedChoices"+ session+".json", function(data) {
    fixedChoicesCollect = data;
  })

$.getJSON("Horizon"+ session+".json", function(data) {
    NtrialsCollect = data;
  })

$.getJSON("rewardsSam"+ session+".json", function(data) {
    samRewards = data;
  })
$.getJSON("rewards4ARB"+ session+".json", function(data) {
    restlessRewards = data;
  })


// click function
var clickMachine = function(machine, task) {
    // input:
    // machine: which machine was clicked (0-3)
    // task: which task are we in (horizon, sam, restless)
    
    // get variables ---------------
    rewards = rewardCollect[currentBlock][trial]
    console.log(rewards)
    Ntrials = NtrialsCollect[currentBlock]
    fixedChoices = fixedChoicesCollect[currentBlock]

    console.log("clicked machine " + machine  + " in task " +task )

    // was this a machine they were allowed to chlick in the horizon task? Do we still have trials left this round?
    if (((trial > 3 | task != "horizon") | machine == fixedChoices[trial])& Ntrials - trial > 0) { // if (either not a fixed choice OR not Horizon task OR they clicked the right one) AND trials left
        

        // get rewards and display them-----------------------------
    // display reward
    var reward = rewards[machine]
    if (machine == 0) {
        machine1.append(reward + ' points');
        // remove score after 1 second
        setTimeout(function() {
            machine1.lastChild.remove();
        }, 1000)

    } else if (machine == 1) { 
        machine2.append(reward + ' points');
        // remove score after 1 second
        setTimeout(function() {
            machine2.lastChild.remove();
        }, 1000)
   
    }else if (machine == 2) { 
        machine3.append(reward + ' points');
        // remove score after 1 second
        setTimeout(function() {
            machine3.lastChild.remove();
        }, 1000)
   
    } else if (machine == 3) { 
        machine4.append(reward + ' points');
        // remove score after 1 second
        setTimeout(function() {
            machine4.lastChild.remove();
        }, 1000)
   
    }

    // increment trial
    trial += 1
    
    console.log(trial)
    // fixed choice

    setTimeout(function() {
        if (trial < 4 & task == "horizon") {
            if (fixedChoices[trial] == 0) {
                machine1.style.opacity = 1;
                machine2.style.opacity = 0.5;
        
            } else {
                machine1.style.opacity = 0.5;
                machine2.style.opacity = 1;
            }
    
        } else {
            machine1.style.opacity = 1;
            machine2.style.opacity = 1;
        }
    }, 800)
     

    if (trial == 4 & task == "horizon") {document.getElementById('machine_title').innerHTML = 'Select the slot machine you would like to play!';}
    
    scoreThisBlock = scoreThisBlock + reward;
    totalScore = totalScore + reward;

    // update score div
    document.getElementById('score').innerHTML ='Score this round:' + scoreThisBlock+ '<br> Total score: ' + totalScore + "<br> Trials left in this round: " + (Ntrials - trial)
    

    // if no trials left -----------------------------------------
    if (trial == Ntrials) { 
        currentBlock += 1;

        // was this a practice round?
        if (currentBlock == 1) { // practice round

            // go to comprehension
            startComprehension(task)



        } else if (currentBlock == Nblocks) { // game over
            scoreThisBlock = 0;
            if (task != "restless") {
            slot_machines.style.display = 'none';
            document.getElementById('machine_title').innerHTML = "This Game is over. Click the button to proceed to the next game."
            setTimeout(function() {
                document.getElementById('score').innerHTML = ""
                nextTaskButton.style.display = 'block';
            }, 1500)
            
        } else { // study over
            slot_machines.style.display = 'none';
            document.getElementById('machine_title').innerHTML = "This study is over. Thank you for participating! Click the button below to return to prolific."
            document.getElementById('score').innerHTML ='<br> Total score: ' + totalScore 
            endTaskButton.style.display = 'block';

        }

        } else { // go to next block

            document.getElementById('machine_title').innerHTML = 'New round!';
            slot_machines.style.display = "none";
            // reset trial
            trial = 0;
            Ntrials = NtrialsCollect[currentBlock]

            //document.getElementById('score').innerHTML ='Score this round:' + scoreThisBlock+ '<br> Total score: ' + totalScore + "<br> Trials left in this round: " + (Ntrials - trial)
            scoreThisBlock = 0;

            setTimeout(function() {
                slot_machines.style.display = "flex";
                // update score
                document.getElementById('score').innerHTML ='Score this round:' + scoreThisBlock+ '<br> Total score: ' + totalScore + "<br> Trials left in this round: " + (Ntrials - trial)
           
                if (task == "horizon") { // for Horizon task, we are back to fixed choice
                    document.getElementById('machine_title').innerHTML = 'Select the highlighted slot machine.';
                    fixedChoices = fixedChoicesCollect[currentBlock]
                    if (fixedChoices[trial] == 0) {
                        machine1.style.opacity = 1;
                        machine2.style.opacity = 0.5;
                
                    } else {
                        machine1.style.opacity = 0.5;
                        machine2.style.opacity = 1;
                    }


                } else {
                    document.getElementById('machine_title').innerHTML = 'Select the slot machine you would like to play!';
                }
                
            }, 2000)

            
            
        }

    }


    }

    
    
}

// the separate tasks -------------------

// horizon task
// function that implements the Horizon task

function horizonTask() {
    // set up variables

    // load rewards or quickly get some place holder rewards
    if (quickrunthrough == 1) { 
        Nblocks = 2; 
        rewardCollect = Array(Nblocks).fill([[20, 30], [20, 30], [20, 30], [20, 30], [20, 30], [20, 30], [20, 30], [20, 30], [20, 30], [20, 30]] )
        fixedChoicesCollect = [[0,1,0,1], [0,0,0,1]] 
        NtrialsCollect = [5, 5] 
    }
    else {
        rewardCollect = horizonRewards

        // number of blocks is length of the list of Ntrials    
        Nblocks = NtrialsCollect.length;


    }
    
    currentBlock = 0;
    trial = 0;
    Ntrials = NtrialsCollect[currentBlock]
    nextTaskButton.onclick = samTask;
    nextTaskButton.style.display = 'none';

    // instructions first -----------------

    document.getElementById('task').style.display = 'none';
    
    document.getElementById('instructions').firstElementChild.innerHTML = "Game 1 Instructions"

    if (comprehensionAttemptsH > 1) {document.getElementById('instructions').firstElementChild.innerHTML += "<br> You answered one or more questions incorrectly. Please read the instructions again and try again."}

    startPracticeButton.innerHTML = "Start Practice Round";
    startPracticeButton.style.display = 'block';

    document.getElementById('instructions').style.display = 'block';
    document.getElementById('instructionText').innerHTML = "In this game, you will choose between two slot machines that give different average rewards. Before making your choice, you will have to make 4 choices that we pre-determined. You will see which machine is highlighted and have to choose the highlighted machine."+
    " <br> <br> After these 4 intial pre-determined choices, you get to make either 1 or 5 free choices. You can see how many choices you can make under 'Trials left in this round'. You will play " + (Nblocks-1)+" rounds of this game. <br> <br> Click the button below to start a practice round.";
    

    // start practice --------------------------

    startPracticeButton.onclick = function() {
        console.log("started practice Horizon task")
        // display slot machines
        document.getElementById('task').style.display = 'block';
        startPracticeButton.style.display = 'none';
        document.getElementById("mach_div3").style.display = 'none'; // we only have 2 arms here
        document.getElementById("mach_div4").style.display = 'none';
        // fixed choice
        document.getElementById('machine_title').innerHTML = 'Select the machine that is highlighted.';

        fixedChoices = fixedChoicesCollect[currentBlock]

        if (fixedChoices[trial] == 0) {
            machine1.style.display = 'inline-block';
            machine2.style.display = 'inline-block';
            machine2.style.opacity = 0.5;

        } else {
            machine2.style.display = 'inline-block';
            machine1.style.display = 'inline-block';
            machine1.style.opacity = 0.5;
        }

        document.getElementById('score').innerHTML ='Score this round:' + scoreThisBlock+ '<br> Total score: ' + totalScore + "<br> Trials left in this round: " + (Ntrials - trial)
        
        // inputs to clickMachine function: machine, trial, currentBlock, rewards, Nblocks, Ntrials, fixedChoices
        //machButton1.addEventListener('click', clickMachine(0, trial, currentBlock, rewards, Nblocks, Ntrials, fixedChoices));
        //machButton2.addEventListener('click', clickMachine(1, trial, currentBlock, rewards, Nblocks, Ntrials, fixedChoices));

        machButton1.onclick =  function(){clickMachine(0, "horizon")};
        machButton2.onclick = function(){clickMachine(1, "horizon")};




    }

    
}


// Sam's task

function samTask() {
    nextTaskButton.style.display = 'none';
    console.log("started sam task")
    // rewards here are a list of lists of lists bc we need to allow for the drifting rewards
        // load rewards or quickly get some place holder rewards
    if (quickrunthrough == 1) { 
        Nblocks = 3; 
        rewardCollect = Array(Nblocks).fill([[20, 30], [20, 30], [20, 30], [20, 30], [20, 30]])
    }
    else {
        rewardCollect = samRewards
        Nblocks = rewardCollect.length;
    }
    //console.log("I got here 1")
    fixedChoicesCollect = Array(Nblocks).fill(0);
    NtrialsCollect = Array(Nblocks).fill(rewardCollect[1].length); //TODO: here is where to change number of trials
    currentBlock = 0;
    trial = 0;
    Ntrials = NtrialsCollect[currentBlock]

    // instructions first -----------------
    //console.log("I got here 2")
    document.getElementById('task').style.display = 'none';
    //console.log("I got here 3")
    document.getElementById('instructions').firstElementChild.innerHTML = "Game 2 Instructions"
    //console.log("I got here 4")
    if (comprehensionAttemptsS > 1) {document.getElementById('instructions').firstElementChild.innerHTML += "<br> You answered one or more questions incorrectly. Please read the instructions again and try again."}
    //console.log("I got here 5")
    startPracticeButton.innerHTML = "Start Practice Round";
    startPracticeButton.style.display = 'block';
    //console.log("I got here 6")
    document.getElementById('instructions').style.display = 'block';
    document.getElementById('instructionText').innerHTML = "In this game you will choose between two slot machines that give different average rewards. Sometimes, the average rewards for one or both of the machines changes over time. You can choose either machine at any time. You will play "+ (Nblocks-1)+
    " rounds of this game consisting of "+ Ntrials+" choices each. <br> <br> Click the button below to start a practice round.";
   // console.log("I got here 7")

    // start practice --------------------------------

    startPracticeButton.onclick = function() {
        
        console.log("started practice round sam")
        // display slot machines
        
        document.getElementById('task').style.display = 'block';
        slot_machines.style.display = 'flex';
        machine1.style.display = 'inline-block';
        machine2.style.display = 'inline-block';
        document.getElementById("mach_div3").style.display = 'none'; // we only have 2 arms here
        document.getElementById("mach_div4").style.display = 'none';
        
        
        document.getElementById('machine_title').innerHTML = 'Select the slot machine you would like to play!';
    
        fixedChoices = fixedChoicesCollect[currentBlock] // just bc we need this to make it fit with the horizon task
        document.getElementById('score').innerHTML ='Score this round:' + scoreThisBlock+ '<br> Total score: ' + totalScore + "<br> Trials left in this round: " + (Ntrials - trial)
        
        // inputs to clickMachine function: machine, trial, currentBlock, rewards, Nblocks, Ntrials, fixedChoices
        //machButton1.addEventListener('click', clickMachine(0, trial, currentBlock, rewards, Nblocks, Ntrials, fixedChoices));
        //machButton2.addEventListener('click', clickMachine(1, trial, currentBlock, rewards, Nblocks, Ntrials, fixedChoices));

        machButton1.onclick =  function(){clickMachine(0, "sam")};
        machButton2.onclick = function(){clickMachine(1, "sam")};

        nextTaskButton.onclick = restlessTask;

    }


}



// restless 4-armed bandit

function restlessTask() {
    nextTaskButton.style.display = 'none';
    console.log("started restless task")
    // rewards here are a list of lists bc the rewards do drift but there is only 1 block so there are Ntrials lists containing 4 rewards (1 for each arm)
    rewardCollect = [[20,20, 20, 20], [20,21, 19, 20], [21,20,20, 21], [24, 23,18, 22], [26, 24,16, 25], [28, 25,14, 28], [30, 26,12, 31], [32, 27,10, 34], [34, 27,10, 37], [36, 28, 8, 40]] //! place holder
    Nblocks = 1; // here we just have 1 block
    fixedChoicesCollect = Array(Nblocks).fill(0);
    NtrialsCollect = Array(Nblocks).fill(10); //TODO: increase number of trials (this is the equivalent of the R code rep(5, Nblocks))
    currentBlock = 0;
    trial = 0;
    Ntrials = NtrialsCollect[currentBlock]

    // instructions first -----------------

    document.getElementById('task').style.display = 'none';

    document.getElementById('instructions').firstElementChild.innerHTML = "Game 3 Instructions"
    if (comprehensionAttemptsR > 1) {document.getElementById('instructions').firstElementChild.innerHTML += "<br> You answered one or more questions incorrectly. Please read the instructions again and try again."}

    startPracticeButton.innerHTML = "Start Practice Round";
    startPracticeButton.style.display = 'block';

    document.getElementById('instructions').style.display = 'block';
    document.getElementById('instructionText').innerHTML = "In this game you will choose between four slot machines that give different average rewards. Importantly, the average reward of each slot machine changes over time. You can choose any machine at any time. In this game, you will only play one round consisting of "+ 
    Ntrials+" choices. <br> <br> Click the button below to start a practice round.";


    // start practice --------------------------------

    startPracticeButton.onclick = function() {

        // display slot machines
        slot_machines.style.display = 'flex';
        document.getElementById('task').style.display = 'block';
        machine1.style.display = 'inline-block';
        machine2.style.display = 'inline-block';
        machine3.style.display = 'inline-block';
        machine4.style.display = 'inline-block';


        document.getElementById('machine_title').innerHTML = 'Select the slot machine you would like to play!';

        fixedChoices = fixedChoicesCollect[currentBlock] // just bc we need this to make it fit with the horizon task

        document.getElementById('score').innerHTML ='Score this round:' + scoreThisBlock+ '<br> Total score: ' + totalScore + "<br> Trials left in this round: " + (Ntrials - trial)
        
        // inputs to clickMachine function: machine, trial, currentBlock, rewards, Nblocks, Ntrials, fixedChoices
        //machButton1.addEventListener('click', clickMachine(0, trial, currentBlock, rewards, Nblocks, Ntrials, fixedChoices));
        //machButton2.addEventListener('click', clickMachine(1, trial, currentBlock, rewards, Nblocks, Ntrials, fixedChoices));

        machButton1.onclick =  function(){clickMachine(0, "restless")};
        machButton2.onclick = function(){clickMachine(1, "restless")};
        machButton3.onclick = function(){clickMachine(2, "restless")};
        machButton4.onclick = function(){clickMachine(3, "restless")};

    }

}



// instructions


document.getElementById('instructions').style.display = 'block';
document.getElementById('task').style.display = 'none';
document.getElementById('questionnaire').style.display = 'none';
document.getElementById('instructionText').innerHTML= "Welcome to the experiment! <br> <br> In this experiment, you will have to choose between playing different slot machines across a series of games. <br> <br> Each slot machine gives you a different average reward. <br> <br> "+
"Your goal is to earn as many points as possible. <br> <br> You will play 3 different games. Each game consists of several rounds, in which the slot machines have different rewards. Thus, if a new round starts, the average rewards of the slot machines have changed and you need to learn them again!"+
" You know how many clicks you have left in a round through the 'trials left this round' indicator. <br>" +
"Below, you can see what an example game looks like: [insert screenshot]" //TODO: add screenshot

var horizonInstructions = "In this game, you will choose between two slot machines that give different average rewards. Before making your choice, you will have to make 4 choices that we pre-determined. You will see which machine is highlighted and have to choose the highlighted machine."+
" <br> <br> After these 4 intial pre-determined choices, you get to make either 1 or 5 free choices. You can see how many choices you can make under 'Trials left in this round'. You will play"+ (Nblocks-1)+"  rounds of this game. <br> <br> Click the button below to start a practice round.";

var samInstructions = "In this game you will choose between two slot machines that give different average rewards. Sometimes, the average rewards for one or both of the machines changes over time. You can choose either machine at any time. You will play "+ (Nblocks-1)+" rounds of this game consisting of "+ Ntrials+" choices each. <br> <br> Click the button below to start a practice round.";
var restlessInstructions = "In this game you will choose between four slot machines that give different average rewards. Importantly, the average reward of each slot machine changes over time. You can choose any machine at any time. In this game, you will only play one round consisting of "+ Ntrials+" choices. <br> <br> Click the button below to start a practice round.";



// comprehension questions -----------------------------

function createQuestion(questionnaireName, questionData) { // function from Toby's questionnaire code
    // This function creates an individual item
  
    var f = document.createElement("form");
    f.setAttribute('method',"post");
    f.setAttribute('action',"submit.php");
    f.setAttribute('id', questionnaireName.concat('_' + questionData.qNumber.toString()));
    f.setAttribute("name", "form_");
  
    var fieldset = document.createElement("fieldset");
    fieldset.setAttribute("class", "form__options");
    fieldset.setAttribute('id', questionnaireName.concat('_' + questionData.qNumber.toString()));
    fieldset.setAttribute("name", "fs_");
  
    var legend = document.createElement("legend");
    legend.setAttribute("class", "form__question");
    legend.setAttribute("name", "legend");
    legend.append(questionData.prompt);
  
    fieldset.appendChild(legend);
  
    var labels = [];
    var i
    for (i = 0; i < questionData.labels.length; i++) {
  
        var p = document.createElement("p");
        p.setAttribute('class', 'form__answer');
        var c = document.createElement("input");
        c.type = "radio";
        c.id = questionnaireName.concat(questionData.qNumber.toString()).concat("answer".concat(i.toString()));
        c.name = "question";
        c.value = i;
  
        var l = document.createElement("label");
        l.setAttribute('for', c.id);
        l.append(questionData.labels[i]);
  
        p.appendChild(c);
        p.appendChild(l);
  
        labels.push(p);
  
        fieldset.appendChild(p)
  
    }
  
    f.appendChild(fieldset);
  
  
    return f;
  
}


function checkComprehension(task){
    console.log("checking comprehension")
  
    var inputs = document.getElementsByName("fs_");
  
      // Loop through the items nad get their values
    var values = {};
    var incomplete = [];
    var i
      for (i = 0; i < inputs.length; i++) {
  
          if (inputs[i].id.length > 0) {
              var id
              // Get responses to questionnaire items
              id = inputs[i].id;
              var legend = inputs[i].querySelectorAll('[name="legend"]')[0];
  
              var checked = inputs[i].querySelector('input[name="question"]:checked');
  
              if (checked != null) {
                  legend.style.color = "#000000";
                 var value = checked.value;
                  values[id] = value;
              }else {
                  legend.style.color = "#ff0000";
                  incomplete.push(id);
              }
          }
            
      }
        
      
      // This checks for any items that were missed and scrolls to them
      if (incomplete.length > 0) {
  
          $('html, body').animate({ // go to first missed items
                  scrollTop: $(document.getElementById(incomplete[0])).offset().top - 100
                  }, 400);
         
  
          if(incomplete.length > 1){ // if you missed more than one item
             
              for (i = 0; i < incomplete.length -1; i++){ // loops through all missed questions and attaches an event listener to each of them
              
              $(document.getElementById(incomplete[i])).children().click(function (e) { 
                  var target = e.target.parentElement.parentElement.parentElement.id // name of the given question
                  var n = incomplete.indexOf(target)// I can't simply use i as the index as it is already done with the loop by the time one clicks
                  var nextMiss = document.getElementById(incomplete[n+1])
                  $('html, body').animate({ // go to next question
                  scrollTop: $(nextMiss).offset().top - 100
                  }, 400);
              });
  
              }
          }
  
          
  
       // everything filled in   
      } else if (task == "horizon" & (values["Q1_0"] == "1" && values["Q2_1"] == "0" && values["Q3_2"] == "1" && values["Q4_3"] == "0")|
                task == "sam" & (values["Q1_0"] == "2" && values["Q2_1"] == "1" && values["Q3_2"] == "2" && values["Q4_3"] == "0")|
                task == "restless" & (values["Q1_0"] == "1" && values["Q2_1"] == "2" && values["Q3_2"] == "2" && values["Q4_3"] == "2")) {
        understood = true
        
        // hide quiz
        $(document.getElementById("questionnaire")).hide()
        $(startTaskButton).hide()
  
        window.scrollTo(0,0);
        
      } else {
  
        // hide quiz
        $(document.getElementById("questionnaire")).hide()
        $(startTaskButton).hide()
  
        window.scrollTo(0,0);

        // set everything to beginning
        if (task == "horizon") {comprehensionAttemptsH +=1; horizonTask()} else if (task == "sam") {comprehensionAttemptsS += 1; samTask()} else {comprehensionAttemptsR += 1; restlessTask()}
        
      }
  
}

function startComprehension(task){

    document.getElementById('instructions').style.display = 'none';
    document.getElementById('task').style.display = 'none';
    document.getElementById("questionnaire").style.display = 'block';

    window.scrollTo(0, 0);


    //! this appends the new questions to the previous ones. 
    // TODO: fix that!
    // basically this is supposed to insert different questions for the different tasks

    if (task == "horizon") {
        var q1Data = {
            qNumber: 0,
            prompt: "How many choices can you make in each round?",
            labels: ['5', 'Sometimes 5, sometimes 10. Indicated by number of trials left', '10']
          };
          var q2Data = {
            qNumber: 1,
            prompt: "What happens when you play the same slot machine several times?",
            labels: ['You will get approximately the same number of points.', 'The number of points you get will gradually decrease.', 'The number of points you get will gradually increase.']
          };
          
          var q3Data = {
            qNumber: 2,
            prompt: "Can you choose freely which slot machine to play?",
            labels: ['No, I always select the highlighted one.', 'I select the highlighted one for the first 4 choices, then I can choose freely.', 'Yes, I can choose freely at any time.']
          };
          
          var q4Data = {
            qNumber: 3,
            prompt: "Do the points you get for each slot machine change between rounds?",
            labels: ['Yes. New round = completely new machines', 'No. They stay the same', 'They sometimes change, sometimes stay the same.']
          };

    } else if (task == "sam") {
        var q1Data = {
            qNumber: 0,
            prompt: "How many choices can you make in each round?",
            labels: ['5', 'Sometimes 5, sometimes 10. Indicated by number of trials left', '10']
          };
          var q2Data = {
            qNumber: 1,
            prompt: "Do the points you get for each slot machine change over time?",
            labels: ['No they stay the same on average.', 'Sometimes they change over time for one or both slot machines.', 'They change over time for both slot machines.']
          };
          
          var q3Data = {
            qNumber: 2,
            prompt: "Can you choose freely which slot machine to play?",
            labels: ['No, I always select the highlighted one.', 'I select the highlighted one for the first 4 choices, then I can choose freely.', 'Yes, I can choose freely at any time.']
          };
          
          var q4Data = {
            qNumber: 3,
            prompt: "Do the points you get for each slot machine change between rounds?",
            labels: ['Yes. New round = completely new machines', 'No. They stay the same', 'They sometimes change, sometimes stay the same.']
          };

    } else if (task == "restless") {
        var q1Data = {
            qNumber: 0,
            prompt: "How many choices can you make in each round?",
            labels: ['5', 'There is only one round with 200 choices', '10']
          };
          var q2Data = {
            qNumber: 1,
            prompt: "Do the points you get for each slot machine change over time?",
            labels: ['No they stay the same on average.', 'Sometimes they change over time for one or several slot machines.', 'They change over time for all slot machines.']
          };
          
          var q3Data = {
            qNumber: 2,
            prompt: "Can you choose freely which slot machine to play?",
            labels: ['No, I always select the highlighted one.', 'I select the highlighted one for the first 4 choices, then I can choose freely.', 'Yes, I can choose freely at any time.']
          };
          
          var q4Data = {
            qNumber: 3,
            prompt: "Do the points you get for each slot machine change between rounds?",
            labels: ['Yes. New round = completely new machines', 'No. They stay the same', 'There is only one round.']
          };
    }

    var Q1 = createQuestion('Q1', q1Data);
    var Q2 = createQuestion('Q2', q2Data);
    var Q3 = createQuestion('Q3', q3Data);
    var Q4 = createQuestion('Q4', q4Data);
    document.getElementById('questionnaire').appendChild(Q1);
    document.getElementById('questionnaire').appendChild(Q2);
    document.getElementById('questionnaire').appendChild(Q3);
    document.getElementById('questionnaire').appendChild(Q4);
    console.log(Q1)


    startTaskButton.style.display = 'block';
    startTaskButton.onclick = function() {
        
        checkComprehension(task)

        trial = 0;
        scoreThisBlock = 0;
        Ntrials = NtrialsCollect[currentBlock]
        document.getElementById("questionnaire").style.display = 'none';
        document.getElementById('task').style.display = 'block';

        if (task == "horizon") {
            document.getElementById('machine_title').innerHTML = 'Select the machine that is highlighted.';

            fixedChoices = fixedChoicesCollect[currentBlock]

            if (fixedChoices[trial] == 0) {
                machine1.style.display = 'inline-block';
                machine2.style.display = 'inline-block';
                machine2.style.opacity = 0.5;

            } else {
                machine2.style.display = 'inline-block';
                machine1.style.display = 'inline-block';
                machine1.style.opacity = 0.5;
            }

            document.getElementById('score').innerHTML ='Score this round:' + scoreThisBlock+ '<br> Total score: ' + totalScore + "<br> Trials left in this round: " + (Ntrials - trial)

        }

        

    }



}





/*



// comprehension questions -----------------------------
function createQuestion(questionnaireName, questionData) { // function from Toby's questionnaire code
    // This function creates an individual item
  
    var f = document.createElement("form");
    f.setAttribute('method',"post");
    f.setAttribute('action',"submit.php");
    f.setAttribute('id', questionnaireName.concat('_' + questionData.qNumber.toString()));
    f.setAttribute("name", "form_");
  
    var fieldset = document.createElement("fieldset");
    fieldset.setAttribute("class", "form__options");
    fieldset.setAttribute('id', questionnaireName.concat('_' + questionData.qNumber.toString()));
    fieldset.setAttribute("name", "fs_");
  
    var legend = document.createElement("legend");
    legend.setAttribute("class", "form__question");
    legend.setAttribute("name", "legend");
    legend.append(questionData.prompt);
  
    fieldset.appendChild(legend);
  
    var labels = [];
    var i
    for (i = 0; i < questionData.labels.length; i++) {
  
        var p = document.createElement("p");
        p.setAttribute('class', 'form__answer');
        var c = document.createElement("input");
        c.type = "radio";
        c.id = questionnaireName.concat(questionData.qNumber.toString()).concat("answer".concat(i.toString()));
        c.name = "question";
        c.value = i;
  
        var l = document.createElement("label");
        l.setAttribute('for', c.id);
        l.append(questionData.labels[i]);
  
        p.appendChild(c);
        p.appendChild(l);
  
        labels.push(p);
  
        fieldset.appendChild(p)
  
    }
  
    f.appendChild(fieldset);
  
  
    return f;
  
}
  
var createComprehension = function(){
  var q1Data = {
    qNumber: 0,
    prompt: "What is the aim of this game?",
    labels: ['To find the kraken.', 'To click on every square at least once.', 'To catch as many fish as possible.']
  };
  var q2Data = {
    qNumber: 1,
    prompt: "What happens when you click the same square several times?",
    labels: ['You will get approximately the same number of fish.', 'The number of fish you get will gradually decrease.', 'You will only get fish the first time, afterwards the fish in that square are gone.']
  };
  
  var q3Data = {
    qNumber: 2,
    prompt: "How do you know how many fish to expect in one location?",
    labels: ['There is no way to know.', 'The lower half of the ocean has more fish.', 'The number of fish in nearby squares is similar.']
  };
  
  var q4Data = {
    qNumber: 3,
    prompt: "What happens when you find the kraken?",
    labels: ['It is the end of the experiment.', 'The round is over and you lose all the fish you collected in that round.', 'You get extra points']
  };
  
  var q5Data = {
    qNumber: 4,
    prompt: "When can you expect to find the kraken?",
    labels: ['When you click a square more than once.', 'At any moment, you have no control over this.', 'When you click a square with less than 45 fish.']
  };
  
  var q6Data = {
    qNumber: 5,
    prompt: "Does the number of fish in each square change from one block to the next?",
    labels: ['No, each block is about the same ocean.', 'Yes, each block is a completely new ocean.', 'The oceans only change a little bit between blocks.']
  };
  
  var q7Data = {
    qNumber: 6,
    prompt: "Where in the ocean is the square with the most fish?",
    labels: ['In a second patch of fish, not the one I start at.', 'Close to my starting square.', 'At the center of the the ocean.']
  };
  
  var q8Data = {
    qNumber: 7,
    prompt: "What is the highest number of fish a square can have in each ocean?",
    labels: ['Around 60.', 'Around 120.', 'Around 200.']
  };
  
  
  var Q1 = createQuestion('Q1', q1Data);
  var Q2 = createQuestion('Q2', q2Data);
  var Q3 = createQuestion('Q3', q3Data);
  var Q4 = createQuestion('Q4', q4Data);
  var Q5 = createQuestion('Q5', q5Data);
  var Q6 = createQuestion('Q6', q6Data);
  var Q7 = createQuestion('Q7', q7Data);
  var Q8 = createQuestion('Q8', q8Data);
  document.getElementById('quiz').appendChild(Q1);
  document.getElementById('quiz').appendChild(Q2);
  document.getElementById('quiz').appendChild(Q3);
  document.getElementById('quiz').appendChild(Q4);
  document.getElementById('quiz').appendChild(Q5);
  document.getElementById('quiz').appendChild(Q6);
  document.getElementById('quiz').appendChild(Q7);
  document.getElementById('quiz').appendChild(Q8);
  
    // create submit buton
    var submit = document.getElementById("submitComp")
    $(submit).show()
    $(document.getElementById("submitContainer")).show()
    submit.addEventListener("mouseup", checkComprehension)
    // document.getElementById('quizContainer').appendChild(submit)
  
}
  
  
  // when submitting comprehension questions
  
function checkComprehension(){
  
  
    var inputs = document.getElementsByName("fs_");
  
      // Loop through the items nad get their values
    var values = {};
    var incomplete = [];
    var i
      for (i = 0; i < inputs.length; i++) {
  
          if (inputs[i].id.length > 0) {
              var id
              // Get responses to questionnaire items
              id = inputs[i].id;
              var legend = inputs[i].querySelectorAll('[name="legend"]')[0];
  
              var checked = inputs[i].querySelector('input[name="question"]:checked');
  
              if (checked != null) {
                  legend.style.color = "#000000";
                 var value = checked.value;
                  values[id] = value;
              }else {
                  legend.style.color = "#ff0000";
                  incomplete.push(id);
              }
          }
            
      }
        
      
      // This checks for any items that were missed and scrolls to them
      if (incomplete.length > 0) {
  
          $('html, body').animate({ // go to first missed items
                  scrollTop: $(document.getElementById(incomplete[0])).offset().top - 100
                  }, 400);
         
  
          if(incomplete.length > 1){ // if you missed more than one item
             
              for (i = 0; i < incomplete.length -1; i++){ // loops through all missed questions and attaches an event listener to each of them
              
              $(document.getElementById(incomplete[i])).children().click(function (e) { 
                  var target = e.target.parentElement.parentElement.parentElement.id // name of the given question
                  var n = incomplete.indexOf(target)// I can't simply use i as the index as it is already done with the loop by the time one clicks
                  var nextMiss = document.getElementById(incomplete[n+1])
                  $('html, body').animate({ // go to next question
                  scrollTop: $(nextMiss).offset().top - 100
                  }, 400);
              });
  
              }
          }
  
          
  
          
      } else if (values["Q1_0"] == "2" && values["Q2_1"] == "0" && values["Q3_2"] == "2" && values["Q4_3"] == "1" && values["Q5_4"] == "2" && values["Q6_5"] == "1" && values["Q7_6"] == "0" && values["Q8_7"] == "1") {
        understood = true
        // close instruction stuff
        instructionContent.innerHTML = '';
        instructionHeading.innerHTML = '';
        instructionContainer.style.height = '15px';
        instructionContainer.style.minHeight = '15px';
        instructionsOpen = false
        training_done = true
  
        // get everything ready to start the task
        createOkButton()
        //var okButton = document.getElementById("ok")
       // okButton.setAttribute("class", "button");
        $(document.getElementById("quizContainer")).hide()
        $(document.getElementById("quiz")).hide()
        $(document.getElementById("submitContainer")).hide()
        $(document.getElementById("submitComp")).hide()
  
        askInfo()
        // runTask(grids[envOrder[currentBlock - 1]], 50, blocks[currentBlock - 1]);
        // $(document.getElementById("krakenPresent")).show()
        window.scrollTo(0,0);
        
      } else {
  
        comprehensionAttempts +=1
        // set everything to beginning
        var ocean = document.getElementById("ocean")
        $(ocean).remove() // int() creates a new one
        document.getElementById("credit").remove()// same here
        training_iteration = 0
        currentBlock = 0
        training_clickable = true;
        training_done = false;
        training_caught = false;
        instructionsOpen = true;
        training_clicks = 0;
        
        init()
        
        $(document.getElementById("quizContainer")).hide()
        $(document.getElementById("quiz")).hide()
        $(document.getElementById("submitContainer")).hide()
        $(document.getElementById("submitComp")).hide()
        window.scrollTo(0, 0);
  
        // create button again bc it was removed with the ocean
        createOkButton()
      }
  
}
  
  
  
function startComprehension(){
    // hide stuff
    $(document.getElementById("ocean")).hide()
    $(document.getElementById("ok")).hide()
    $(document.getElementById("credit")).hide()
    removeChilds(instructions)
    instructions.innerHTML = "<h2>Before you start the game, please answer some questions to see whether you understood everything correctly.</h2>";
    $(document.getElementById("quizContainer")).show()
    $(document.getElementById("quiz")).show()
    if (comprehensionAttempts == 1){ // if this is the first attempt, create the questions
      createComprehension()
    } else { // if it isnt, no need to create them but do need to show the button that was previously hidden
      $(document.getElementById("submitContainer")).show()
      $(document.getElementById("submitComp")).show()
    }
   window.scrollTo(0, 0);
  
  //  var submit = document.createElement('button');
  //  submit.setAttribute("class", "submit_button");
  //  submit.setAttribute("type", "button");
  //  submit.setAttribute("id", "submit");
  
     
}


*/
// ----------------------------

// Event listeners for buttons
const startPracticeButton = document.getElementById('startPractice');
const startTaskButton = document.getElementById('startTask');
const endTaskButton = document.getElementById('endTask');
const nextTaskButton = document.getElementById('nextTask');

nextTaskButton.style.display = 'none';
endTaskButton.style.display = 'none';
startTaskButton.style.display = 'none';

startPracticeButton.onclick = function(){
    // Hide instructions and show practice trials
    
    startPracticeButton.style.display = 'none';
    horizonTask();

    // Call a function to display practice trials
}

startTaskButton.addEventListener('click', () => {
    // Hide practice trials and show task trials
    document.getElementById('practiceTrials').style.display = 'none';
    document.getElementById('task').style.display = 'block';
    endTaskButton.style.display = 'none';
    nextTaskButton.style.display = 'none';

    // Call a function to display task trials
});

endTaskButton.addEventListener('click', () => {
    // Hide task trials and display completion message

    setTimeout(function () { //KW: makes the text "next block" be displayed on the grid for 1s then disappear.
          window.location.href = "https://www.youtube.com/watch?v=dQw4w9WgXcQ" ;  //TODO: replace this with postquestionnaire page or prolific link
          }, 500)
    
});