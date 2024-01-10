// task code for exploration reliablity study

// coded by Kristin Witte and github copilot (and chatgpt)

// settings

var quickrunthrough = false // if true, we use placeholder rewards and only 2 blocks per task and print some stuff to the console

// variables we need
//var session = 1

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
var fixedChoicesCollectH
var NtrialsCollectH
var rewardCollect
var NtrialsCollect
var score = 0
var comprehensionAttemptsH = 1
var comprehensionAttemptsS = 1
var comprehensionAttemptsR = 1
var understood = true
var horizonRewards
var samRewards
var restlessRewards
var data = {}
var bonusPayment
var tic 
var toc
var clickable = 0
var task
data["restless"] = {}
data["restless"]["choice"] = {}
data["restless"]["reward"] = {}
data["restless"]["time"] = {}
data["restless"]["taskReward"] = 0



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


// function saveData(filedata){
//     console.log(data)
//     var filename = "../data/" + subjectID + "data_task_bonus_" + bonusPayment+ "_session_"+ session + ".txt";
//     $.post("./results_data.php", {postresult: filedata + "\n", postfile: filename })
  
// }

// function saveTemp(filedata){
//     console.log(data)
//     var filename = "../data/" + subjectID + "temp_data_task_bonus_" + bonusPayment+ "_session_"+ session + ".txt";
//     $.post("./results_data.php", {postresult: filedata + "\n", postfile: filename })
  
// }

function saveData(filedata) {
    var filename = "bandits/"+subjectID+"_data_task_bonus_" + bonusPayment+ "_session_"+ session + ".txt"
    var filename_folder = "../data/" + filename;
    $.post("save_data.php", { postresult: filedata + "\n", postfile: filename_folder })
}

async function saveTemp(filedata) {
    //console.log("saving temp")
    var filename = "bandits/" + subjectID + "_temp_data_task_session_"+ session + ".txt";
    var filename_folder = "../data/" + filename;
    $.post("save_data.php", { postresult: filedata + "\n", postfile: filename_folder })
}

function getQueryVariable(variable)
{
    var query = window.location.search.substring(1);
    var vars = query.split("&");
  
    for (var i=0;i<vars.length;i++) {
        var pair = vars[i].split("=");
        if(pair[0] == variable){return pair[1];}
    }
    return(false);
}

if (window.location.search.indexOf('PROLIFIC_PID') > -1) {
    var subjectID = getQueryVariable('PROLIFIC_PID');
} else {
    var subjectID = "test-" + Math.random().toString(36).substring(7);
    //console.log(subjectID)
}

function keyDownHandler(e, task) {
    if (clickable == 1) {
        if (task != "restless") {
        if (e.key === 's') {
            clickMachine(0, task)
        } else if (e.key == 'k') {
            clickMachine(1, task)

        }

    } else {
        if (e.key === 's') {
            clickMachine(0, task)
        } else if (e.key == 'd') {
            clickMachine(1, task)
        } else if (e.key == 'k') {
            clickMachine(2, task)
        } else if (e.key == 'l') {
            clickMachine(3, task)
        }

    }
}
}

async function handle_operation_keypress(e) {
 
        if (clickable == 1) {
            //document.removeEventListener("keydown", handle_operation_keypress, false);
            if (task != "restless") {
            if (e.key === 's') {
                clickMachine(0, task)
            } else if (e.key == 'k') {
                clickMachine(1, task)
    
            }
    
        } else {
            if (e.key === 's') {
                clickMachine(0, task)
            } else if (e.key == 'd') {
                clickMachine(1, task)
            } else if (e.key == 'k') {
                clickMachine(2, task)
            } else if (e.key == 'l') {
                clickMachine(3, task)
            }
    
        }
    }

    }

// randomly select which order the tasks are in
var order = Math.floor(Math.random() * 6) + 1;
//order = 1
var tasks

tasks = [restlessTask]

var task_ind = 0
data["taskOrder"] = order

var session = 0
// turn session into integer and add 1
var s = parseInt(session)+1

// pre-load rewards
$.getJSON("rewardsHorizon"+ s+".json", function(data) {
    horizonRewards = data;
  })
$.getJSON("fixedChoices"+ s+".json", function(data) {
    fixedChoicesCollectH = data;
  })

$.getJSON("Horizon"+ s+".json", function(data) {
    NtrialsCollectH = data;
  })

$.getJSON("rewardsSam"+ s+".json", function(data) {
    samRewards = data;
  })
$.getJSON("rewards4ARB"+ s+".json", function(data) {
    restlessRewards = data;
  })

// click function
var clickMachine = function(machine, task) {
    // input:
    // machine: which machine was clicked (0-3)
    // task: which task are we in (horizon, sam, restless)

    toc = Number(new Date())
    
    // get variables ---------------
    rewards = rewardCollect[currentBlock][trial]
    Ntrials = NtrialsCollect[currentBlock]
    fixedChoices = fixedChoicesCollect[currentBlock]
    if (quickrunthrough){console.log("clicked machine " + machine  + " in task " +task )}
    
        // was this a machine they were allowed to chlick in the horizon task? Do we still have trials left this round?
    if (((trial > 3 | task != "horizon") | machine == fixedChoices[trial])& Ntrials - trial > 0) { // if (either not a fixed choice OR not Horizon task OR they clicked the right one) AND trials left


        // get rewards and display them-----------------------------
    // display reward
    var reward = rewards[machine]
    if (machine == 0) {
        document.getElementById("machine1").innerHTML = reward + " points";
        // remove score after 1 second
        setTimeout(function() {
            document.getElementById("machine1").innerHTML = "";
        }, 1000)

    } else if (machine == 1) { 
        document.getElementById("machine2").innerHTML = reward + " points";
        // remove score after 1 second
        setTimeout(function() {
            document.getElementById("machine2").innerHTML = "";
        }, 1000)
   
    }else if (machine == 2) { 
        document.getElementById("machine3").innerHTML = reward + " points";
        // remove score after 1 second
        setTimeout(function() {
            document.getElementById("machine3").innerHTML = "";
        }, 1000)
   
    } else if (machine == 3) { 
        document.getElementById("machine4").innerHTML = reward + " points";
        // remove score after 1 second
        setTimeout(function() {
            document.getElementById("machine4").innerHTML = "";
        }, 1000)
   
    }

    // save data
    if (trial == 0) {
        data[task]["choice"][currentBlock] = {}
        data[task]["reward"][currentBlock] = {}
        data[task]["time"][currentBlock] = {}
    }
    data[task]["choice"][currentBlock][trial] = machine
    data[task]["reward"][currentBlock][trial] = reward
    data[task]["time"][currentBlock][trial] = toc - tic
    if (currentBlock > 0) {data[task]["taskReward"] += reward}
    if (task == "restless" & trial%5 == 0){saveTemp(JSON.stringify(data))}


    

    // increment trial
    trial += 1

    if (trial < Ntrials) {tic = Number(new Date()); } // timing for next click
    
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
    
    // if this is the Horizon task and the fixed choices just finished, take a little break and remind them how many free choices they get
    if (trial == 4 & task == "horizon") {
        clickable = 0
        setTimeout(function() {
        slot_machines.style.display = "none";}, 1000)
        if (Ntrials == 5){
            document.getElementById('machine_title').innerHTML = '<b>Short round.</b> You can now make <b>one</b> free choice. <br> Press S for the left machine and K for the right machine.'; 
        } else {
            document.getElementById('machine_title').innerHTML = '<b>Long round.</b> You can now make <b>6</b> free choices. <br> Press S for the left machine and K for the right machine.';
        }

        setTimeout(function() {
            slot_machines.style.display = "flex";
            clickable = 1

            //document.getElementById('machine_title').innerHTML = 'Select the slot machine you would like to play! Round '+ currentBlock +' of ' + (Nblocks-1) +
             //   '<br> Press S for the left machine and K for the right machine.';
            
        }, 3000)
        
    }

    //if (trial == 4 & task == "horizon" & Ntrials == 5) {document.getElementById('machine_title').innerHTML = '<b>Short round.</b> Select the slot machine you would like to play!<br>Press';}
    //else if (trial == 4 & task == "horizon" & Ntrials == 10){document.getElementById('machine_title').innerHTML = '<b>Long round.</b> Select the slot machine you would like to play!';}
    scoreThisBlock = scoreThisBlock + reward;
    if (currentBlock > 0) {totalScore = totalScore + reward}

    // update score div
    document.getElementById('score').innerHTML ='Score this round:' + scoreThisBlock+ '<br> Total score: ' + totalScore + "<br> Trials left in this round: " + (Ntrials - trial)
    

    // if no trials left -----------------------------------------
    if (trial == Ntrials) { 
        currentBlock += 1;
        clickable = 0

        // make it really not clickable anymore, otherwise have 2 event listeners later
        document.removeEventListener("keydown", handle_operation_keypress, false);

        // save data
        saveTemp(JSON.stringify(data))

        // was this a practice round?
        if (currentBlock == 1) { // practice round

            // go to comprehension
            setTimeout(function() {
            startComprehension(task)
            }, 1500)


        } else if (currentBlock == Nblocks) { // game over
            scoreThisBlock = 0;
            
            // study over
            slot_machines.style.display = 'none';
            document.getElementById('machine_title').innerHTML = "The game is over. Please click the button below to return to Prolific."
            document.getElementById('score').innerHTML ='<br> Total score: ' + totalScore 
            endTaskButton.style.display = 'block';

            // save data
            data["totalScore"] = totalScore
            data["bonusPayment"] = bonusPayment
            data["subjectID"] = subjectID
            data["session"] = session
            data["comprehensionAttemptsR"] = comprehensionAttemptsR
            
            if (quickrunthrough) {console.log(data)}

            saveData(JSON.stringify(data))
            //saveData(JSON.stringify([0,0]))

        

        } else { // go to next block

            setTimeout(function() {
                document.getElementById('machine_title').innerHTML = 'New round!';
                slot_machines.style.display = "none";
                // reset trial
                trial = 0;
                Ntrials = NtrialsCollect[currentBlock]
    
                //document.getElementById('score').innerHTML ='Score this round:' + scoreThisBlock+ '<br> Total score: ' + totalScore + "<br> Trials left in this round: " + (Ntrials - trial)
                scoreThisBlock = 0;
            } , 1500)
            

            setTimeout(function() {
                tic = Number(new Date())
                slot_machines.style.display = "flex";

                // update score
                document.getElementById('score').innerHTML ='Score this round:' + scoreThisBlock+ '<br> Total score: ' + totalScore + "<br> Trials left in this round: " + (Ntrials - trial)
                clickable = 1
                document.addEventListener("keydown", handle_operation_keypress, false);

                if (task == "horizon") { // for Horizon task, we are back to fixed choice
                    if (NtrialsCollect[currentBlock] == 5){document.getElementById('machine_title').innerHTML = '<b>Short round.</b> Select the highlighted slot machine. Round '+ (currentBlock) +' of ' + (Nblocks-1);} 
                    else {document.getElementById('machine_title').innerHTML = '<b>Long round.</b> Select the highlighted slot machine. Round '+ currentBlock +' of ' + (Nblocks-1);}

                    fixedChoices = fixedChoicesCollect[currentBlock]
                    if (fixedChoices[trial] == 0) {
                        machine1.style.opacity = 1;
                        machine2.style.opacity = 0.5;
                
                    } else {
                        machine1.style.opacity = 0.5;
                        machine2.style.opacity = 1;
                    }


                } else {
                    document.getElementById('machine_title').innerHTML = 'Select the slot machine you would like to play! Round '+ currentBlock +' of ' + (Nblocks-1) +
                    '<br> Press S for the left machine and K for the right machine.';
                }
                
            }, 4000)

            
            
        }

    }


    }

    
    
}

// the separate tasks -------------------

// horizon task
// function that implements the Horizon task

function horizonTask() {
    // set up variables
    tic = Number(new Date())
    task = "horizon"

    // only increment task if people are NOT coming back here from the comprehension questions
    if (understood) {task_ind += 1}

    // load rewards or quickly get some place holder rewards
    if (quickrunthrough) { 
        Nblocks = 2; 
        rewardCollect = Array(Nblocks).fill([[20, 30], [20, 30], [20, 30], [20, 30], [20, 30], [20, 30], [20, 30], [20, 30], [20, 30], [20, 30]] )
        fixedChoicesCollect = [[0,1,0,1], [0,0,0,1]] 
        NtrialsCollect = [5, 5] 
    }
    else {
        rewardCollect = horizonRewards

        // number of blocks is length of the list of Ntrials
        NtrialsCollect = NtrialsCollectH    
        Nblocks = NtrialsCollect.length;

        // had to do it like this so that the preloaded stuff does not get overwritten
        fixedChoicesCollect = fixedChoicesCollectH
        


    }

    currentBlock = 0;
    trial = 0;
    Ntrials = NtrialsCollect[currentBlock]
    nextTaskButton.onclick = tasks[task_ind]
    nextTaskButton.style.display = 'none';

    // instructions first -----------------

    document.getElementById('task').style.display = 'none';
    
    document.getElementById('instructions').firstElementChild.innerHTML = "Game "+task_ind+" Instructions"

    if (comprehensionAttemptsH > 1) {document.getElementById('instructions').firstElementChild.innerHTML += "<br> You answered one or more questions incorrectly. Please read the instructions again and try again."}

    startPracticeButton.innerHTML = "Start Practice Round";
    startPracticeButton.style.display = 'block';

    document.getElementById('instructions').style.display = 'block';
    document.getElementById('instructionText').innerHTML = "In this game, you will choose between two slot machines that give different average rewards. The average rewards for each machine stay the same within a round but there is some noise added. Before making your choice, you will have to make 4 choices that we pre-determined. You will see which machine is highlighted and have to choose the highlighted machine."+
    " <br> <br> After these 4 intial pre-determined choices, you get to make either 1 or 6 free choices. You can see how many choices you can make under 'Trials left in this round' as well as above the slot machines where it says 'Short round' when you can only make one free choice and 'Long round' when you can make 6 free choices. "+
    "To select a slot machine, press the 's' key for the left machine and the 'k' key for the right machine, as indicated above the machines.<br> <br>"
    "You will play " + (Nblocks-1)+" rounds of this game. <br> <br> Click the button below to start a practice round.";
    

    // start practice --------------------------

    startPracticeButton.onclick = function() {
        // make machines clickable with keyboard presses
        document.addEventListener("keydown", handle_operation_keypress, false);

        clickable = 1

        if(quickrunthrough){console.log("started practice Horizon task")} 
        toc = Number(new Date())
        data["horizon"]["instructionTime"] = toc - tic
        tic = Number(new Date())
        // display slot machines
        document.getElementById('task').style.display = 'block';
        document.getElementById('slot_machines').style.display = 'flex';
        startPracticeButton.style.display = 'none';
        // make sure the naming of the machines is correct in case the restless bandit happened before
        document.getElementById("mach_div2").firstElementChild.innerHTML = "Machine K";
        document.getElementById("mach_div3").style.display = 'none'; // we only have 2 arms here
        document.getElementById("mach_div4").style.display = 'none';
        // fixed choice
        if (Ntrials == 5) {
            document.getElementById('machine_title').innerHTML = '<b>Short round.</b> Select the machine that is highlighted. <br> Press S for the left machine and K for the right machine.';
        } else {
            document.getElementById('machine_title').innerHTML = '<b>Long round.</b> Select the machine that is highlighted. <br> Press S for the left machine and K for the right machine.';
        }
        

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


        // clicking machines with mouse keys
        // machButton1.onclick =  function(){clickMachine(0, "horizon")};
        // machButton2.onclick = function(){clickMachine(1, "horizon")};




    }

    
}


// Sam's task

function samTask() {
    task = "sam"
    // only increment task_ind if people are not coming back here from the comprehension questions
    if (understood) {task_ind += 1}
    nextTaskButton.style.display = 'none';
    if (quickrunthrough) {console.log("started sam task")}
    tic = Number(new Date())
    // rewards here are a list of lists of lists bc we need to allow for the drifting rewards
        // load rewards or quickly get some place holder rewards
    if (quickrunthrough) { 
        Nblocks = 3; 
        rewardCollect = Array(Nblocks).fill([[20, 30], [20, 30], [20, 30], [20, 30], [20, 30]])
    }
    else {
        rewardCollect = samRewards
        Nblocks = rewardCollect.length;
    }
    fixedChoicesCollect = Array(Nblocks).fill(0);
    NtrialsCollect = Array(Nblocks).fill(rewardCollect[1].length); 
    currentBlock = 0;
    trial = 0;
    Ntrials = NtrialsCollect[currentBlock]


    // instructions first -----------------
    document.getElementById('task').style.display = 'none';
    document.getElementById('instructions').firstElementChild.innerHTML = "Game "+task_ind+" Instructions"
    if (comprehensionAttemptsS > 1) {document.getElementById('instructions').firstElementChild.innerHTML += "<br> You answered one or more questions incorrectly. Please read the instructions again and try again."}
    startPracticeButton.innerHTML = "Start Practice Round";
    startPracticeButton.style.display = 'block';
    document.getElementById('instructions').style.display = 'block';
    document.getElementById('instructionText').innerHTML = "In this game you will choose between two slot machines that give different average rewards. Sometimes, the average rewards for one or both of the machines changes over time. You can choose either machine at any time. You will play "+ (Nblocks-1)+
    " rounds of this game consisting of "+ Ntrials+" choices each. <br>You select the slot machines using the S and the K keys on your keyboard.<br> Click the button below to start a practice round.";

    // start practice --------------------------------

    startPracticeButton.onclick = function() {
        toc = Number(new Date())
        data["sam"]["instructionTime"] = toc - tic
        if(quickrunthrough){console.log("started practice sam task")}
        // display slot machines
        tic = Number(new Date())
        
        document.getElementById('task').style.display = 'block';
        slot_machines.style.display = 'flex';
        machine1.style.display = 'inline-block';
        machine2.style.display = 'inline-block';
        // make sure the naming of the machines is correct in case the restless bandit happened before
        document.getElementById("mach_div2").firstElementChild.innerHTML = "Machine K";

        document.getElementById("mach_div3").style.display = 'none'; // we only have 2 arms here
        document.getElementById("mach_div4").style.display = 'none';
        
        
        document.getElementById('machine_title').innerHTML = 'Select the slot machine you would like to play!<br> Press S for the left machine and K for the right machine.';
    
        fixedChoices = fixedChoicesCollect[currentBlock] // just bc we need this to make it fit with the horizon task
        document.getElementById('score').innerHTML ='Score this round:' + scoreThisBlock+ '<br> Total score: ' + totalScore + "<br> Trials left in this round: " + (Ntrials - trial)
        
        // inputs to clickMachine function: machine, trial, currentBlock, rewards, Nblocks, Ntrials, fixedChoices
        //machButton1.addEventListener('click', clickMachine(0, trial, currentBlock, rewards, Nblocks, Ntrials, fixedChoices));
        //machButton2.addEventListener('click', clickMachine(1, trial, currentBlock, rewards, Nblocks, Ntrials, fixedChoices));

        // make machines clickable with keyboard presses
        document.addEventListener("keydown", handle_operation_keypress, false);
        
        clickable = 1

        // machButton1.onclick =  function(){clickMachine(0, "sam")};
        // machButton2.onclick = function(){clickMachine(1, "sam")};

        nextTaskButton.onclick = tasks[task_ind]

    }


}



// restless 4-armed bandit

function restlessTask() {
    task = "restless"
    // only increment task_ind if people are not coming back here from the comprehension questions
    if (understood) {task_ind += 1}
    nextTaskButton.onclick = tasks[task_ind]
    nextTaskButton.style.display = 'none';
    if (quickrunthrough) {console.log("started restless task")}
    tic = Number(new Date())

    document.getElementById("mach_div2").firstElementChild.innerHTML = "Machine D";
    document.getElementById("mach_div3").firstElementChild.innerHTML = "Machine K";
    document.getElementById("mach_div4").firstElementChild.innerHTML = "Machine L";

    // load rewards or quickly get some place holder rewards
    if (quickrunthrough) { 
        Nblocks = 2; 
        rewardCollect = Array(Nblocks).fill([[20, 30, 20, 30], [20, 30, 20, 30], [20, 30, 20, 30], [20, 30, 20, 30], [20, 30, 20, 30], [20, 30, 20, 30], [20, 30, 20, 30], [20, 30, 20, 30], [20, 30, 20, 30], [20, 3, 20, 300]] )
        NtrialsCollect = [10, 10]
    }
    else {
        rewardCollect = restlessRewards

        // number of blocks is length of the list of Ntrials    
        Nblocks = rewardCollect.length;
        NtrialsCollect = [10, 200, 200]
    }
    // rewards here are a list of lists bc the rewards do drift but there is only 1 block so there are Ntrials lists containing 4 rewards (1 for each arm)
    fixedChoicesCollect = Array(Nblocks).fill(0);
    currentBlock = 0;
    trial = 0;
    Ntrials = NtrialsCollect[currentBlock]

    // instructions first -----------------

    document.getElementById('task').style.display = 'none';

    document.getElementById('instructions').firstElementChild.innerHTML ="Game "+task_ind+" Instructions"
    if (comprehensionAttemptsR > 1) {document.getElementById('instructions').firstElementChild.innerHTML += "<br> You answered one or more questions incorrectly. Please read the instructions again and try again."}

    startPracticeButton.innerHTML = "Start Practice Round";
    startPracticeButton.style.display = 'block';

    document.getElementById('instructions').style.display = 'block';
    document.getElementById('instructionText').innerHTML = "In this game you will choose between four slot machines that give different average rewards. Importantly, the average reward of each slot machine changes over time."+
    " The rewards from all slot machines change over time so a slot machine that gives low rewards at first can give high rewards later on and vice-versa. You can choose any machine at any time. In this game, you will only play two rounds consisting of "+ 
    NtrialsCollect[1] +" choices. <br> You can select the machines using the S, D, K and L keys, as indicated above the slot machines. <br> Click the button below to start a practice round.";


    // start practice --------------------------------

    startPracticeButton.onclick = function() {
        toc = Number(new Date())
        data["restless"]["instructionTime"] = toc - tic
        tic = Number(new Date())
        // display slot machines
        slot_machines.style.display = 'flex';
        document.getElementById('task').style.display = 'block';
        machine1.style.display = 'inline-block';
        machine2.style.display = 'inline-block';
        machine3.style.display = 'inline-block';
        machine4.style.display = 'inline-block';


        document.getElementById('machine_title').innerHTML = 'Select the slot machine you would like to play! <br> Press S for the left machine, D for the second machine, K for the third machine and L for the right machine.';

        fixedChoices = fixedChoicesCollect[currentBlock] // just bc we need this to make it fit with the horizon task

        document.getElementById('score').innerHTML ='Score this round:' + scoreThisBlock+ '<br> Total score: ' + totalScore + "<br> Trials left in this round: " + (Ntrials - trial)
        
        // inputs to clickMachine function: machine, trial, currentBlock, rewards, Nblocks, Ntrials, fixedChoices
        //machButton1.addEventListener('click', clickMachine(0, trial, currentBlock, rewards, Nblocks, Ntrials, fixedChoices));
        //machButton2.addEventListener('click', clickMachine(1, trial, currentBlock, rewards, Nblocks, Ntrials, fixedChoices));

        // make machines clickable with keyboard presses
        // make machines clickable with keyboard presses
        document.addEventListener("keydown", handle_operation_keypress, false);
        clickable = 1
        // machButton1.onclick =  function(){clickMachine(0, "restless")};
        // machButton2.onclick = function(){clickMachine(1, "restless")};
        // machButton3.onclick = function(){clickMachine(2, "restless")};
        // machButton4.onclick = function(){clickMachine(3, "restless")};

    }

}



// instructions


document.getElementById('instructions').style.display = 'block';
document.getElementById('task').style.display = 'none';
document.getElementById('questionnaire').style.display = 'none';
document.getElementById('instructionText').innerHTML= "Welcome to the experiment! <br> <br> In this experiment, you will have to choose between playing different slot machines across a series of games. <br> <br> Each slot machine gives you a different average reward. <br> <br> "+
"Your goal is to earn as many points as possible. <br> <br> The game consists of several rounds, in which the slot machines have different rewards. Thus, if a new round starts, the average rewards of the slot machines have changed and you need to learn them again!"+
" You know how many clicks you have left in a round through the 'trials left this round' indicator. <br>" 

var horizonInstructions = "In this game, you will choose between two slot machines that give different average rewards. Before making your choice, you will have to make 4 choices that we pre-determined. You will see which machine is highlighted and have to choose the highlighted machine."+
" <br> <br> After these 4 intial pre-determined choices, you get to make either 1 or 6 free choices. You can see how many choices you can make under 'Trials left in this round'. You will play"+ (Nblocks-1)+"  rounds of this game. <br> <br> Click the button below to start a practice round.";

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
    if (quickrunthrough) {console.log("checking comprehension")}
    toc = Number(new Date())

    data[task]["comprehensionTime"] = toc - tic
  
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
      if (incomplete.length > 0) {// missing items

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
      } else if ((values["Q1_0"] == "1" && values["Q2_1"] == "2" && values["Q3_2"] == "0")) {// complete and correct
        understood = true
        
        $(startTaskButton).hide()
  
        window.scrollTo(0,0);
        tic = Number(new Date())

        trial = 0;
        scoreThisBlock = 0;
        Ntrials = NtrialsCollect[currentBlock]
        document.getElementById("questionnaire").style.display = 'none';
        document.getElementById('task').style.display = 'block';

        clickable = 1
        document.addEventListener("keydown", handle_operation_keypress, false);

        if (task == "horizon") {
            if (Ntrials == 5){
                document.getElementById('machine_title').innerHTML = '<b>Short round.</b> Select the machine that is highlighted. Round '+ (currentBlock) +' of ' + (Nblocks-1) +
                '<br> Press S for the left machine and K for the right machine.';
            } else {
                document.getElementById('machine_title').innerHTML = '<b>Long round.</b> Select the machine that is highlighted. Round '+ (currentBlock) +' of ' + (Nblocks-1) +
                '<br> Press S for the left machine and K for the right machine.';
            }

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
 
        } else if (task == "sam") {
            document.getElementById('machine_title').innerHTML = 'Select the slot machine you would like to play! Round '+ currentBlock +' of ' + (Nblocks-1) +
            '<br> Press S for the left machine and K for the right machine.';
        } else {
            document.getElementById('machine_title').innerHTML = 'Select the slot machine you would like to play! Round '+ currentBlock +' of ' + (Nblocks-1) +
            '<br> Press S for the left machine, D for the second machine, K for the third machine and L for the right machine.';
        }

        
      } else {//complete but incorrect
  
        // hide quiz
        $(document.getElementById("questionnaire")).hide()
        $(startTaskButton).hide()
  
        window.scrollTo(0,0);

        // set everything to beginning
        if (task == "horizon") {comprehensionAttemptsH +=1; horizonTask()} else if (task == "sam") {comprehensionAttemptsS += 1; samTask()} else {comprehensionAttemptsR += 1; restlessTask()}
        
      }
  
}

function startComprehension(task){
    understood = false

    document.getElementById('instructions').style.display = 'none';
    document.getElementById('task').style.display = 'none';
    document.getElementById("questionnaire").style.display = 'block';

    window.scrollTo(0, 0);

    while (document.getElementById("questionnaires").lastChild) {document.getElementById("questionnaires").removeChild(document.getElementById("questionnaires").lastChild);} 

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
            labels: ['No they stay the same on average.', 'Sometimes they change over time for one or both slot machines.', 'They always change over time for both slot machines.']
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
            labels: ['100', '200', '10']
          };

          var q2Data = {
            qNumber: 1,
            prompt: "Do the points you get for each slot machine change over time?",
            labels: ['No they stay the same on average.', 'Sometimes they change over time for one or several slot machines.', 'They always change over time for all slot machines.']
          };
          
          var q3Data = {
            qNumber: 2,
            prompt: "Do the points you get for each slot machine change between rounds?",
            labels: ['Yes. New round = completely new machines', 'No. They stay the same', 'There is only one round.']
          };
    }

    var Q1 = createQuestion('Q1', q1Data);
    var Q2 = createQuestion('Q2', q2Data);
    var Q3 = createQuestion('Q3', q3Data);
    document.getElementById('questionnaires').appendChild(Q1);
    document.getElementById('questionnaires').appendChild(Q2);
    document.getElementById('questionnaires').appendChild(Q3);

    tic = Number(new Date())

    startTaskButton.style.display = 'block';
    startTaskButton.onclick = function() {
        
        checkComprehension(task)

       
        

    }



}

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
    tasks[task_ind]()

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

    setTimeout(function () { 
          window.location.href = "cpilab.org";  
          }, 500)
    
});