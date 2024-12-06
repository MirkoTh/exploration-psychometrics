# supplementary information for file uploads on the manuscript "How should we measure exploration?"
- to create the stimuli used in the tasks run wm-tasks/generate-stimuli.R; the respective outputs of that script were manually copied into the respective js and html files
- we provide all the experimental scripts to run the tasks; if you have downloaded all files from the OSF page, you can just run index.html to run the full study
- we provide the raw anonymized data in the following files:
    * data/exclusions1_noPID.csv
    * data/exclusions2_noPID.csv
    * data/finalRestlessSession1.csv
    * data/finalRestlessSession2.csv
    * data/wm-performance.csv
    * analysis/bandits/banditsWave1.Rda
    * analysis/bandits/banditsWave2.Rda
- these files are written in load-all-data.R, but this file cannot be run because prolific ids are used in this file
- run prep_questionnaire_data.R to filter attention checks, reverse code items, and create questionnaire compound scores
- run several scripts to fit the models to the data; this will allow you to reproduce the figures in the manuscript
    * analysis/bandits/modellingHorizon.R
    * analysis/bandits/modeling2AB.R
    * analysis/bandits/model_selection.R
    * analysis/compile_model_parameters.R
    * analysis/bandits/4arlb-fit-ml.R
    * analysis/bandits/4arlb-hierarchical-bayesian.R
    * analysis/bandits/4arlb-compare-ml-hc-bayes.R
    * analysis/compile_model_parameters.R
    * analysis/bandits/strategies-correlations.R
    * analysis/bandits/2ab-choice-position.R
    * analysis/reliability-and-validity.R
    * analysis/external_validity.R
    * analysis/questionnairesCFA.R
    * analysis/prep_questionnaire_data.R
- then, you can reproduce the figures with the script analysis/plotting-ms.R