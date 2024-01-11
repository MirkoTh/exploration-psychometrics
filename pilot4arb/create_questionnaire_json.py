import json
import pandas as pd

questionnaire_data = pd.read_csv('task/questionnaires.csv',sep=';', lineterminator='\r')

data = {}

for i in questionnaire_data.Measure.unique()[0:6]:# 0:6 bc it loves to just add some nan rows in the end so one of the measures is nan
    data[i] = dict(questions=[], preamble=questionnaire_data.Preamble[questionnaire_data.Measure == i].iloc[0],
                   name=i)
    for j in questionnaire_data[questionnaire_data.Measure == i].iterrows():
        print([i for i in j[1].Options.split(',') if len(i)])
        data[i]['questions'].append(dict(prompt=j[1].Question,
                                            labels=[i for i in j[1].Options.split(',') if len(i)]))

json_data = json.dumps(data)

with open('task/questionnaires.json', 'w') as outfile:
    json.dump(data, outfile)

