#!/bin/env python3
# {
#     "_id" : ObjectId("5bed30cb5d8bd881161c57b4"),
#     "text" : "test text",
#     "user_answers" : [ 
#     ],
#     "survey_answers" : [ 
#         {
#             "u_id" : 1234,
#             "score" : 1
#         }, 
#         {
#             "u_id" : 1234,
#             "score" : 1
#         }, 
#         {
#             "u_id" : 1234,
#             "score" : 1
#         }
#     ]
# }

import json
def readfile(file):
	with open(file) as f:
		return f.readlines()

userid = 0
questions = {}

for i in range(1,6):
	quest_text = readfile(f"questions/paired-survey-{i}-codebook.txt")
	user_answers = readfile(f"answers/paired-survey-{i}-responses.csv")
	for answer in user_answers[1:]:
		answer = answer.split("\t")[4:]

		for j in range(0, len(quest_text)):
			txt = quest_text[j].split("\t")[1].strip()
			if(txt not in questions):
				questions[txt] = {}
			questions[txt][userid] = int(answer[j])

		userid += 1

dump = []

for question_text, user_answer_dict in questions.items():
	document = {'text': question_text, 'survey_answers': []}
	for userid, score in user_answer_dict.items():
		document['survey_answers'].append({'respondent_id': userid, 'score': score})
		
	dump.append(json.dumps(document))

with open("survey_answers.json", '+w') as f:
	f.write('['+',\n'.join(dump)+']') 
