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


def getQuestions():
	userid = 0
	questionId = 0
	questionIds = {}
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

	return questions

def convertToJson():
	questions = getQuestions()

	dump = []

	for question_text, user_answer_dict in questions.items():
		document = {'text': question_text, 'survey_answers': []}
		for userid, score in user_answer_dict.items():
			document['survey_answers'].append({'respondent_id': userid, 'score': score})
			
		dump.append(json.dumps(document))

	with open("survey_answers.json", '+w') as f:
		f.write('['+',\n'.join(dump)+']') 

def convertToList():
	questCount = 0
	userCount = 0
	questions = {}

	answers = {}
	for i in range(1,6):
		quest_text = readfile(f"questions/paired-survey-{i}-codebook.txt")
		user_answers = readfile(f"answers/paired-survey-{i}-responses.csv")
		for answer in user_answers[1:]:
			answer = answer.split("\t")[4:]
			answers[userCount] = {}
			for j in range(0, len(quest_text)):
				txt = quest_text[j].split("\t")[1].strip()
				if(txt not in questions):
					questions[txt] = questCount
					questCount += 1
				answers[userCount][questions[txt]] = int(answer[j])
			userCount += 1

	return questions, answers

def getCorrelation(questions):
	correlationDict = {}
	
	for i in range(1,6):
		if i == 4:
			continue
		quest_text = readfile(f"questions/paired-survey-{i}-codebook.txt")
		question_text = [x.split("\t")[1].strip() for x in quest_text]

		correlaions = readfile(f"correlations/paired-survey-{i}-cross-correlations.csv")
		rowQuestIndex = 0
		for correlation in correlaions[1:]:
			correlation = correlation.split("\t")[1:]
			rowQuestionId = questions[question_text[rowQuestIndex]]
			correlationDict[rowQuestionId] = {}
			for j in range(len(correlation)):
				#print(question_text)
				#print(j)
				#print(question_text[j])
				colQuestionId = questions[question_text[j]]
				correlationDict[rowQuestionId][colQuestionId] = correlation[j].strip()

			rowQuestIndex += 1
			#print(str(rowQuestIndex) + " " + str(i))
			
			
	return correlationDict




def saveQuestions(quest):
	'''with open("questions.csv", 'w') as f:
		for key in quest:
			f.write(f"{quest[key]}; {key};\n")
'''
	with open("questions.json", 'w') as f:
		f.write("[")
		first = True
		
		for key in quest:
			if not first:
				f.write(",\n")
			else:
				first = False	
			f.write('{"index": ' + str(quest[key]) + ', "text": "' + key.replace('"', "\\\"").replace("&lsquo;", "'") + '"}')
		f.write("]")

def saveNestedDict(file, data):
	with open(file, 'w') as f:
		for row in data:
			for col in data[row]:
				value = data[row][col]
				
				f.write(f"{row}; {col}; {value};\n")


def fixAnswerFile3():
	file = readfile("answers/paired-survey-3-responses.csv.original")
	lines = []
	with open("answers/paired-survey-3-responses.csv", 'w') as f:
		for line in file:
			parts = line.split("\t")[:134]
			for x in parts:
				f.write(x + "\t")
			f.write("\n")

def fixAnswerFile5():
	file = readfile("answers/paired-survey-5-responses.csv.original")
	lines = []
	with open("answers/paired-survey-5-responses.csv", 'w') as f:
		f.write(file[0])
		for line in file[1:]:
			parts = line.split("\t")
			newLine = ""
			for x in range(len(parts)):
				elm = parts[x] + "\t"
				if x < 4:
					newLine += elm
				elif(x-4) % 3 == 0:
					newLine += elm
			f.write(newLine + "\n")

def fixCorreFile3():
	file = readfile("correlations/paired-survey-3-cross-correlations.csv.original")
	lines = []
	with open("correlations/paired-survey-3-cross-correlations.csv", 'w') as f:
		for x in range(len(file)):
			if x > 129:
				return
			line = file[x]
			parts = line.split("\t")[:130]
			for x in parts:
				f.write(x + "\t")
			f.write("\n")

#fixCorreFile3()
#fixAnswerFile5()

quest, answers = convertToList()
correlation = getCorrelation(quest)

saveNestedDict("correlations.csv", correlation)
saveQuestions(quest)
saveNestedDict("answers.csv", answers)
