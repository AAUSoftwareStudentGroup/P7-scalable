#!/bin/env python3
import pymongo
import json
import importSurveyData

def readfile(file):
    with open(file) as f:
        return f.read()

db = pymongo.MongoClient('localhost',8020)['datingdb']
db.authenticate('datingdbuser', 'datingdbpassword')

if not db.get_collection('questions').count() > 0:
    filename = importSurveyData.createDicsAndQuestions()
    data = json.loads(readfile(filename))
    db = pymongo.MongoClient('localhost',8020)['datingdb']
    db.authenticate('datingdbuser', 'datingdbpassword')
    db.drop_collection('questions')
    db.create_collection('questions')
    db.questions.insert_many(data)