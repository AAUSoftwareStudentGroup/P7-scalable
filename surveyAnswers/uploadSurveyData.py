#!/bin/env python3
import pymongo
import json

def readfile(file):
    with open(file) as f:
        return f.read()

data = json.loads(readfile('questions.json'))

db = pymongo.MongoClient('localhost',8020)['datingdb']
db.authenticate('datingdbuser', 'datingdbpassword')
db.drop_collection('questions')
db.create_collection('questions')
db.questions.insert_many(data)