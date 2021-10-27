import requests
import os
import json

path = 'data'
print(os.getcwd())
subreddit = input("Enter desired subreddit name: ")

file = subreddit + '.json'
dir_list = os.listdir(path)
print("List of directories and files before creation:")
print(dir_list)
print()

def jsonize(word):
  if word[0:4] == 'True':
    return "true" + word[4:]
  if word[0:5] == 'False':
    return "false" + word[5:]
  if word[0:4] == 'None':
    return "null" + word[4:]
  return word


link = 'https://www.reddit.com/r/' + subreddit + '/hot.json'
response = str(requests.get(link, headers = {'User-agent': '3110 Final Project'}).json())
#print(response)
words = response.split(' ')
#print(words)
words = list(map(jsonize, words))
words = ' '.join(words)
changeToQuotes = json.dumps(words)
print(changeToQuotes)

with open(os.path.join(path, file), 'w') as jsonFile:
  jsonFile.write(changeToQuotes)
  jsonFile.close()