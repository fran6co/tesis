__author__ = 'fran6co'

import requests
import json
import os
import urlparse

api_url = 'http://localhost:8080'

alloy_file_name = "filesystem.als"

tutorial_path = os.path.join(os.path.dirname(__file__), '../tutorial')
alloy_file = open(os.path.join(tutorial_path, alloy_file_name), "rb")

# starts a new session for a specification
session = requests.post(
    api_url,
    data={'data':json.dumps({'theory': 'file_system'})},
    files={'file':(alloy_file_name, alloy_file)}
).json

print session

# get all assertions for the theory
assertions = requests.get(
    urlparse.urljoin(api_url, session['path'] + '/assertions')
).json

print assertions

# starts proving the assertion NoDirAliases
prove_session = requests.post(
    urlparse.urljoin(api_url, assertions['NoDirAliases']['path'] + '/session')
).json

print prove_session

goal = prove_session['goals'][0]

# runs skosimp*
prove_session = requests.put(
    urlparse.urljoin(api_url, goal['path'] + '/command'),
    params={'data': json.dumps({'command': 'skosimp*'})}
).json

print prove_session

goal = prove_session['goals'][0]

# runs dsp-case
prove_session = requests.put(
    urlparse.urljoin(api_url, goal['path'] + '/command'),
    params={'data': json.dumps({'command': 'dps-case', 'parameters': ["oh_1 in Root"]})}
).json

print prove_session['goals']

goal = prove_session['goals'][0]

# get all facts for the theory
facts = requests.get(
    urlparse.urljoin(api_url, session['path'] + '/facts')
).json

print facts

# use fact RootHasNoParent
prove_session = requests.put(
    urlparse.urljoin(api_url, goal['path'] + '/command'),
    params={'data': json.dumps({'command': 'use', 'parameters': ["RootHasNoParent"]})}
).json

print prove_session['goals']

goal = prove_session['goals'][0]

# use fact ParentDefinition to close
prove_session = requests.put(
    urlparse.urljoin(api_url, goal['path'] + '/command'),
    params={'data': json.dumps({'command': 'use', 'parameters': ["ParentDefinition"]})}
).json

print prove_session['goals']

goal = prove_session['goals'][0]

prove_session = requests.put(
    urlparse.urljoin(api_url, goal['path'] + '/command'),
    params={'data': json.dumps({'command': 'dps-hyp', 'parameters': ["no ( (contents.oh_1) . Name) => no contents.oh_1"]})}
).json

print prove_session['dps']['models_size']
print prove_session['dps']['counter_examples']
print prove_session['goals']

goal = prove_session['goals'][0]

# closes branch
prove_session = requests.put(
    urlparse.urljoin(api_url, goal['path'] + '/command'),
    params={'data': json.dumps({'command': 'reduce'})}
).json

print prove_session['goals']

goal = prove_session['goals'][1]

# use lemma1
prove_session = requests.put(
    urlparse.urljoin(api_url, goal['path'] + '/command'),
    params={'data': json.dumps({'command': 'use', 'parameters': ["Lemma1"]})}
).json

print prove_session['goals']

# get all open goals
goals = requests.get(
    urlparse.urljoin(api_url, prove_session['path'] + '/goals'),
    params={'filter': 'open'}
).json

print goals

goal = goals['NoDirAliases.1.2']

# typepred "oh_1"
prove_session = requests.put(
    urlparse.urljoin(api_url, goal['path'] + '/command'),
    params={'data': json.dumps({'command': 'typepred', 'parameters': ["oh_1"]})}
).json

print prove_session['goals']

goal = prove_session['goals'][0]

prove_session = requests.put(
    urlparse.urljoin(api_url, goal['path'] + '/command'),
    params={'data': json.dumps({'command': 'prop'})}
).json

print prove_session['goals']

goal = goals['NoDirAliases.2']

prove_session = requests.put(
    urlparse.urljoin(api_url, goal['path'] + '/command'),
    params={'data': json.dumps({'command': 'use', 'parameters': ['OneParent']})}
).json

print prove_session['goals']

goal = prove_session['goals'][0]

prove_session = requests.put(
    urlparse.urljoin(api_url, goal['path'] + '/command'),
    params={'data': json.dumps({'command': 'grind'})}
).json

print prove_session['goals']

print prove_session['status']

