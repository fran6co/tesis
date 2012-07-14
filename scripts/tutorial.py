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
session = json.loads(
    requests.post(
        api_url,
        headers = {'content-type': 'application/json'},
        data={'data':json.dumps({'theory': 'file_system'})},
        files={'file':(alloy_file_name, alloy_file)}
).text)

# get all assertions for the theory
assertions = json.loads(
    requests.get(
        urlparse.urljoin(api_url, session['path'] + '/assertions'),
        headers = {'content-type': 'application/json'}
).text)

print assertions

# starts proving the assertion NoDirAliases
prove_session = json.loads(
    requests.put(
        urlparse.urljoin(api_url, assertions['NoDirAliases']['path'] + '/prove'),
        headers = {'content-type': 'application/json'}
).text)

print prove_session['goals']

goal = prove_session['goals'][0]

# runs skosimp*
prove_session = json.loads(
    requests.put(
        urlparse.urljoin(api_url, goal['path'] + '/command'),
        params={'data': json.dumps({'command': 'skosimp*'})},
        headers = {'content-type': 'application/json'}
).text)

print prove_session['goals']

goal = prove_session['goals'][0]

# runs dsp-case
prove_session = json.loads(
    requests.put(
        urlparse.urljoin(api_url, goal['path'] + '/command'),
        params={'data': json.dumps({'command': 'dps-case', 'parameters': ["oh_1 in Root"]})},
        headers = {'content-type': 'application/json'}
).text)

print prove_session['goals']

goal = prove_session['goals'][0]

# get all facts for the theory
facts = json.loads(
    requests.get(
        urlparse.urljoin(api_url, session['path'] + '/facts'),
        headers = {'content-type': 'application/json'}
).text)

print facts

# use fact RootHasNoParent
prove_session = json.loads(
    requests.put(
        urlparse.urljoin(api_url, goal['path'] + '/command'),
        params={'data': json.dumps({'command': 'use', 'parameters': ["RootHasNoParent"]})},
        headers = {'content-type': 'application/json'}
).text)

print prove_session['goals']

goal = prove_session['goals'][0]

# use fact ParentDefinition to close
prove_session = json.loads(
    requests.put(
        urlparse.urljoin(api_url, goal['path'] + '/command'),
        params={'data': json.dumps({'command': 'use', 'parameters': ["ParentDefinition"]})},
        headers = {'content-type': 'application/json'}
).text)

print prove_session['goals']

goal = prove_session['goals'][0]

prove_session = json.loads(
    requests.put(
        urlparse.urljoin(api_url, goal['path'] + '/command'),
        params={'data': json.dumps({'command': 'dps-hyp', 'parameters': ["no ( (contents.oh_1) . Name) => no contents.oh_1"]})},
        headers = {'content-type': 'application/json'}
).text)

print prove_session['dps']['models_size']
print prove_session['dps']['counter_examples']
print prove_session['goals']

goal = prove_session['goals'][0]

# closes branch
prove_session = json.loads(
    requests.put(
        urlparse.urljoin(api_url, goal['path'] + '/command'),
        params={'data': json.dumps({'command': 'reduce'})},
        headers = {'content-type': 'application/json'}
).text)

print prove_session['goals']

goal = prove_session['goals'][0]

# get all lemmas for the theory
lemmas = json.loads(
    requests.get(
        urlparse.urljoin(api_url, session['path'] + '/lemmas'),
        headers = {'content-type': 'application/json'}
    ).text)

print lemmas

goal = prove_session['goals'][1]

# use lemma1
prove_session = json.loads(
    requests.put(
        urlparse.urljoin(api_url, goal['path'] + '/command'),
        params={'data': json.dumps({'command': 'use', 'parameters': ["Lemma1"]})},
        headers = {'content-type': 'application/json'}
).text)

print prove_session['goals']

# get all open goals
goals = json.loads(
    requests.get(
        urlparse.urljoin(api_url, prove_session['path'] + '/goals'),
        params={'filter': 'open'},
        headers = {'content-type': 'application/json'}
).text)

print goals

goal = goals['NoDirAliases.1.2']

# typepred "oh_1"
prove_session = json.loads(
    requests.put(
        urlparse.urljoin(api_url, goal['path'] + '/command'),
        params={'data': json.dumps({'command': 'typepred', 'parameters': ["oh_1"]})},
        headers = {'content-type': 'application/json'}
).text)

print prove_session['goals']

goal = prove_session['goals'][0]

prove_session = json.loads(
    requests.put(
        urlparse.urljoin(api_url, goal['path'] + '/command'),
        params={'data': json.dumps({'command': 'prop'})},
        headers = {'content-type': 'application/json'}
).text)

print prove_session['goals']

goal = goals['NoDirAliases.2']

prove_session = json.loads(
    requests.put(
        urlparse.urljoin(api_url, goal['path'] + '/command'),
        params={'data': json.dumps({'command': 'use', 'parameters': ['OneParent']})},
        headers = {'content-type': 'application/json'}
).text)

print prove_session['goals']

goal = prove_session['goals'][0]

prove_session = json.loads(
    requests.put(
        urlparse.urljoin(api_url, goal['path'] + '/command'),
        params={'data': json.dumps({'command': 'grind'})},
        headers = {'content-type': 'application/json'}
).text)

print prove_session['goals']

print prove_session['status']

