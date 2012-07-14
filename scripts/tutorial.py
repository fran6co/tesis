__author__ = 'fran6co'

import requests
import json
import os
import urlparse

test_path = os.path.join(os.path.dirname(__file__),'../test')

payload = {'theory': 'file_system'}

api_url = 'http://localhost:8080'
session = json.loads(requests.post(api_url,data={'data':json.dumps(payload)},files={'file':('filesystem.als', open(os.path.join(test_path,"filesystem.als"),"rb"))}).text)

theorem = json.loads(requests.get(urlparse.urljoin(api_url, ''.join([session['path'], '/NoDirAliases']))).text)
print theorem['last_statement']

command = json.loads(requests.get(urlparse.urljoin(api_url, ''.join([theorem['path'], '/command'])),params={'data': json.dumps({'command': 'skosimp*'})}).text)
print command

command = json.loads(requests.get(urlparse.urljoin(api_url, ''.join([theorem['path'], '/command'])),params={'data': json.dumps({'command': 'dps-case', 'parameters': ["oh_1 in Root"]})}).text)
print command