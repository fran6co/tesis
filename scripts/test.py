__author__ = 'fran6co'

import requests
import json
import os

test_path = os.path.join(os.path.dirname(__file__),'../test')

payload = {'theory': 'file_system'}

r = requests.get('http://localhost:8080')
sessions = json.loads(r.text)

if not sessions:
    r = requests.post('http://localhost:8080',data={'data':json.dumps(payload)},files={'file':('filesystem.als', open(os.path.join(test_path,"filesystem.als"),"rb"))})

    sessions = [json.loads(r.text)]

for session in sessions:
    payload = {'assertion': 'NoDirAliases'}
    r = requests.get('http://localhost:8080/'+session['id']+'/assert',params={'data': json.dumps(payload)})
    print r.text