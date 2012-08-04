__author__ = 'fran6co'

from twisted.web.resource import Resource, NoResource
from twisted.python import log

import pexpect
import uuid
import json
import re
import os
import cgi
import glob
import hashlib


class Goal(Resource):
    def __init__(self, assertion, pvs_output, session, parent):
        Resource.__init__(self)

        self.assertion = assertion
        self.parent = parent
        self.action = None
        self.session = session

        self.parse_pvs(pvs_output)
        self.id = str(id(self))

    def parse_pvs(self, pvs_output):
        self.label = re.search('^(?P<id>'+self.assertion+'(\.\d)*) :', pvs_output, re.M).group('id')

    def path(self):
        return self.parent.path() + '/goals/' + self.id

    def render(self,request):
        request.setHeader('Access-Control-Allow-Origin', '*')
        request.setHeader("Access-Control-Allow-Methods","*")
        request.setHeader("Access-Control-Allow-Headers","Content-Type,*")

        return Resource.render(self,request)

    def render_GET(self,request):
        return self._render(request,"GET")
    def render_POST(self,request):
        return self._render(request,"POST")
    def render_PUT(self,request):
        return self._render(request,"PUT")
    def render_DELETE(self,request):
        return self._render(request,"DELETE")

    def getChild(self, name, request):
        try:
            if '' == name:
                name = None
            else:
                func = getattr(self,name+'_'+request.method)
        except AttributeError:
            return NoResource()

        self.action = name

        return self

    def render_OPTIONS(self,request):
        return ''

    def _render(self, request,method):
        if self.action:
            func = getattr(self,self.action+'_'+method)
            return json.dumps(func(request))
        else:
            return self.parent.render_GET(request)

    def command_PUT(self,request):
        data = json.loads(request.args['data'][0])

        command = data["command"]
        try:
            parameters = map(lambda parameter: "\"%s\"" % (parameter) if isinstance(parameter, basestring) else parameter, data["parameters"])
        except KeyError:
            parameters = []

        log.msg("[%s][%s] Running (%s %s)" % (self.session.id, self.assertion, command,' '.join(parameters)))
        self.parent.pvs.sendline("(%s %s)" % (command,' '.join(parameters)))
        self.parent.pvs.expect("\nRule\? ")

        log.msg("[%s][%s] Raw result:\n%s" % (self.session.id, self.assertion, self.parent.pvs.before) )
        result = self.processResult(self.parent.pvs.before)

        goal = Goal(self.assertion, self.parent.pvs.before, self.session, self.parent)
        self.parent.goals[goal.id] = goal

        goals = []

        goals.append({'id': goal.id, 'path': goal.path()})

        return {'path': self.path(), 'goals': goals}

    def processResult(self,result):
        pattern = re.compile('(?P<antecedents>(\s*[{|\[]-?\d+[}|\]]\s*.+\s*)*)\|-------(?P<consequents>(\s*[{|\[]-?\d+[}|\]]\s*.+\s*)*)')

        m = pattern.search(result)

        antecedents = m.group('antecedents')
        consequents = m.group('consequents')

        ents_pattern = re.compile('[{|\[]-?\d+[}|\]]\s*(.+)')

        antecedents = map(lambda s: s.strip(),ents_pattern.findall(antecedents))
        consequents = map(lambda s: s.strip(),ents_pattern.findall(consequents))

        return {'antecedents': antecedents, 'consequents': consequents}

class ProveSession(Resource):
    def __init__(self, session, parent):
        Resource.__init__(self)

        self.session = session
        self.parent = parent
        self.pvs = None
        self.goals = {}
        self.request_path = None

    def path(self):
        return self.parent.path() + '/session'

    def render(self,request):
        request.setHeader('Access-Control-Allow-Origin', '*')
        request.setHeader("Access-Control-Allow-Methods","*")
        request.setHeader("Access-Control-Allow-Headers","Content-Type,*")

        return Resource.render(self,request)

    def render_GET(self, request):
        goals = []

        for id, goal in self.goals.iteritems():
            goals.append({'id': id, 'path': goal.path()})

        return json.dumps({'path': self.path(), 'goals': goals})

    def render_POST(self, request):
        self.pvs = self.initDynamite()

        self.pvs.sendline("(prove \"%s\")!" % (self.parent.name))
        self.pvs.expect("\nRule\? ")

        goal = Goal(self.parent.name, self.pvs.before, self.session, self)

        self.goals[goal.id] = goal

        log.msg("[%s] Proving %s assertion" % (self.session.id, self.parent.name))

        return self.render_GET(request)

    def initDynamite(self):
        session_path = os.path.normpath(os.path.join(os.path.dirname(__file__),"../data/", self.session.id))
        dynamite_path = os.path.normpath(os.path.join(os.path.dirname(__file__),"../lib/dynamite"))

        pvs = pexpect.spawn(
            os.path.normpath(os.path.join(os.path.dirname(__file__),"../lib/pvs/pvsio")),
            [os.path.join(session_path,"%sTheorems" % (self.session.theory))],
            timeout=30)

        log.msg("[%s] PVS channel open" % (self.session.id))

        pvs.expect("\n<PVSio> ")

        constants = '(progn\n'\
                    + '(defconstant +dps-path+ "'+dynamite_path+'/")\n'\
                    + '(defconstant +java+ "/usr/bin/java")\n'\
                    + '(defconstant +alloy+ "'+os.path.join(dynamite_path,'lib/alloy4.jar')+'")\n'\
                    + '(defconstant +alloy-entry-point+ "alloy.cli.AlloyCLI")\n'\
                    + '(defconstant +dynamite-translator+ "'+os.path.join(dynamite_path,'lib/dynamite-translator.jar')+'")\n'\
                    + '(defconstant +dynamite-translator-synthesized-expression-processor-entry-point+ "ar.uba.dc.dynamite.api.SynthesizedExpressionProcessor")\n'\
                    + '(defconstant +dynamite-translator-synthesized-formula-processor-entry-point+ "ar.uba.dc.dynamite.api.SynthesizedFormulaProcessor")\n'\
                    + '(defconstant +dynamite-translator-counterexample-vizualizator-entry-point+ "ar.uba.dc.dynamite.api.VizGUI")\n'\
                    + '(defconstant +dynamite-translator-specification-processor-entry-point+ "ar.uba.dc.dynamite.api.SpecificationProcessor")\n'\
                    + '(defconstant +dynamite-translator-synthesized-goal-processor-entry-point+ "ar.uba.dc.dynamite.api.SynthesizedGoalProcessor")\n'\
                    + '(defconstant +dynamite-translator-postulator-entry-point+ "ar.uba.dc.dynamite.api.ExistencialCandidatePostulator")\n'\
                    + '(defconstant +core-file-name+ "core")\n'\
                    + '(defconstant +sugs-file-name+ "sugs")\n'\
                    + '(defconstant +synthesized-goal-file-name+ "goal.als")\n'\
                    + '(defvar *dps-pp-activated* t)\n'\
        +')!'

        pvs.sendline(constants)
        pvs.expect("\n<PVSio> ")

        log.msg("[%s] DPS constants initialized" % (self.session.id))

        pvs.sendline('(require (namestring "'+os.path.join(dynamite_path,'Starter/dps-intermediaries.lisp')+'"))!')
        pvs.expect("\n<PVSio> ")

        log.msg("[%s] DPS intermediaries loaded" % (self.session.id))

        pvs.sendline('(require (namestring "'+os.path.join(dynamite_path,'Starter/dps-userinterface.lisp')+'"))!')
        pvs.expect("\n<PVSio> ")

        log.msg("[%s] DPS user interface loaded" % (self.session.id))

        pvs.sendline('(require (namestring "'+os.path.join(dynamite_path,'Starter/dps-prover.lisp')+'"))!')
        pvs.expect("\n<PVSio> ")

        log.msg("[%s] PVS Ready" % (self.session.id))

        return pvs

    def getChild(self, name, request):
        if name == '':
            self.request_path = None
            return self

        if name == 'goals':
            self.request_path = name
            return self

        if self.request_path == 'goals':
            try:
                return self.goals[name]
            except KeyError:
                return NoResource()

        self.request_path = None

        return Resource.getChild(self, name, request)


class AssertionResource(Resource):
    def __init__(self, name, session, parent):
        Resource.__init__(self)
        self.session = session

        self.parent = parent
        self.name = name

        self.putChild('session', ProveSession(session, self))

    def render(self,request):
        request.setHeader('Access-Control-Allow-Origin', '*')
        request.setHeader("Access-Control-Allow-Methods","*")
        request.setHeader("Access-Control-Allow-Headers","Content-Type,*")

        return Resource.render(self,request)

    def render_GET(self, request):
        return json.dumps({'path': self.path() })

    def path(self):
        return self.parent.path() + '/' + self.name

    def getChild(self, name, request):
        if name == '':
            return self

        return Resource.getChild(self, name, request)


class Assertions(Resource):
    def __init__(self, session):
        Resource.__init__(self)

        self.session = session

        for assertion in self.getAssertions():
            self.putChild(assertion, AssertionResource(assertion, session, self))

    def render(self,request):
        request.setHeader('Access-Control-Allow-Origin', '*')
        request.setHeader("Access-Control-Allow-Methods","*")
        request.setHeader("Access-Control-Allow-Headers","Content-Type,*")

        return Resource.render(self,request)

    def path(self):
        return self.session.id + '/assertions'

    def render_GET(self, request):
        return json.dumps(self.getAssertions())

    def getAssertions(self):
        session_path = os.path.normpath(os.path.join(os.path.dirname(__file__),"../data/", self.session.id))

        alloy_file = os.path.join(session_path,self.session.theory + '.als')

        assertions = re.findall(r'^assert\s+([^\{|\s]+)', open(alloy_file, 'r').read(), re.M)

        data = {}

        for assertion in assertions:
            data[assertion] = {'path': self.path() + '/' + assertion }

        return data

    def getChild(self, name, request):
        if name == '':
            return self

        return Resource.getChild(self, name, request)


class SessionPVCResource(Resource):
    def __init__(self,id,theory):
        Resource.__init__(self)

        self.theory = theory
        self.id = id

        self.putChild('assertions', Assertions(self))


    def render(self,request):
        request.setHeader('Access-Control-Allow-Origin', '*')
        request.setHeader("Access-Control-Allow-Methods","*")
        request.setHeader("Access-Control-Allow-Headers","Content-Type,*")

        return Resource.render(self,request)

    def render_GET(self, request):
        return json.dumps({'id': self.id, 'path': self.id})

    def getChild(self, name, request):
        if name == '':
            return self

        return Resource.getChild(self, name, request)

class RootPVCResource(Resource):
    def getChild(self, name, request):
        if name == '':
            return self

        child = Resource.getChild(self, name, request)

        if isinstance(child, NoResource):
            session_path = os.path.normpath(os.path.join(os.path.dirname(__file__),"../data/",name))
            if os.path.exists(session_path):
                theory_path = glob.glob(os.path.join(session_path,'*.als'))[0]
                theory = os.path.splitext(os.path.basename(theory_path))[0]
                child = self.init_session(name, theory)

        return child

    def render(self,request):
        request.setHeader('Access-Control-Allow-Origin', '*')
        request.setHeader("Access-Control-Allow-Methods","*")
        request.setHeader("Access-Control-Allow-Headers","Content-Type,*")
        return Resource.render(self,request)

    def render_GET(self, request):
        sessions = []
        sessions_path = glob.glob(os.path.join(os.path.dirname(__file__),"../data/*"))
        for session in sessions_path:
            theory_path = glob.glob(os.path.join(session,'*.als'))[0]
            theory = os.path.splitext(os.path.basename(theory_path))[0]
            hash = hashlib.md5(open(theory_path,'rb').read()).hexdigest()

            id = os.path.basename(session)

            sessions.append({'id': id, 'theory': theory, 'hash': hash})

        return json.dumps(sessions)

    def render_OPTIONS(self,request):
        return ''

    def init_session(self, id, theory, file = None):
        log.msg("[%s] Intializing work environment" % (id))

        session_path = os.path.normpath(os.path.join(os.path.dirname(__file__),"../data/",id))

        dynamite_path = os.path.normpath(os.path.join(os.path.dirname(__file__),"../lib/dynamite"))
        if not os.path.exists(session_path) or file:
            os.makedirs(session_path)

            log.msg("[%s] Created path %s" % (id, session_path))

            alloy_file = os.path.join(session_path, theory+'.als')

            out = open(alloy_file, 'wb')
            out.write(file.value)
            out.close()

            log.msg("[%s] Theory data file %s saved" % (id, alloy_file))

            os.chdir(session_path)
            os.system("/usr/bin/java -cp "+os.path.join(dynamite_path,'lib/dynamite-translator.jar')+":"+os.path.join(dynamite_path,'lib/alloy4.jar')+" ar.uba.dc.dynamite.api.SpecificationTranslatorRunner "+alloy_file)

            log.msg("[%s] Translated alloy theory file to PVS" % (id))

        resource = SessionPVCResource(id, theory)

        self.putChild(id,resource)

        return resource

    def render_POST(self,request):
        id = str(uuid.uuid1())

        headers = request.getAllHeaders()

        data = cgi.FieldStorage(
            fp = request.content,
            headers = headers,
            environ = {
                'REQUEST_METHOD':'POST',
                'CONTENT_TYPE': headers['content-type'],
                }
        )

        file = data["file"]
        data = json.loads(data['data'].value)

        resource = self.init_session(id, data["theory"], file)

        return resource.render_GET(request)