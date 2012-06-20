__author__ = 'fran6co'

from twisted.web.resource import Resource, NoResource
from twisted.python import log

import pexpect
import uuid
import json
import re
import os
import cgi

class PVSResource(Resource):
    def __init__(self,id,theory,pvs):
        self.pvs = pvs
        self.theory = theory
        self.id = id
        self.action = None

        Resource.__init__(self)

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

    def render_GET(self,request):
        return self._render(request,"GET")
    def render_POST(self,request):
        return self._render(request,"POST")
    def render_PUT(self,request):
        return self._render(request,"PUT")
    def render_DELETE(self,request):
        return self._render(request,"DELETE")

    def render_OPTIONS(self,request):
        return ''

    def render(self,request):
        request.setHeader('Access-Control-Allow-Origin', '*')
        request.setHeader("Access-Control-Allow-Methods","*")
        request.setHeader("Access-Control-Allow-Headers","Content-Type,*")
        return Resource.render(self,request)

    def toDict(self):
        return {"id":self.id,"theory":self.theory}

    def _render(self, request,method):
        if self.action:
            func = getattr(self,self.action+'_'+method)
            return json.dumps(func(request))
        else:
            return json.dumps(self.toDict())

    def assert_GET(self,request):
        data = json.loads(request.args['data'][0])

        self.pvs.sendline("(prove \"%s\")!" % (data['assertion']))
        self.pvs.expect("\nRule\? ")

        statement = self.processResult(self.pvs.before)

        return {'statement':statement}

    def command_GET(self,request):
        data = json.loads(request.args['data'][0])

        command = data["command"]
        parameters = map(lambda parameter: "\"%s\"" % (parameter) if isinstance(o, basestring) else parameter,data["parameters"])

        log.msg("Running (%s %s)" % (command,' '.join(parameters)))
        self.pvs.sendline("(%s %s)" % (command,' '.join(parameters)))
        self.pvs.expect("\nRule\? ")

        result = self.processResult(self.pvs.before)

        return {'result':result}

    def processResult(self,result):
        pattern = re.compile('(?P<antecedents>(\s*[{|\[]-?\d+[}|\]]\s*.+\s*)*)\|-------(?P<consequents>(\s*[{|\[]-?\d+[}|\]]\s*.+\s*)*)')

        m = pattern.search(result)

        antecedents = m.group('antecedents')
        consequents = m.group('consequents')

        log.msg('Ant: '+antecedents)
        log.msg('Cons: '+consequents)

        ents_pattern = re.compile('[{|\[]-?\d+[}|\]]\s*(.+)')

        antecedents = map(lambda s: s.strip(),ents_pattern.findall(antecedents))
        consequents = map(lambda s: s.strip(),ents_pattern.findall(consequents))

        return (antecedents,consequents)

class RootPVSResource(Resource):
    pvs = {}

    def getChild(self, name, request):
        if name == '':
            return self

        return Resource.getChild(self, name, request)

    def render(self,request):
        request.setHeader('Access-Control-Allow-Origin', '*')
        request.setHeader("Access-Control-Allow-Methods","*")
        request.setHeader("Access-Control-Allow-Headers","Content-Type,*")
        return Resource.render(self,request)

    def render_GET(self, request):
        for key in self.pvs.keys():
            self.getChild(key, request)

        #return json.dumps(map(lambda id: self.getChild(id, request).toDict(),self.pvs.keys()))
        return json.dumps(map(lambda id: {'id':id},self.pvs.keys()))

    def render_OPTIONS(self,request):
        return ''

    def render_POST(self,request):
        id = str(uuid.uuid1())

        log.msg("[%s] Intializing work environment" % (id))

        headers = request.getAllHeaders()

        data = cgi.FieldStorage(
            fp = request.content,
            headers = headers,
            environ = {
                'REQUEST_METHOD':'POST',
                'CONTENT_TYPE': headers['content-type'],
            }
        )

        session_path = os.path.normpath(os.path.join(os.path.dirname(__file__),"../data/",id))
        if not os.path.exists(session_path):
            os.makedirs(session_path)

        log.msg("[%s] Created path %s" % (id, session_path))

        alloy_file = os.path.join(session_path,data["file"].filename)

        out = open(alloy_file, 'wb')
        out.write(data["file"].value)
        out.close()

        log.msg("[%s] Theory data file %s saved" % (id, alloy_file))

        dynamite_path = os.path.normpath(os.path.join(os.path.dirname(__file__),"../lib/dynamite"))

        os.chdir(session_path)
        os.system("/usr/bin/java -cp "+os.path.join(dynamite_path,'lib/dynamite-translator.jar')+":"+os.path.join(dynamite_path,'lib/alloy4.jar')+" ar.uba.dc.dynamite.api.SpecificationTranslatorRunner "+alloy_file)

        log.msg("[%s] Translated alloy theory file to PVS" % (id))

        data = json.loads(data['data'].value)
        theory = data["theory"]

        pvs = self.pvs[id] = pexpect.spawn(
            os.path.normpath(os.path.join(os.path.dirname(__file__),"../lib/pvs/pvsio")),
            [os.path.join(session_path,"%sTheorems" % (theory))],
            timeout=30)

        log.msg("[%s] PVS channel open" % (id))

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

        log.msg("[%s] DPS constants initialized" % (id))

        pvs.sendline('(require (namestring "'+os.path.join(dynamite_path,'Starter/dps-intermediaries.lisp')+'"))!')
        pvs.expect("\n<PVSio> ")

        log.msg("[%s] DPS intermediaries loaded" % (id))

        pvs.sendline('(require (namestring "'+os.path.join(dynamite_path,'Starter/dps-userinterface.lisp')+'"))!')
        pvs.expect("\n<PVSio> ")

        log.msg("[%s] DPS user interface loaded" % (id))

        pvs.sendline('(require (namestring "'+os.path.join(dynamite_path,'Starter/dps-prover.lisp')+'"))!')
        pvs.expect("\n<PVSio> ")

        log.msg("[%s] PVS Ready" % (id))

        resource = PVSResource(id,theory,pvs)

        self.putChild(id,resource)

        return resource.render_GET(request)