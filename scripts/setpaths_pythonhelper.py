#! /usr/bin/env python

import re
import os
# Strip everything out until the end of the first if statement, and
# also strip everything from the exec statement in the 'katana' script
pat = re.compile(r'.*?\nfi(?P<payload>.*)\nexec.*', re.I | re.DOTALL)

scriptFile = open(os.environ['KATANA_HOME'] + os.sep + 'katana', 'r')
script = scriptFile.read()

m = pat.match(script)
payload = m.groupdict()['payload']
outputFile = open('/tmp/katanaScript.rc', 'w')
outputFile.write(payload)
outputFile.close()
