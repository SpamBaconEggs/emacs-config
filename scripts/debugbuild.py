#! /usr/bin/env python

import contextlib
import datetime
import optparse
import subprocess

_gHelpStr = """usage: %prog [options] args_for_scons

A wrapper for easily setting SCons commandline options
for faster incremental SCons builds using
cached dependencies or interactive builds.

"""

_gParser = optparse.OptionParser(usage=_gHelpStr)
_gParser.add_option("-n",
    "--nodeps",
    dest="noDeps",
    action="store_true",
    help="Don't recalculate dependencies - just build",
    default=False)
_gParser.add_option("-i",
    "--interactive",
    dest="interactive",
    action="store_true",
    help="Do an interactive build session. Implies --nodeps",
    default=False)
_gParser.add_option("-r",
    "--release",
    dest="release",
    action="store_true",
    help="Do a release build. The default is to do a debug build.",
    default=False)
_gParser.add_option("-j",
                    "--threads",
                    dest="threads",
                    type="int",
                    default="4")
_gParser.add_option("-s",
                    "--scons-opts",
                    dest="sconsOpts",
                    type="str",
                    default="")
(_options, _args) = _gParser.parse_args()
_gBuildType = "release" if _options.release else "debug"

# _prepend = "cd /workspace/fanner/katana2.0 && source ./setpaths.rc && "
_gCmdStr = "cd /workspace/fanner/katana2.0/Apps/Katana && scons -u -j%i FnOptType=%s FnInternalPlugins=True %s" % (_options.threads, _gBuildType, _options.sconsOpts)

@contextlib.contextmanager
def sconsTimer():
    try:
        startTime = datetime.datetime.now()
        yield
    finally:
        def totalSeconds(timeDiff):
            td = timeDiff
            return (td.microseconds + (td.seconds + td.days * 24 * 3600) * 10.0**6) / 10.0**6
        timeDiff = datetime.datetime.now() - startTime
        print "SCons build duration: %0.1f seconds" % totalSeconds(timeDiff)

if _options.interactive:
    print "FAST INTERACTIVE REBUILD USING CACHED DEPENDENCIES."
    CmdStr = " ".join([_gCmdStr,
                       "FnNoScan3rdPartyHeaders=1 --max-drift=1 --implicit-deps-unchanged --interactive"])
    with sconsTimer():
        proc = subprocess.Popen(CmdStr, executable="/bin/bash", shell=True)
        proc.wait()
elif _options.noDeps:
    print "INCREMENTAL BUILD USING CACHED DEPENDENCIES."
    CmdStr = " ".join([_gCmdStr,
                       "FnNoScan3rdPartyHeaders=1 --max-drift=1 --implicit-deps-unchanged"])
    with sconsTimer():
        proc = subprocess.Popen(CmdStr, executable="/bin/bash", shell=True)
        proc.wait()
else:
    print "Normal scons build (without cached dependencies)"
    with sconsTimer():
        proc = subprocess.Popen(_gCmdStr, executable="/bin/bash", shell=True)
        proc.wait()
