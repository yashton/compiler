#! /usr/bin/python2

import sys, urllib2, urllib

#data = [("lex", "0")]
data = []

if len(sys.argv) < 2:
    data.append(("file", sys.stdin.read()))
else:
    data.append(("file", open(sys.argv[1]).read()))

resp = urllib2.urlopen(urllib2.Request(
#        "http://matt.might.net/apps/pyparse/pyparse.php",
        "http://matt.might.net/apps/pylex/pylex.php",
        urllib.urlencode(data)))
print resp.read()
