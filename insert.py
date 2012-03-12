#!/usr/bin/env python
import urllib2, simplejson

def insert(start, count):
    for i in range(start, start+count):
        sid = 'session%d'%i
        session = {
            'auth': {
                'uid': 1,
                'name': 'test',
            },
            'extra': {}
        }
        urllib2.urlopen('http://localhost:3000/insert/'+sid, data=simplejson.dumps(session)).read()

if __name__ == '__main__':
    import sys
    if len(sys.argv)>2:
        insert(int(sys.argv[1]), int(sys.argv[2]))
    else:
        print '%s start count'%sys.argv[0]
