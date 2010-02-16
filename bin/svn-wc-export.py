#!/usr/bin/python
import pysvn
import os
import os.path
from sys import argv, exit
from time import time
from optparse import OptionParser

client = pysvn.Client()
base = pysvn.Revision(pysvn.opt_revision_kind.base)
working = pysvn.Revision(pysvn.opt_revision_kind.working)
unspecified = pysvn.Revision(pysvn.opt_revision_kind.unspecified)
# print client.diff("/tmp",argv[1])
#for x in client.list(argv[1],revision=base,peg_revision=working):

class Mark(object):
   marks = []
   def __init__(self):
      marks = Mark.marks
      self.i = len(marks)
      self.N = self.i + 1
      marks += [ self ]
   def __str__(self):
      return "mark :%d" % self.N
   def __contains__(self,x):
      return x in self.__dict__

def datalen(x):
   return "data %d\n%s" % (len(x), x)

def blob(repopath,path,info):
   reponame = path[1+len(repopath):]
   lastslash = reponame.rfind('/')
   sbase = '.svn/text-base/'
   if lastslash == -1:
      textbase = sbase + reponame
   else:
      textbase = reponame[0:lastslash] + '/' + sbase + reponame[1+lastslash:]
   textbase += '.svn-base'
   textbase = repopath + '/' + textbase
   textwc = repopath + '/' + reponame
   if not os.path.isfile(textbase):
      raise Exception("base not a file? %s\n%s %s %s" % (textbase, repopath, path, reponame))
   if not os.path.isfile(textwc):
      raise Exception("wc not a file? %s\n%s %s %s" % (textwc, repopath, path, reponame))
   infile = open(textbase,'rb')
   data = infile.read()
   mark = Mark()
   mark.file = True
   mark.reponame = reponame
   mark.perms = 0100644
   if os.access(textwc,os.X_OK):
      mark.perms |= 0111
   print "blob"
   print mark
   print datalen(data)

def commit(repopath,client):
   try:
      uuid = client.info2(repopath,recurse=False)
      uuid = uuid[0][1].repos_UUID
   except Exception:
      uuid = None
   try:
      repolog = client.log(repopath,limit=1)
      repolog = repolog[0]
      if False:
         for k in repolog.keys():
            print "%s\t%s" % (k,repolog[k])
   except Exception:
      repolog = None
   if repolog and 'revprops' in repolog:
      props = repolog['revprops']
      log = props['svn:log']
      author = props['svn:author']
      if -1 == author.find('@'):
         author = '%s <%s@%s>' % (author, author, uuid)
      date = props['svn:date']
      rev = repolog.revision.number
   else:
      log = 'Initial import from SVN'
      author = os.environ['LOGNAME']
      date = time()
   if log:
      msg = log
   cmark = Mark()
   print "commit refs/remotes/trunk"
   print cmark
   if author:
      for authortype in ['author','committer']:
         print "%s %s %d -0500" % (authortype, author, date)
   if len(log):
      print datalen(log)
   for mark in [ m for m in Mark.marks if 'file' in m ]:
      print "M %o :%d %s" % (mark.perms, mark.N, mark.reponame)
   print ""

if __name__ == '__main__':
   parser = OptionParser()
   parser.add_option('--.git', dest="dogit", help="generate .git/svn/.metadata")
   parser.add_option('--wc', type="string", dest="wc", help="working copy path")
   (options, args) = parser.parse_args()
   if not options.wc and len(args):
      options.wc = args[0]
      args = args[1:]
   if not options.wc:
      parser.error("No working copy")
   repopath = argv[1]
   repopath = os.path.abspath(repopath)
   files_info = client.info2(repopath)
   files_info.sort()

   a=0
   for path, info in files_info:
      a+=1
      if info['kind'] == pysvn.node_kind.dir:
         continue
      elif info['kind'] != pysvn.node_kind.file:
         raise Exception("Unhandled node kind: %s" % info['kind'])
      if False:
         print path[1+len(repopath):]
         for k in info.keys():
            print "\t%s\t%s" % (k, info[k])
            if k == 'wc_info':
               for k2 in info[k].keys():
                  print "\t\t%s\t%s" % (k2, info[k][k2])
         continue
      blob(repopath,path,info)
   commit(repopath,client)
