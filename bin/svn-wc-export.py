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
      self.i = len(Mark.marks)
      self.N = self.i + 1
      Mark.marks += [ self ]
   def __str__(self):
      return "mark :%d" % self.N
   def __contains__(self,x):
      return x in self.__dict__
   def __getitem__(self,x):
      return self.__dict__[x]

def datalen(x):
   return "data %d\n%s" % (len(x), x)

def blob(repopath,client,path,info,debug=False):
   if debug:
      print path[1+len(repopath):]
      for k in info.keys():
         print "\t%s\t%s" % (k, info[k])
         if k == 'wc_info':
            for k2 in info[k].keys():
               print "\t\t%s\t%s" % (k2, info[k][k2])
      p = client.proplist(path)
      if len(p):
         for k, v in p:
            if k == path:
               for prop in v.keys():
                  print "\tprop[%s]={%s}" % (prop,v[prop])
      else:
         print "\tno props"
      return
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

def get_uuid(repopath,client):
   try:
      uuid = client.info2(repopath,recurse=False)
      uuid = uuid[0][1].repos_UUID
   except Exception:
      uuid = None
   return uuid

def commit(repopath,client,debug=False):
   uuid = get_uuid(repopath,client)
   repoinfo = get_repoinfo(repopath,client)
   try:
      repolog = client.log(repopath,limit=1)
      repolog = repolog[0]
      if debug:
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
   if not len(log):
      log = "Initial import from SVN"
   repoinfo = get_repoinfo(repopath,client,debug)
   log += "\ngit-svn-id: %(URL)s@%(revnum)d %(repos_UUID)s" % repoinfo
   print datalen(log)
   for mark in [ m for m in Mark.marks if 'file' in m ]:
      print "M %(perms)o :%(N)d %(reponame)s" % mark
   print ""

def mkdirp(filename):
   dirname = os.path.dirname(filename)
   print "mkdir -p %s" % dirname

def file_as_shell(filename,content,mode='>'):
   mkdirp(filename)
   marker = "HEREDOC%dDELIMITER" % time()
   print "cat %s %s <<%s\n%s\n%s" % (mode, filename, marker, content, marker)

def get_repoinfo(repopath,client,debug=False):
   repoinfo = client.info2(repopath,recurse=False)
   repoinfo = repoinfo[0][1]
   if debug:
      for k in repoinfo.keys():
         print "%s=%s" % (k,repoinfo[k])
   repoinfo['revnum'], repoinfo['newest_path'] = get_maxrev(repopath,client,debug)
   repoinfo['trunkless_root'] = repoinfo['URL'].replace('/trunk','',1)
   return repoinfo

def get_maxrev(repopath,client,debug=False):
   files_info = client.info2(repopath)
   maxrev = None
   newest = None
   for path, info in files_info:
      if not maxrev or maxrev < info['rev'].number:
         maxrev = info['rev'].number
         newest = path
      if debug:
         print "newmax=%d @ %s" % (maxrev, newest)
         print path[1+len(repopath):]
         for k in info.keys():
            print "\t%s\t%s" % (k, info[k])
            if k == 'wc_info':
               for k2 in info[k].keys():
                  print "\t\t%s\t%s" % (k2, info[k][k2])
   return maxrev, newest

if __name__ == '__main__':
   parser = OptionParser()
   parser.add_option('--wc', type="string", help="working copy path")
   parser.add_option('--debug', action="store_true", help="Debug mode")
   (options, args) = parser.parse_args()
   if not options.wc and len(args):
      options.wc = args[0]
      args = args[1:]
   if not options.wc:
      parser.error("No working copy")
   repopath = options.wc
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
      blob(repopath,client,path,info,debug=options.debug)
   commit(repopath,client,debug=options.debug)
