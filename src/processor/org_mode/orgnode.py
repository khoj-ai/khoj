# Copyright (c) 2010 Charles Cave
#
#  Permission  is  hereby  granted,  free  of charge,  to  any  person
#  obtaining  a copy  of  this software  and associated  documentation
#  files   (the  "Software"),   to  deal   in  the   Software  without
#  restriction, including without limitation  the rights to use, copy,
#  modify, merge, publish,  distribute, sublicense, and/or sell copies
#  of  the Software, and  to permit  persons to  whom the  Software is
#  furnished to do so, subject to the following conditions:
#
#  The above copyright notice and this permission notice shall be
#  included in all copies or substantial portions of the Software.
#
#  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
#  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
#  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
#  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
#  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
#  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
#  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#  SOFTWARE.

# Program written by Charles Cave   (charlesweb@optusnet.com.au)
# February - March 2009
# Version 2 - June 2009
#   Added support for all tags, TODO priority and checking existence of a tag
# More information at
#    http://members.optusnet.com.au/~charles57/GTD

"""
The Orgnode module consists of the Orgnode class for representing a
headline and associated text from an org-mode file, and routines for
constructing data structures of these classes.
"""

import re, sys
import datetime
from pathlib import Path
from os.path import relpath

def normalize_filename(filename):
   file_relative_to_home = f'~/{relpath(filename, start=Path.home())}'
   escaped_filename = f'{file_relative_to_home}'.replace("[","\[").replace("]","\]")
   return escaped_filename

def makelist(filename):
   """
   Read an org-mode file and return a list of Orgnode objects
   created from this file.
   """
   ctr = 0

   try:
      f = open(filename, 'r')
   except IOError:
      print(f"Unable to open file {filename}")
      print("Program terminating.")
      sys.exit(1)

   todos         = { "TODO": "", "WAITING": "", "ACTIVE": "",
                     "DONE": "", "CANCELLED": "", "FAILED": ""} # populated from #+SEQ_TODO line
   level         = 0
   heading       = ""
   bodytext      = ""
   tags          = set()      # set of all tags in headline
   closed_date   = ''
   sched_date    = ''
   deadline_date = ''
   logbook       = list()
   nodelist      = []
   propdict      = dict()
   in_properties_drawer = False
   in_logbook_drawer = False

   for line in f:
       ctr += 1
       hdng = re.search(r'^(\*+)\s(.*?)\s*$', line)
       if hdng:  # we are processing a heading line
          if heading: # if we have are on second heading, append first heading to headings list
             thisNode = Orgnode(level, heading, bodytext, tags)
             if closed_date:
                thisNode.setClosed(closed_date)
                closed_date = ''
             if sched_date:
                thisNode.setScheduled(sched_date)
                sched_date = ""
             if deadline_date:
                thisNode.setDeadline(deadline_date)
                deadline_date = ''
             if logbook:
                thisNode.setLogbook(logbook)
                logbook = list()
             thisNode.setProperties(propdict)
             nodelist.append( thisNode )
          propdict = {'LINE': f'file:{normalize_filename(filename)}::{ctr}'}
          level = hdng.group(1)
          heading =  hdng.group(2)
          bodytext = ""
          tags = set()       # set of all tags in headline
          tagsrch = re.search(r'(.*?)\s*:([a-zA-Z0-9].*?):$',heading)
          if tagsrch:
              heading = tagsrch.group(1)
              parsedtags = tagsrch.group(2)
              if parsedtags:
                 for parsedtag in parsedtags.split(':'):
                    if parsedtag != '': tags.add(parsedtag)
       else:      # we are processing a non-heading line
           if line[:10] == '#+SEQ_TODO':
              kwlist = re.findall(r'([A-Z]+)\(', line)
              for kw in kwlist: todos[kw] = ""

           # Ignore Properties Drawers Completely
           if re.search(':PROPERTIES:', line):
              in_properties_drawer=True
              continue
           if in_properties_drawer and re.search(':END:', line):
              in_properties_drawer=False
              continue

           # Ignore Logbook Drawer Start, End Lines
           if re.search(':LOGBOOK:', line):
              in_logbook_drawer=True
              continue
           if in_logbook_drawer and re.search(':END:', line):
              in_logbook_drawer=False
              continue

           # Extract Clocking Lines
           clocked_re = re.search(r'CLOCK:\s*\[([0-9]{4}-[0-9]{2}-[0-9]{2} [a-zA-Z]{3} [0-9]{2}:[0-9]{2})\]--\[([0-9]{4}-[0-9]{2}-[0-9]{2} [a-zA-Z]{3} [0-9]{2}:[0-9]{2})\]', line)
           if clocked_re:
              # convert clock in, clock out strings to datetime objects
              clocked_in = datetime.datetime.strptime(clocked_re.group(1), '%Y-%m-%d %a %H:%M')
              clocked_out = datetime.datetime.strptime(clocked_re.group(2), '%Y-%m-%d %a %H:%M')
              # add clocked time to the entries logbook list
              logbook += [(clocked_in, clocked_out)]
              line = ""

           prop_srch = re.search(r'^\s*:([a-zA-Z0-9]+):\s*(.*?)\s*$', line)
           if prop_srch:
              # Set ID property to an id based org-mode link to the entry
              if prop_srch.group(1) == 'ID':
                 propdict['ID'] = f'id:{prop_srch.group(2)}'
              else:
                 propdict[prop_srch.group(1)] = prop_srch.group(2)
              continue

           cd_re = re.search(r'CLOSED:\s*\[([0-9]{4})-([0-9]{2})-([0-9]{2})', line)
           if cd_re:
              closed_date = datetime.date(int(cd_re.group(1)),
                                         int(cd_re.group(2)),
                                         int(cd_re.group(3)) )
           sd_re = re.search(r'SCHEDULED:\s*<([0-9]+)\-([0-9]+)\-([0-9]+)', line)
           if sd_re:
              sched_date = datetime.date(int(sd_re.group(1)),
                                         int(sd_re.group(2)),
                                         int(sd_re.group(3)) )
           dd_re = re.search(r'DEADLINE:\s*<(\d+)\-(\d+)\-(\d+)', line)
           if dd_re:
              deadline_date = datetime.date(int(dd_re.group(1)),
                                            int(dd_re.group(2)),
                                            int(dd_re.group(3)) )

           # Ignore property drawer, scheduled, closed, deadline, logbook entries and # lines from body
           if not in_properties_drawer and not cd_re and not sd_re and not dd_re and not clocked_re and line[:1] != '#':
               bodytext = bodytext + line

   # write out last node
   thisNode = Orgnode(level, heading, bodytext, tags)
   thisNode.setProperties(propdict)
   if sched_date:
      thisNode.setScheduled(sched_date)
   if deadline_date:
      thisNode.setDeadline(deadline_date)
   if closed_date:
      thisNode.setClosed(closed_date)
   if logbook:
      thisNode.setLogbook(logbook)
   nodelist.append( thisNode )

   # using the list of TODO keywords found in the file
   # process the headings searching for TODO keywords
   for n in nodelist:
       h = n.Heading()
       todoSrch = re.search(r'([A-Z]+)\s(.*?)$', h)
       if todoSrch:
           if todoSrch.group(1) in todos:
               n.setHeading( todoSrch.group(2) )
               n.setTodo ( todoSrch.group(1) )

       # extract, set priority from heading, update heading if necessary
       prtysrch = re.search(r'^\[\#(A|B|C)\] (.*?)$', n.Heading())
       if prtysrch:
          n.setPriority(prtysrch.group(1))
          n.setHeading(prtysrch.group(2))

       # Set SOURCE property to a file+heading based org-mode link to the entry
       escaped_heading = n.Heading().replace("[","\\[").replace("]","\\]")
       n.properties['SOURCE'] = f'[[file:{normalize_filename(filename)}::*{escaped_heading}]]'

   return nodelist

######################
class Orgnode(object):
    """
    Orgnode class represents a headline, tags and text associated
    with the headline.
    """
    def __init__(self, level, headline, body, tags):
        """
        Create an Orgnode object given the parameters of level (as the
        raw asterisks), headline text (including the TODO tag), and
        first tag. The makelist routine postprocesses the list to
        identify TODO tags and updates headline and todo fields.
        """
        self.level = len(level)
        self.headline = headline
        self.body = body
        self.tags = set(tags)     # All tags in the headline
        self.todo = ""
        self.prty = ""            # empty of A, B or C
        self.scheduled = ""       # Scheduled date
        self.deadline = ""        # Deadline date
        self.closed = ""          # Closed date
        self.properties = dict()
        self.logbook = list()     # List of clock-in, clock-out tuples representing logbook entries

        # Look for priority in headline and transfer to prty field

    def Heading(self):
        """
        Return the Heading text of the node without the TODO tag
        """
        return self.headline

    def setHeading(self, newhdng):
        """
        Change the heading to the supplied string
        """
        self.headline = newhdng

    def Body(self):
        """
        Returns all lines of text of the body of this node except the
        Property Drawer
        """
        return self.body

    def Level(self):
        """
        Returns an integer corresponding to the level of the node.
        Top level (one asterisk) has a level of 1.
        """
        return self.level

    def Priority(self):
        """
        Returns the priority of this headline: 'A', 'B', 'C' or empty
        string if priority has not been set.
        """
        return self.prty

    def setPriority(self, newprty):
        """
        Change the value of the priority of this headline.
        Values values are '', 'A', 'B', 'C'
        """
        self.prty = newprty

    def Tags(self):
        """
        Returns the set of all tags
        For example, :HOME:COMPUTER: would return {'HOME', 'COMPUTER'}
        """
        return self.tags

    def hasTag(self, srch):
        """
        Returns True if the supplied tag is present in this headline
        For example, hasTag('COMPUTER') on headling containing
        :HOME:COMPUTER: would return True.
        """
        return srch in self.tags

    def setTags(self, newtags):
        """
        Store all the tags found in the headline.
        """
        self.tags = set(newtags)

    def Todo(self):
        """
        Return the value of the TODO tag
        """
        return self.todo

    def setTodo(self, value):
        """
        Set the value of the TODO tag to the supplied string
        """
        self.todo = value

    def setProperties(self, dictval):
        """
        Sets all properties using the supplied dictionary of
        name/value pairs
        """
        self.properties = dictval

    def Property(self, keyval):
        """
        Returns the value of the requested property or null if the
        property does not exist.
        """
        return self.properties.get(keyval, "")

    def setScheduled(self, dateval):
        """
        Set the scheduled date using the supplied date object
        """
        self.scheduled = dateval

    def Scheduled(self):
        """
        Return the scheduled date object or null if nonexistent
        """
        return self.scheduled

    def setDeadline(self, dateval):
        """
        Set the deadline (due) date using the supplied date object
        """
        self.deadline = dateval

    def Deadline(self):
        """
        Return the deadline date object or null if nonexistent
        """
        return self.deadline

    def setClosed(self, dateval):
        """
        Set the closed date using the supplied date object
        """
        self.closed = dateval

    def Closed(self):
        """
        Return the closed date object or null if nonexistent
        """
        return self.closed

    def setLogbook(self, logbook):
        """
        Set the logbook with list of clocked-in, clocked-out tuples for the entry
        """
        self.logbook = logbook

    def Logbook(self):
        """
        Return the logbook with all clocked-in, clocked-out date object pairs or empty list if nonexistent
        """
        return self.logbook

    def __repr__(self):
        """
        Print the level, heading text and tag of a node and the body
        text as used to construct the node.
        """
        # This method is not completed yet.
        n = ''
        for _ in range(0, self.level):
           n = n + '*'
        n = n + ' ' + self.todo + ' '
        if self.prty:
           n = n +  '[#' + self.prty + '] '
        n = n + self.headline
        n = "%-60s " % n     # hack - tags will start in column 62
        closecolon = ''
        for t in self.tags:
           n = n + ':' + t
           closecolon = ':'
        n = n + closecolon
        n = n + "\n"

        # Output Closed Date, Scheduled Date, Deadline Date
        if self.closed:
           n = n + f'CLOSED: [{self.closed.strftime("%Y-%m-%d %a")}] '
        if self.scheduled:
           n = n + f'SCHEDULED: <{self.scheduled.strftime("%Y-%m-%d %a")}> '
        if self.deadline:
           n = n + f'DEADLINE: <{self.deadline.strftime("%Y-%m-%d %a")}> '
        if self.closed or self.scheduled or self.deadline:
           n = n + '\n'

        # Ouput Property Drawer
        n = n + ":PROPERTIES:\n"
        for key, value in self.properties.items():
           n = n + f":{key}: {value}\n"
        n = n + ":END:\n"

        n = n + self.body

        return n
