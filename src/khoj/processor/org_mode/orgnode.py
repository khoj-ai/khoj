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

import re
import datetime
from pathlib import Path
from os.path import relpath
from typing import List

indent_regex = re.compile(r'^ *')

def normalize_filename(filename):
   "Normalize and escape filename for rendering"
   if not Path(filename).is_absolute():
      # Normalize relative filename to be relative to current directory
      normalized_filename = f'~/{relpath(filename, start=Path.home())}'
   else:
      normalized_filename = filename
   escaped_filename = f'{normalized_filename}'.replace("[","\[").replace("]","\]")
   return escaped_filename

def makelist(filename):
   """
   Read an org-mode file and return a list of Orgnode objects
   created from this file.
   """
   ctr = 0

   f = open(filename, 'r')

   todos         = { "TODO": "", "WAITING": "", "ACTIVE": "",
                     "DONE": "", "CANCELLED": "", "FAILED": ""} # populated from #+SEQ_TODO line
   level         = ""
   heading       = ""
   bodytext      = ""
   tags          = list()      # set of all tags in headline
   closed_date   = ''
   sched_date    = ''
   deadline_date = ''
   logbook       = list()
   nodelist: List[Orgnode] = list()
   property_map  = dict()
   in_properties_drawer = False
   in_logbook_drawer = False
   file_title = f'{filename}'

   for line in f:
       ctr += 1
       heading_search = re.search(r'^(\*+)\s(.*?)\s*$', line)
       if heading_search:  # we are processing a heading line
          if heading: # if we have are on second heading, append first heading to headings list
             thisNode = Orgnode(level, heading, bodytext, tags)
             if closed_date:
                thisNode.closed = closed_date
                closed_date = ''
             if sched_date:
                thisNode.scheduled = sched_date
                sched_date = ""
             if deadline_date:
                thisNode.deadline = deadline_date
                deadline_date = ''
             if logbook:
                thisNode.logbook = logbook
                logbook = list()
             thisNode.properties = property_map
             nodelist.append( thisNode )
          property_map = {'LINE': f'file:{normalize_filename(filename)}::{ctr}'}
          level = heading_search.group(1)
          heading =  heading_search.group(2)
          bodytext = ""
          tags = list()       # set of all tags in headline
          tag_search = re.search(r'(.*?)\s*:([a-zA-Z0-9].*?):$',heading)
          if tag_search:
              heading = tag_search.group(1)
              parsedtags = tag_search.group(2)
              if parsedtags:
                 for parsedtag in parsedtags.split(':'):
                    if parsedtag != '': tags.append(parsedtag)
       else:      # we are processing a non-heading line
           if line[:10] == '#+SEQ_TODO':
              kwlist = re.findall(r'([A-Z]+)\(', line)
              for kw in kwlist: todos[kw] = ""

           # Set file title to TITLE property, if it exists
           title_search = re.search(r'^#\+TITLE:\s*(.*)$', line)
           if title_search and title_search.group(1).strip() != '':
               title_text = title_search.group(1).strip()
               if file_title == f'{filename}':
                  file_title = title_text
               else:
                  file_title += f' {title_text}'
               continue

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

           property_search = re.search(r'^\s*:([a-zA-Z0-9]+):\s*(.*?)\s*$', line)
           if property_search:
              # Set ID property to an id based org-mode link to the entry
              if property_search.group(1) == 'ID':
                 property_map['ID'] = f'id:{property_search.group(2)}'
              else:
                 property_map[property_search.group(1)] = property_search.group(2)
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
   thisNode = Orgnode(level, heading or file_title, bodytext, tags)
   thisNode.properties = property_map
   if sched_date:
      thisNode.scheduled = sched_date
   if deadline_date:
      thisNode.deadline = deadline_date
   if closed_date:
      thisNode.closed = closed_date
   if logbook:
      thisNode.logbook = logbook
   nodelist.append( thisNode )

   # using the list of TODO keywords found in the file
   # process the headings searching for TODO keywords
   for n in nodelist:
       todo_search = re.search(r'([A-Z]+)\s(.*?)$', n.heading)
       if todo_search:
           if todo_search.group(1) in todos:
               n.heading = todo_search.group(2)
               n.todo = todo_search.group(1)

       # extract, set priority from heading, update heading if necessary
       priority_search = re.search(r'^\[\#(A|B|C)\] (.*?)$', n.heading)
       if priority_search:
          n.priority = priority_search.group(1)
          n.heading = priority_search.group(2)

       # Set SOURCE property to a file+heading based org-mode link to the entry
       if n.level == 0:
         n.properties['LINE'] = f'file:{normalize_filename(filename)}::0'
         n.properties['SOURCE'] = f'[[file:{normalize_filename(filename)}]]'
       else:
         escaped_heading = n.heading.replace("[","\\[").replace("]","\\]")
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
        self._level = len(level)
        self._heading = headline
        self._body = body
        self._tags = tags          # All tags in the headline
        self._todo = ""
        self._priority = ""        # empty of A, B or C
        self._scheduled = ""       # Scheduled date
        self._deadline = ""        # Deadline date
        self._closed = ""          # Closed date
        self._properties = dict()
        self._logbook = list()     # List of clock-in, clock-out tuples representing logbook entries

        # Look for priority in headline and transfer to prty field

    @property
    def heading(self):
        """
        Return the Heading text of the node without the TODO tag
        """
        return self._heading

    @heading.setter
    def heading(self, newhdng):
        """
        Change the heading to the supplied string
        """
        self._heading = newhdng

    @property
    def body(self):
        """
        Returns all lines of text of the body of this node except the
        Property Drawer
        """
        return self._body

    @property
    def hasBody(self):
        """
        Returns True if node has non empty body, else False
        """
        return self._body and re.sub(r'\n|\t|\r| ', '', self._body) != ''

    @property
    def level(self):
        """
        Returns an integer corresponding to the level of the node.
        Top level (one asterisk) has a level of 1.
        """
        return self._level

    @property
    def priority(self):
        """
        Returns the priority of this headline: 'A', 'B', 'C' or empty
        string if priority has not been set.
        """
        return self._priority

    @priority.setter
    def priority(self, new_priority):
        """
        Change the value of the priority of this headline.
        Values values are '', 'A', 'B', 'C'
        """
        self._priority = new_priority

    @property
    def tags(self):
        """
        Returns the list of all tags
        For example, :HOME:COMPUTER: would return ['HOME', 'COMPUTER']
        """
        return self._tags

    @tags.setter
    def tags(self, newtags):
        """
        Store all the tags found in the headline.
        """
        self._tags = newtags

    def hasTag(self, tag):
        """
        Returns True if the supplied tag is present in this headline
        For example, hasTag('COMPUTER') on headling containing
        :HOME:COMPUTER: would return True.
        """
        return tag in self._tags

    @property
    def todo(self):
        """
        Return the value of the TODO tag
        """
        return self._todo

    @todo.setter
    def todo(self, new_todo):
        """
        Set the value of the TODO tag to the supplied string
        """
        self._todo = new_todo

    @property
    def properties(self):
        """
        Return the dictionary of properties
        """
        return self._properties

    @properties.setter
    def properties(self, new_properties):
        """
        Sets all properties using the supplied dictionary of
        name/value pairs
        """
        self._properties = new_properties

    def Property(self, property_key):
        """
        Returns the value of the requested property or null if the
        property does not exist.
        """
        return self._properties.get(property_key, "")

    @property
    def scheduled(self):
        """
        Return the scheduled date
        """
        return self._scheduled

    @scheduled.setter
    def scheduled(self, new_scheduled):
        """
        Set the scheduled date to the scheduled date
        """
        self._scheduled = new_scheduled

    @property
    def deadline(self):
        """
        Return the deadline date
        """
        return self._deadline

    @deadline.setter
    def deadline(self, new_deadline):
        """
        Set the deadline (due) date to the new deadline date
        """
        self._deadline = new_deadline

    @property
    def closed(self):
        """
        Return the closed date
        """
        return self._closed

    @closed.setter
    def closed(self, new_closed):
        """
        Set the closed date to the new closed date
        """
        self._closed = new_closed

    @property
    def logbook(self):
        """
        Return the logbook with all clocked-in, clocked-out date object pairs or empty list if nonexistent
        """
        return self._logbook

    @logbook.setter
    def logbook(self, new_logbook):
        """
        Set the logbook with list of clocked-in, clocked-out tuples for the entry
        """
        self._logbook = new_logbook

    def __repr__(self):
        """
        Print the level, heading text and tag of a node and the body
        text as used to construct the node.
        """
        # Output heading line
        n = ''
        for _ in range(0, self._level):
           n = n + '*'
        n = n + ' '
        if self._todo:
           n = n + self._todo + ' '
        if self._priority:
           n = n +  '[#' + self._priority + '] '
        n = n + self._heading
        n = "%-60s " % n     # hack - tags will start in column 62
        closecolon = ''
        for t in self._tags:
           n = n + ':' + t
           closecolon = ':'
        n = n + closecolon
        n = n + "\n"

        # Get body indentation from first line of body
        indent = indent_regex.match(self._body).group()

        # Output Closed Date, Scheduled Date, Deadline Date
        if self._closed or self._scheduled or self._deadline:
           n = n + indent
        if self._closed:
           n = n + f'CLOSED: [{self._closed.strftime("%Y-%m-%d %a")}] '
        if self._scheduled:
           n = n + f'SCHEDULED: <{self._scheduled.strftime("%Y-%m-%d %a")}> '
        if self._deadline:
           n = n + f'DEADLINE: <{self._deadline.strftime("%Y-%m-%d %a")}> '
        if self._closed or self._scheduled or self._deadline:
           n = n + '\n'

        # Ouput Property Drawer
        n = n + indent + ":PROPERTIES:\n"
        for key, value in self._properties.items():
           n = n + indent + f":{key}: {value}\n"
        n = n + indent + ":END:\n"

        # Output Body
        if self.hasBody:
           n = n + self._body

        return n
