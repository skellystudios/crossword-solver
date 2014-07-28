import cookielib
import urllib2
import urllib
import re
from bs4 import BeautifulSoup
import string
from collections import defaultdict





class SparseList(list):
    def __setitem__(self, index, value):
        """Overrides list's __setitem__ to extend the list
           if the accessed index is out of bounds."""
        sparsity = index - len(self) + 1
        self.extend([None] * sparsity)
        list.__setitem__(self, index, value)

""" """

answers = dict()
clues = dict()
intersections = dict()

start = 3530
end = 3537

for c in range(start, end):	

	path='http://www.theguardian.com/crosswords/everyman/%d' % c
	try:
		req=urllib2.Request(path)
		page=urllib2.urlopen(req).read()
		print "Scraping from: " + path
	except:
		print "Couldn't find: " +  path
		continue

	soup = BeautifulSoup(page, "lxml")

	
	# Find the Clue Text
	for d in ["across", "down"]:
		for n in range(1,30):	
			try:
				raw_clue = soup.find(id="%d-%s-clue" % (n,d)).get_text()
				result = re.findall(r'[\d,]*\s*(.*)\s\(([\d,]*)\)', raw_clue)[0]
				clues[(c, n, d)] = (result[0], result[1])

			except:
				pass

	#intersections["8-down-5"] = "12-across-9";
	solutions = re.findall(r'intersections\["(\d*)-(.*)-(\d*)"\] = "(.*)"', page)
	for solution in solutions:
		intersections[(c, int(solution[0]), solution[1], int(solution[2]))] = solution[3]

			
					
	# Find the solution letters and compound them
	solutions = re.findall(r'solutions\["(\d*)-(.*)-(\d*)"\] = "(.)";', page) 
	for solution in solutions:
		ident = (c, int(solution[0]), solution[1]) 
		try:
			sstring = answers[ ident ]
		except:
			sstring = ""
		sstring += solution[3]
		answers[ ident ] = sstring

	# Find the intersections, because if we don't do it now then we'll never do it




#print clues
#print answers
#print intersections


for c in range(start,end):	
	for d in ["across", "down"]:
		for n in range(1,30):
			try:
				(clue, length) = clues[c,n,d]
				if "," in length:
					continue
				answer = answers[c,n,d]
				
				exclude = set(string.punctuation)
				clue_changed = ''.join(ch for ch in clue if ch not in exclude)

				#print str(n) + " " + d
				#print "%s (%s) - %s" % (clue, length, answer)
				print "(Clue (\"%s\",%s), \"%s\")," % (clue_changed, length, answer)
			except:
				pass



	