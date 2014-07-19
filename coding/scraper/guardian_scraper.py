import cookielib
import urllib2
import urllib
import re
from bs4 import BeautifulSoup
from collections import defaultdict





class SparseList(list):
    def __setitem__(self, index, value):
        """Overrides list's __setitem__ to extend the list
           if the accessed index is out of bounds."""
        sparsity = index - len(self) + 1
        self.extend([None] * sparsity)
        list.__setitem__(self, index, value)

""" """


path='http://www.theguardian.com/crosswords/cryptic/26315'

req=urllib2.Request(path)
page=urllib2.urlopen(req).read()

soup = BeautifulSoup(page, "lxml")

answers = dict()
clues = dict()

for d in ["across", "down"]:
	for n in range(1,30):	
		try:
			raw_clue = soup.find(id="%d-%s-clue" % (n,d)).get_text()
			result = re.findall(r'[\d,]*\s*(.*)\s\(([\d,]*)\)', raw_clue)[0]

			clues[(n, d)] = (result[0], result[1])
		except:
			pass

# 					solutions["22-down-4"] = "T";
				

solutions = re.findall(r'solutions\["(\d*)-(.*)-(\d*)"\] = "(.)";', page) 


for solution in solutions:
	ident = (int(solution[0]), solution[1]) 
	try:
		string = answers[ ident ]
	except:
		string = ""
	string += solution[3]
	answers[ ident ] = string

print clues
print answers



for d in ["across", "down"]:
	for n in range(1,30):
		try:
			(clue, length) = clues[n,d]
			answer = answers[n,d]
			print str(n) + " " + d
			print "%s (%s) - %s" % (clue, length, answer)
		except:
			pass



	