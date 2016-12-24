import re
import os

with open('thesaurus', 'r') as f:
    for i, line in enumerate(f):
        try:
            m= re.search('\("(.*?)"', line)
            word = m.group(1)

            directory = "words/" + word[0] + "/"
            if not os.path.exists(directory):
                os.makedirs(directory)

            f2 = open(directory + word, 'w')
            f2.write(line + "\n")
            f2.close()
        except:
            print "failed on line " + str(i)
