f = open('wordlist')

dictionary = {}


def write_output_string():
    limit = 0

    f2 = open('wordlist.hs','w')
    
    f2.write('module Wordlist (wordlist) where \nimport Data.Set (Set,fromList) \n')
    f2.write('wordlist = Data.Set.fromList [')
    string = ""
    for line in f:
        out = line.decode('utf-8', 'ignore')
        string += "\"" + out[:-2] + "\", "
        limit += 1
        if limit > 1000*1000: 
            break
    string = string[:-2]
    string = string + "]\n"
    f2.write(string)

    f2.close()

write_output_string()