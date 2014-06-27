import sys 

f = open('dict/mobythes3.aur')

dictionary = {}


def create_sublists(x):
    out = []
    for i in range(1, len(x)):
        for j in range(2, len(x)+1):
            if i<j:
                out.append(x[i:j])
    return out

def join_sublists(lists):
    return map ((lambda x :' '.join(x).lower()), lists)

def extract_to_dictionary():

    for line in f:
        words = line.split(",")
        base = words[0]
        for word in words:
            clean_word = word.rstrip()
            add_or_append(base, clean_word)
            add_or_append(clean_word, base)
           


def add_or_append(key, value):
    try:
        baseList = dictionary[key]
        baseList.append(value)
        dictionary[key] = baseList
        #print "appending %s to key %s" % (value, key)
    except:
        dictionary[key] = [value]
   

def write_output_string():

    wordset =  join_sublists(create_sublists(sys.argv))
    print wordset
    f2 = open('Thesaurus.hs','w')
    f2.write('[')

     
    for key in dictionary.keys():

        if len(sys.argv) < 2 or key in wordset:
            string = "(\"" + key + "\",["
            for defn in dictionary[key]:
                string = string + "\"" + defn + "\","
            string = string[:-1]
            string = string + "]),"
            f2.write(string)


    f2.write('("",[])]')
    f2.close()


extract_to_dictionary()
write_output_string()