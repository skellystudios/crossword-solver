import sys 
import en
import copy

'''


write_output_string()

'''

dictionary = {}
f = open('dict/mobythes3.aur') 

def main():

    global dictionary

    print "Beginning to extract to dictionary"
    extract_to_dictionary()


    print "Beginning to extend from wordnet"
    extend_dictionary_with_wordnet()


    print "Beginning to extend with conjugations"
    extend_dictionary_with_conjugations()


    print "Beginning to write output"
    write_output_string()

def extend_dictionary_with_conjugations():

    functions = [en.verb.present, en.verb.present_participle, en.verb.past, en.verb.past_participle, en.noun.plural]

    i = 0
    for base in dictionary.keys():
        if i%5000 == 0:
            print i
        i += 1
        for f in functions:
            try: 
                #print f
                base2 = f(base)
                if base == base2:
                    continue
                for word in dictionary[base]:
                    try:
                        # print "word: " + word
                        word2 = f(word)
                        # print "word2: " + word2
                        if word2 ==base:
                            continue
                        add_or_append(word2, base2)
                        add_or_append(base2, word2)
                    except:
                        continue
            except:
                continue    

            



def extend_dictionary_with_wordnet():

    for base in en.wordnet.all_nouns().keys() + en.wordnet.all_verbs().keys() + en.wordnet.all_adjectives().keys():
        for word in related_words(base):
            add_or_append(word, base)
            add_or_append(base, word)
  


def related_words(word):

    functions = [en.noun.hyponyms, en.noun.hypernym, en.noun.holonym, en.noun.meronym, 
                 en.verb.hyponyms, en.verb.hypernym, en.verb.holonym, en.verb.meronym,
                 en.adjective.hyponyms, en.adjective.hypernym, en.adjective.holonym, en.adjective.meronym]

    related = []

    for f in functions:
        for sensenumber in range(0,10):
            output = f(word, sense=sensenumber)
            if output == []:
                break
            related += output
    return en.list.flatten(related)


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


    # Make a list of all the keys, sorted, and map it to their numbers
    keysout = '['
    ids = dict()
    i = 0
    for key in sorted(dictionary.keys()):
        ids[key] = i
        keysout += '"' + key + '",'
        i += 1
    keysout = keysout[:-1]
    keysout += ']'

    fkeys = open('data/thesaurus-list-keys','w')
    fkeys.write(keysout)
    fkeys.close


    f2 = open('data/thesaurus-list','w')
    f2.write('[')
     
    for key in dictionary.keys():

        string = "(\"" + key + "\",["
        related = set(dictionary[key])
        for defn in related:
            string += str(ids[defn]) + ","
        string = string[:-1]
        string += "]),"
        f2.write(string)

    f2.write('("",[])]')
    f2.close()



if __name__ == "__main__":
    main()

