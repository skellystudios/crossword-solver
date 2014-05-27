f = open('mobythes3.aur')

dictionary = {}

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
    f2 = open('out.hs','w')
    
    f2.write('import qualified Data.Map as Map \n')
    f2.write('mapManual = \n')
    for key in dictionary.keys():
        string = "     Map.insert  \"" + key + "\" ["
        for defn in dictionary[key]:
            string = string + "\"" + defn + "\","
        string = string[:-1]
        string = string + "]\n"
        f2.write(string)

    f2.write('Map.insert "xxx" [] $ Map.empty')
    f2.close()


extract_to_dictionary()
write_output_string()