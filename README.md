# Cryptic Crossword Solver

Using AI and language parsing techniques to solve British-style cryptic crosswords in Haskell.

```
  *Solver> solve $ clue 1
  [Answer ("escort",ParsedClue (Clue ("companion shredded corset",6),"companion",[],AnagC ["shredded"] ["corset"]))]
```

## Running it

### Setting up the Thesaurus

Unless you're able to load the whole thesaurus into memory (at the cost of, like, 6GB, or something – doesn't work for me) then you'll need to run the following.  

```

  $ cd data/
  $ python create_thesaurus_files.py

```

If you do want to do it in memory, then uncomment out the `THESAURUS IN MEMORY` section in `Thesaurus.hs`


### Via Docker and Docker-Compose

```

  $ docker-compose build solver
  $ docker-compose run solver

```


## To Do

### Functioning
- [ ] Get all the clue types working
- [ ] Move the thesaurus function to be a system call rather than via files
- [ ] Make a more expansive, extensible thesaurus + wordlist
- [ ] Pull in a proper memoize library, and memoize the eval function
- [ ] Print the Answers in a nicer way

### Testing/Results
- [ ] Start gathering a better benchmark suite
- [ ] Make a function to run solver against the benchmarks: at a different level of timeout
