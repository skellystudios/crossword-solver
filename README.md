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
- [x] Get all the missing clue types working
  - [x] Firsts
  - [x] Lasts
  - [x] Before
  - [x] After
  - [ ] "Small" (small female = "f")
- [x] Pull in a proper memoize library, and memoize the eval function
- [ ] Print the Answers in a nicer way
- [ ] Support multi-word clues?

### Data
- [ ] Move the thesaurus function to be a system call rather than via files
- [ ] Make a more expansive, extensible thesaurus + wordlist
    - [ ] "X is a Y" types things -> I'm missing Ibsen, Scorsese, BSE, Waterloo (battle + song) etc.
    - [ ] More abbreviations -> Ag = silver
    - [ ] Phrasal verbs? "Expand on" + conjugates
- [ ] Scrape the web / books for more indicators words

### Testing/Results
- [ ] Start gathering a better benchmark suite – scrape the Times, Guardian, Telegraph etc.
- [ ] Make a function to run solver against the benchmarks: at a different level of timeout
