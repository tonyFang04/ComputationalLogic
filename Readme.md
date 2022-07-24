# Computational Logical Coursework Report

Tony Fang

### Negation

First goal of this assignment is to handle negation such as the following example.

> Every teacher is happy. 
> Donald is not happy. 
> Therefore, Donald is not a teacher.

To do this, first I editted the ```proloxa_grammer.pl``` file. I added "donald" as a proper noun and "happy" as an adjective. This is achieved by adding the two following lines to ```proloxa_grammer.pl```.

```
proper_noun(s,donald) --> [donald].
pred(happy,  1,[a/happy]).
pred(teacher,    1,[n/teacher]).
```

Now prolexa can interpret utterance "donald is happy".

```
prolexa> "donald is happy".
*** utterance(donald is happy)
*** rule([(happy(donald):-true)])
*** answer(I will remember that donald is happy)
I will remember that donald is happy
```

From the example above, we can see that prolexa takes "donald is happy" as input and convert that sentence into a rule ```[(happy(donald):-true)]```. This suggests that if I want prolexa to interpret "donald is not happy", prolexa need to convert that into a coressponding predicate form that expresses "donald is not happy". The following predicate forms are all potential choices:

```
[(not(happy(donald)):-true)]
[(happy(donald):-false)]
[:-(happy(donald))]
```

For the sake of making minimal changes to the code later on, ```[(not(happy(donald)):-true)]``` was chosen as the corresponding predicate form for expression "donald is not happy".

Inspections on ```prolexa.pl``` made me realise that the utterances and rules are converted back and forth from each other using the "sentence/3" predicate in ```proloxa_grammer.pl```. So the goal is to get the "sentence/3" predicate to handle query ```?-sentence([(not(happy(donald)):-true)],[donald, is, not, happy], [])```. This is achieved by adding

```
sentence1([(not(L):-true)]) --> proper_noun(N,X),not_verb_phrase(N,X=>L).
not_verb_phrase(s,M) --> [is, not],property(s,M).
not_verb_phrase(p,M) --> [are, not],property(p,M).
```

to ```proloxa_grammer.pl```. Now query ```?-sentence([(not(happy(donald)):-true)],[donald, is, not, happy], []).``` returns ```true```. The ```proloxa_grammer.pl``` should not only be able to interpret the present tense single and plural of "be" verbs, but also handle the present tense single and plural of the rest of the verbs such that it can handle "tweety does not fly". This requires two addition lines of code into ```proloxa_grammer.pl```:

```
not_verb_phrase(s,M) --> [does, not],iverb(p,M).
not_verb_phrase(p,M) --> [do, not],iverb(p,M).
```

Notice that the only difference between these two lines is the input of the first argument in predicate ```not_verb_phrase/4```. The "s" stands for "single" and the "p" stands for "plural". Now query ```?-sentence([(not(fly(tweety)):-true)],[tweety, does, not, fly], []).``` returns true.

Prolexa should also be able to interpret statements like "every human is not mortal" or "all birds do not fly". This requires the "sentence/3" predicate in ```proloxa_grammer.pl``` be able to query ```?-sentence([(not(mortal(X)):-human(X))],[every, human, is, not, mortal], []).``` and ```?-sentence([(not(fly(X)):-bird(X))],[all, birds, do, not, fly], []).``` This requires understanding what the arguments "N", "M1" and "M2" in the "determiner/6" predicate stands for in the follwing line:

```
sentence1(C) --> determiner(N,M1,M2,C),noun(N,M1),verb_phrase(N,M2).
```

Querying ```?-determiner(N,M1,M2,[(happy(donald):-true)],E,[]).```  showed the following:

```
N = s,
M1 = _A=>true,
M2 = _A=>happy(donald),
E = [every] ;
N = p,
M1 = _A=>true,
M2 = _A=>happy(donald),
E = [all].
```

This means that "N" corresponds to the whether the noun is single or plural, "M1" corresponds to the body of the rule and "M2" corresponds to the head of the rule. Therefore,

```
sentence1(C) --> determiner(N,M1,M2,C),noun(N,M1),verb_phrase(N,M2).
```

can be rewritten as:

```
sentence1([(H:-B)]) --> determiner(N,M1=>B,M2=>H,[(H:-B)]),noun(N,M1=>B),verb_phrase(N,M2=>H).
sentence1([(not(H):-B)]) --> determiner(N,M1=>B,M2=>H,[(H:-B)]),noun(N,M1=>B),not_verb_phrase(N,M2=>H).
```
The first line handles queries like ```sentence1([(mortal(X):-human(X))],[every, human, is, mortal],[]).```. The second line handles queries like ```sentence1([(not(mortal(X)):-human(X))],[every, human, is, not, mortal],[]).``` Notice that the arguments in the "deteminer/6" and the "noun/4" predicates are exactly the same. This is because negation is only added to the part of the sentence after "is", which corresponds to the head of the rule. Now prolexa can handle the following utterances.

Finally, we need to add the Modus Tollens reasoning (if A then B. Not B. Therefore not A), which is the generalized case of 

> Every teacher is happy. 
> Donald is not happy. 
> Therefore, Donald is not a teacher.

to the "prolexa_engine.pl". This is achieved by adding the following lines to the "prove_rb/4" predicate:

```
prove_rb(not(A),Rulebase,P0,P):-
    find_clause((B:-A),Rule,Rulebase),
	prove_rb(not(B),Rulebase,[p(not(A),Rule)|P0],P).
```
which means, in order to prove not(A) is true, find a clause that states "if A then B", and prove not(B) is true. And now the program can interpret negation.

```
1 ?- prolexa_cli.
prolexa> "forget everything you know".
*** utterance(forget everything you know)
*** goal(retractall(prolexa:stored_rule(_55582,_55584)))
*** answer(I am a blank slate)
I am a blank slate


prolexa> "peter is human". 
*** utterance(peter is human)
*** rule([(human(peter):-true)])
*** answer(I will remember that peter is human)
I will remember that peter is human


prolexa> "every human is mortal".
*** utterance(every human is mortal)
*** rule([(mortal(_56282):-human(_56282))])
*** answer(I will remember that every human is mortal)
I will remember that every human is mortal


prolexa> "peter is mortal".
*** utterance(peter is mortal)
*** rule([(mortal(peter):-true)])
*** answer(I already knew that peter is mortal)
I already knew that peter is mortal


prolexa> "explain why peter is mortal".
*** utterance(explain why peter is mortal)
*** goal(explain_question(mortal(peter),_57092,_56882))
*** answer(peter is human; every human is mortal; therefore peter is mortal)
peter is human; every human is mortal; therefore peter is mortal


prolexa> "donald is not happy".
*** utterance(donald is not happy)
*** rule([(not(happy(donald)):-true)])
*** answer(I will remember that donald is not happy)
I will remember that donald is not happy


prolexa> "every teacher is happy".
*** utterance(every teacher is happy)
*** rule([(happy(_58310):-teacher(_58310))])
*** answer(I will remember that every teacher is happy)
I will remember that every teacher is happy


prolexa> "donald is not a teacher".
*** utterance(donald is not a teacher)
*** rule([(not(teacher(donald)):-true)])
*** answer(I already knew that donald is not a teacher)
I already knew that donald is not a teacher


prolexa> "explain why donald is not a teacher".
*** utterance(explain why donald is not a teacher)
*** goal(explain_question(not(teacher(donald)),_59272,_58998))
*** answer(donald is not happy; every teacher is happy; therefore donald is not a teacher)
donald is not happy; every teacher is happy; therefore donald is not a teacher


prolexa> "tweety does not fly".
*** utterance(tweety does not fly)
*** rule([(not(fly(tweety)):-true)])
*** answer(I will remember that tweety does not fly)
I will remember that tweety does not fly


prolexa> "every bird flies".
*** utterance(every bird flies)
*** rule([(fly(_60480):-bird(_60480))])
*** answer(I will remember that every bird flies)
I will remember that every bird flies
prolexa> "explain why tweety is not a bird".


*** utterance(explain why tweety is not a bird)
*** goal(explain_question(not(bird(tweety)),_60924,_60654))
*** answer(tweety does not fly; every bird flies; therefore tweety is not a bird)
tweety does not fly; every bird flies; therefore tweety is not a bird
prolexa>
```

### Conjunction

The meta interpreter in ```prolexa_engine.pl``` can handle conjunction. However, the grammer for conjunction is not implemented. Therefore, to complete conjunction for prolexa, the following clauses are added:

```
sentence1([((H1,H2):-B)]) --> determiner(N,M1=>B,M2=>H1,[(H1:-B)]),noun(N,M1=>B),verb_phrase(N,M2=>H1),property_and_list(N,(M2=>H2)).
sentence1([((H1,H2):-true)]) --> proper_noun(N,X),verb_phrase(N,X=>H1),property_and_list(N,(X=>H2)).
property_and_list(N,X=>M) --> [and],property(N,X=>M).
property_and_list(N,X=>(M1,M2)) --> [and],property(N,X=>M1), property_and_list(N,X=>M2).
```

The ```property_and_list/4``` predicate allows the grammer to handle utterences cpnsisting multiple "and", e.g.,

```
?- property_and_list(A,B,[and,human,and,mortal,and,happy,and,red,and,blue],[]). 
B = _A=>(human(_A), mortal(_A), happy(_A), red(_A), blue(_A)) ;
false.
```

One thing to note is the ```X=>``` in the ```property_and_list/4``` which ensures that the atoms in conjuntion with one another share the same argument such that ```(human(_A), mortal(_A),...)``` all share the same argument (_A).

Now prolexa can deal with utterences like "peter is happy and mortal" and "every human is happy and mortal". It can also handle reasonings like "Peter is human. Every human is happy. Peter is human. Every huamn is mortal. Therefore, Peter is happy and mortal".

```
prolexa> "forget everything".
*** utterance(forget everything)
*** goal(retractall(prolexa:stored_rule(_56902,_56904)))
*** answer(I am a blank slate)
I am a blank slate
prolexa> "peter is human".
*** utterance(peter is human)
*** rule([(human(peter):-true)])
*** answer(I will remember that peter is human)
I will remember that peter is human
prolexa> "every human is happy".
*** utterance(every human is happy)
*** rule([(happy(_57380):-human(_57380))])
*** answer(I will remember that every human is happy)
I will remember that every human is happy
prolexa> "every human is mortal".
*** utterance(every human is mortal)
*** rule([(mortal(_57746):-human(_57746))])
*** answer(I will remember that every human is mortal)
I will remember that every human is mortal
prolexa> "explain why peter is happy and mortal".
*** utterance(explain why peter is happy and mortal)
*** goal(explain_question((happy(peter),mortal(peter)),_58214,_57944))
*** answer(peter is human; every human is mortal; peter is human; every human is happy; therefore peter is happy and mortal)
peter is human; every human is mortal; peter is human; every human is happy; therefore peter is happy and mortal
prolexa>
```

### Disjunction
#### Grammer
The added grammer for disjunction is very similar to that of conjunction, except the ```property_or_list/4``` handles utterences that contain "or" rather than "and".

```
sentence1([((H1;H2):-true)]) --> proper_noun(N,X),verb_phrase(N,X=>H1),property_or_list(N,(X=>H2)).
sentence1([((H1;H2):-B)]) --> determiner(N,M1=>B,M2=>H1,[(H1:-B)]),noun(N,M1=>B),verb_phrase(N,M2=>H1),property_or_list(N,(M2=>H2)).
property_or_list(N,X=>M) --> [or],property(N,X=>M).
property_or_list(N,X=>(M1;M2)) --> [or],property(N,X=>M1), property_or_list(N,X=>M2).
```

#### Meta Interpreter

The goal is to add clauses to the meta interpreter such that it can handle cases like "every human is happy or mortal. peter is human. peter is not mortal. therefore peter is happy". This means the meta interpreter needs to handle the following reasoning pattern: "if p then q or r. p. not r. therefore q", or "if p then q or r. p. not q. therefore r". This can be also written as "if p then q or r. p and not r. therefore q", or "if p then q or r. p and not q. therefore r". The corresponding added clauses to the meta interpreter therefore is the following:

```
prove_rb(A,Rulebase,P0,P):-
    (find_clause((A;B:-C),Rule,Rulebase);
	find_clause((B;A:-C),Rule,Rulebase)),
	prove_rb((C,not(B)),Rulebase,[p(A,Rule)|P0],P).
```

Now prolexa can handle utterences like the following:

```
?- prolexa_cli.                   
prolexa> "tell me everything you know".
*** utterance(tell me everything you know)
*** goal(all_rules(_58748))
*** answer(peter is human. every human is happy or mortal. peter is not mortal)
peter is human. every human is happy or mortal. peter is not mortal
prolexa> "explain why peter is happy". 
*** utterance(explain why peter is happy)
*** goal(explain_question(happy(peter),_59842,_59632))
*** answer(peter is not mortal; peter is human; every human is happy or mortal; therefore peter is happy)
peter is not mortal; peter is human; every human is happy or mortal; therefore peter is happy
prolexa> "forget peter is not mortal".
*** utterance(forget peter is not mortal)
*** goal(retractall(prolexa:stored_rule(_61074,[(not(mortal(peter)):-true)])))
*** answer(I erased it from my memory)
I erased it from my memory
prolexa> "peter is not happy".
*** utterance(peter is not happy)
*** rule([(not(happy(peter)):-true)])
*** answer(I will remember that peter is not happy)
I will remember that peter is not happy
prolexa> "explain why peter is mortal".
*** utterance(explain why peter is mortal)
*** goal(explain_question(mortal(peter),_61694,_61484))
*** answer(peter is not happy; peter is human; every human is happy or mortal; therefore peter is mortal)
peter is not happy; peter is human; every human is happy or mortal; therefore peter is mortal
prolexa>
```