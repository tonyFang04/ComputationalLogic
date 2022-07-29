# Computational Logic Coursework Report By Tony Fang

### Author [Tony Fang](https://github.com/tonyFang04) 

This is the report for the coursework of Computational Logic.

All code can be found at this [repo](https://github.com/tonyFang04/ComputationalLogic)

### Contents

1. [Introduction to Prolexa](#motivation)
   1. [My Objectives](#objectives)
2. [Determine Prolexa source code Locations that needs to be extended](#method)
3. [Adding Clauses to ```sentence1/3``` and ```prove_rb/4```](#implementation)
   1. [Objectives 1, 2 and 3: Negation](#negation)
   2. [Objectives 4, 5 and 6: Conjunction](#conjunction)
   3. [Objectives 7, 8 and 9: Disjunction](#disjunction)
4. [Conclusion](#limitations)


# <a name="motivation">Introduction to Prolexa followed by My Objectives #
   [Prolexa](https://github.com/simply-logical/ComputationalLogic/tree/prolexa-plus) is a simple question-answering assistant written in Prolog. It can be run on the command line, in Google Colab, or integrated with Alexa. For details on how to run Prolexa, please go to repo [https://github.com/simply-logical/ComputationalLogic/tree/prolexa-plus
](https://github.com/simply-logical/ComputationalLogic/tree/prolexa-plus
).
   
   At runtime, the assistant takes in an utterence as input, processes the utterence, then wait for the next utterence. The assistant processes the utterence in one of the 4 following ways:
   
   - A: it checks whether the utterence coressponds to a statement. If so, it will convert the utterence into a Prolog rule. It will then attempt to infer this rule from all the stored rules in its rule base. If the inference process is successful, it will output "I already know \<utterence\>". If the inference process fails, it will output "I will remember that \<utterence\>". For example, if the assistant takes in an utterence "peter is happy", it will first convert this utterence to \[(happy(peter):-true)\], which is the corresponding Prolog rule to "peter is happy". It will then query whether \[(happy(peter):-true)\] can be successfully inferred from its rule base. If the query is successful, it will output "I already know peter is happy". If the query is not successful, it will output "I will remember that peter is happy".
   - B: it checks whether the utterence corresponds to a question and attempts to answer the question.
   - C: it checks whether the utterence corresponds to a command. If so, it will execute the command and output a corresponding message. For example, if the assistant receives utterence "explain why \<statement\>", it will attempt to infer the rule corresponding to the \<statement\>. If this rule can successfully be inferred, it will output a message that containts the steps of the inference process. For example, if the rule base contains rules corresponding to "peter is human" and "every human is mortal", then the utterence "explain why peter is mortal" will result in an output message of "peter is human. every human is mortal. therefore peter is mortal". If the inference process fails, it will simply output "Sorry, I don't think this is the case".
   - D: if an utterence cannot be processed following the steps in A, B, or C, the assistant will simply output "I heard you say, \<utterence\>, could you rephrase that please?
   
   However, Prolexa can only process utterences that do not contain word "not", "and", and "or". This means that Prolexa cannot handle utterences that contains negation, conjunction and disjunction. Therefore, my goal for this coursework is to extend Prolexa such that it can process utterences containing negation, conjunction and disjunction. 
   
   ## <a name="objectives">My Objectives #
   
   My objectives for this coursework is to extend Prolexa such that:
   
   1. utterence "peter is not happy" can be converted to rule \[(not(happy(peter)):-true)\].
   2. utterence "every teacher is not happy" can be converted to rule \[(not(happy(X)):-teacher(X))\].
   3. given the rule base contains rules corresponding to "peter is not happy" and "every teacher is happy", Prolexa can infer utterence "peter is not a teacher".
   4. utterence "peter is happy and mortal and red" can be converted to rule \[(happy(peter),mortal(peter),red(peter)):-true)\].
   5. utterence "every human is happy and mortal and red" can be converted to rule \[(happy(X),mortal(X),red(X)):-human(X))\].
   6.  given the rule base contains rules corresponding to "peter is human", "every human is happy", "every human is mortal", "every human is red", it can infer utterence "peter is happy and mortal and red".
   7. utterence "peter is happy or mortal or red" can be converted to rule \[(happy(peter);mortal(peter);red(peter)):-true)\].
   8. utterence "every human is happy or mortal or red" can be converted to rule \[(happy(X);mortal(X);red(X)):-human(X))\].
   9. given the rule base contains rules corresponding to "peter is human", "every human is happy or mortal", "peter is not happy", it can infer utterence "peter is mortal".

   
# <a name="method">Determine Prolexa source code Locations that needs to be extended #
   
   To achieve the objectives listed in the previous section, I need to determine where in the source code that I need to add more clauses. This means I need to find out what sections of the Prolexa source code converts statements/sentences into rules, and what sections handles the inference. To achieve this, I first cloned the [Prolexa repo](https://github.com/simply-logical/ComputationalLogic/tree/prolexa-plus) to my own device locally. I then started investigating the .pl files in the ```prolexa/prolog``` directory by inspecting the files by eyes, testing certain rules out of interest by running queries on the command line and using the swipl graphical debugger.
   
   I started my inspection from ```prolexa.pl``` because running ```swipl prolexa.pl``` starts Prolexa. Line 1 in ```prolexa.pl``` suggested that ```prolexa_cli``` is the command that starts the question-answering in Prolexa. The body of rule ```prolexa_cli/1``` reads the input utterence from comand line, then calls rule ```handle_utterence/3```, which processes the input utterence then outputs a message.
   
   ```handle_utterence/3``` first tokenizes the input utterence to a list of words. It then determines whether the utterence can be processed in 1 of the 4 ways described in [Motivation](#motivation). To determine whether the utterence corresponds to a statement (or a sentence that is not a question), a question, a command or none of the 3. This is achieved by calling  rule ```sentence/3```, ```question/3```, ```command/3``` from  ```prolexa_grammer.pl``` respectively. 
   
   However, out of the 3 rules only ```sentence/3``` is responsible for converting tokenized utterences into Prolog rules whilst the actual convertion in ```sentence1/3``` is handled entirely by ```sentence1/3```. ```question/3``` only converts utterences into facts to be queried and ```command/3``` calls ```sentence/3``` and ```sentence1/3``` to convert sentences to rules. For example, querying ```sentence/3``` and ```question/3``` produces the following results:
   
   ```
   3 ?- sentence(A,[every,human,is,mortal],[]).
   A = [(mortal(_A):-human(_A))] .

   2 ?- question(A,[who,is,mortal],[]). 
   A = mortal(_) .
   ```
   
   whilst Line 136 and 140 in ```prolexa_grammer.pl``` showed rule ```command/3``` calling ```sentence/3``` and ```sentence1/3```
   
   ```
   command(g(retractall(prolexa:stored_rule(_,C)),"I erased it from my memory")) --> forget,sentence(C). 
   command(g(explain_question(Q,_,Answer),Answer)) --> [explain,why],sentence1([(Q:-true)]).
   ```
   
   Therefore, in order to achieve objectives 1, 2, 4, 5, 7 and 8, I only need to add more clauses to rule ```sentence1/3```.
   
   After ```sentence/3``` is called in ```prolexa.pl```, it then calls ```known_rule/2``` in ```prolexa_engine.pl```. ```known_rule/2``` then calls ```prove_rb/2``` which then calls ```prove_rb/4```. ```prove_rb/4``` is a meta-interpreter that functions exactly the same as the following meta-interpreter found in Simply Logical Chpater 3.8:
   
```
prove_p(true,[]):-!.
prove_p((A,B),[p((A,B),(A:-C))|Proof]):-!,
    clause(A,C),
    conj_append(C,B,D),
    prove_p(D,Proof).
prove_p(A,[p(A,(A:-B))|Proof]):-
    clause(A,B),
    prove_p(B,Proof).
```
   
   The only differences between the two is that ```prove_rb/4``` has an accumulator and checks the rules stored in the dynamic rulebase constructed at runtime rather than the static rules in the files. 
   
   By using the swipl graphical debugger, I confirmed that ```prove_rb/4``` is indeed the meta-interpreter that handles the inference processes in Prolexa after sentences/statements are converted into rules. Therefore, to achieve objectives 3, 6 and 9 I only need to add more clauses to ```prove_rb/4```.
   
# <a name="implementation">Adding Clauses to ```sentence1/3``` and ```prove_rb/4``` #
   Since it has been established in the previous section that Prolexa can already tokenize input sentences, Objective 1-9 can simply be reduced to the following:
   
   1. utterence list [peter, is, not, happy] can be converted to rule \[(not(happy(peter)):-true)\].
   2. utterence list [every, teacher, is, not, happy] can be converted to rule \[(not(happy(X)):-not(teacher(X)))\].
   3. given the rule base contains rules corresponding to "peter is not happy" and "every teacher is happy", Prolexa can infer utterence "peter is not a teacher".
   4. utterence list [peter, is, happy, and, mortal, and, red] can be converted to rule \[(happy(peter),mortal(peter),red(peter)):-true)\].
   5. utterence list [every, human, is, happy, and, mortal, and, red] can be converted to rule \[(happy(X),mortal(X),red(X)):-human(X))\].
   6.  given the rule base contains rules corresponding to "peter is human", "every human is happy", "every human is mortal", "every human is red", it can infer utterence "peter is happy and mortal and red".
   7. utterence list [peter, is, happy, or, mortal, or, red] can be converted to rule \[(happy(peter);mortal(peter);red(peter)):-true)\].
   8. utterence list [every, human, is, happy, or, mortal, or, red] can be converted to rule \[(happy(X);mortal(X);red(X)):-human(X))\].
   9. given the rule base contains rules corresponding to "peter is human", "every human is happy or mortal", "peter is not happy", it can infer utterence "peter is mortal".
   
## Objectives 1, 2 and 3: Negation <a name="negation">
   
   ### Objective 1
   
   To achieve objective 1, the following queries should be successful when running ```swipl prolexa_grammer.pl```:
   
```
1 ?- sentence1(A,[peter,is,not,happy],[]).                           
A = [(not(happy(peter)):-true)] .
2 ?- sentence1(A,[tweety,does,not,fly],[]).    
A = [(not(fly(tweety)):-true)] .
```
   
   This is achieved by adding the following clauses to ```prolexa_grammer.pl```:
   
```
sentence1([(not(L):-true)]) --> proper_noun(N,X),not_verb_phrase(N,X=>L).
not_verb_phrase(s,M) --> [is, not],property(s,M).
not_verb_phrase(p,M) --> [are, not],property(p,M).
not_verb_phrase(s,M) --> [does, not],iverb(p,M).
not_verb_phrase(p,M) --> [do, not],iverb(p,M).
```
   Notice that s and p stands for single and plural form of a verb or a noun, and X=\>L means L(X) (from Simply Logical Chapter 7.2 and 7.3).
   
   ### Objective 2
   
   To achieve objective 2, the following queries should be successful when running ```swipl prolexa_grammer.pl```:
   
```
5 ?- sentence1(A,[all, birds ,do,not,fly],[]). 
A = [(not(fly(_A)):-bird(_A))] .
6 ?- sentence1(A,[every, human, is, mortal],[]). 
A = [(not(mortal(_A)):-human(_A))] .
```
   
   To achieve this the following clause was added.
   
```
sentence1([(not(H):-B)]) --> determiner(N,X=>B,X=>H,[(H:-B)]),noun(N,X=>B),not_verb_phrase(N,X=>H).
```
   The 4th argument in ```determiner/6``` usually serves the purpose of defining the rule that the sentences/statements will be converted into (see Simply Logical Chapter 7.3). However, since this responsibility is handled by the first argument in ```sentence1/3``` already, the 4 argument in ```determiner/4``` is trivial. The ```noun/4``` constructs the fact in the body of the rule whilst the ```not_verb_phrase/4``` constructs the fact in the head of the rule. The facts in the head and body should share the same argument X, hence the 2nd arguments in ```noun/4``` and ```not_verb_phrase/4``` are X=\>H and X=\>B respectively.
   
   ### Objective 3
   
   To achieve Objective 3, I need the meta-interpreter to be able to infer fact (not(teacher(peter))) given that the rule base contains rule [(not(happy(peter)):-true )] and [(happy(X):-teacher(X))]. To generalize, I need the meta-interpreter to be able to handle the following: to prove (not(A)), it needs to find two rules, [(B:-A)] and [(not(B):-true)]. Therefore, I added the following lines to ```prolexa_engine.pl```:
   
```
prove_rb(not(A),Rulebase,P0,P):-
   find_clause((B:-A),Rule,Rulebase),
   prove_rb(not(B),Rulebase,[p(not(A),Rule)|P0],P).
```
	
   The 3rd argument is the accumulator. Therefore it needs to add p(not(A),Rule) to P0. This is also known as the Modus Tollens inference rule: if A then B. not B. therefore not A.

### Testing the Prolexa after more clauses have been added
	
To test whether Prolexa runs as intended after the clauses are added, the following commands were executed on the commandline (after running ```swipl prolexa.pl```) and expected output messages were returned:

```
1 ?- prolexa_cli.
prolexa> "forget everything".
*** utterance(forget everything)
*** goal(retractall(prolexa:stored_rule(_55846,_55848)))
*** answer(I am a blank slate)
I am a blank slate
prolexa> "every teacher is happy".
*** utterance(every teacher is happy)
*** rule([(happy(_56068):-teacher(_56068))])
*** answer(I will remember that every teacher is happy)
I will remember that every teacher is happy
prolexa> "peter is not happy".
*** utterance(peter is not happy)
*** rule([(not(happy(peter)):-true)])
*** answer(I will remember that peter is not happy)
I will remember that peter is not happy
prolexa> "explain why peter is not a teacher".
*** utterance(explain why peter is not a teacher)
*** goal(explain_question(not(teacher(peter)),_56800,_56526))
*** answer(peter is not happy; every teacher is happy; therefore peter is not a teacher)
peter is not happy; every teacher is happy; therefore peter is not a teacher
prolexa>
```
	
## Objectives 4, 5 and 6: Conjunction <a name="conjunction">
### Objective 4

To achieve objective 4, the following querying should be sucessful after running ```swipl prolexa_grammer.pl```:

```
1 ?- sentence1(A,[peter,is,happy,and,mortal,and,red],[]). 
A = [(happy(peter), mortal(peter), red(peter):-true)] .
```

This is achieved by adding the following clauses to ```prolexa_grammer.pl```:

```
sentence1([((H1,H2):-true)]) --> proper_noun(N,X),verb_phrase(N,X=>H1),property_and_list(N,(X=>H2)).
property_and_list(N,X=>M) --> [and],property(N,X=>M).
property_and_list(N,X=>(M1,M2)) --> [and],property(N,X=>M1), property_and_list(N,X=>M2).
```

Note that H1 corresponds to the first fact in the head, e.g, happy(peter). H2 corresponds to a conjunction list of facts, e.g., mortal(peter), red(peter).
	
Also, rule ```property_and_list(N,X=>M)``` uses recursion to parse a sentence that contains multiple conjunction such that the following query is successful:

```
7 ?- property_and_list(A,B,[and,happy,and,mortal,and,green,and,blue,and,red],[]).
B = _A=>(happy(_A), mortal(_A), green(_A), blue(_A), red(_A)) .
```
	
### Objective 5

To achieve objective 5, the following queries must be successful after running ```swipl prolexa_grammer.pl```:

```
7 ?- sentence1(A,[every,human,is,happy,and,mortal,and,red],[]).                   
A = [(happy(_A), mortal(_A), red(_A):-human(_A))] .
```

This is achieved by adding the following clause to to ```prolexa_grammer.pl```:
```
sentence1([((H1,H2):-B)]) --> determiner(N,X=>B,X=>H1,[(H1:-B)]),noun(N,X=>B),verb_phrase(N,X=>H1),property_and_list(N,(X=>H2)).
```

Like Objective 2, ```determiner/6``` is trivial here because the rules is constructed in ```sentence1/3```.

### Objective 6
	
To achieve objective 6, the meta-interpreter need to be be able to infer fact (happy(peter),mortal(peter),red(peter)) give that the rulebase contains rules [(happy(X):-human(X))], [(mortal(X):-human(X))], [(red(X):-human(X))] and [(human(peter):-true)]. More generally, this means the meta-interpreter need to be able to infer fact (P, Q, R) given rule [(P:-K)], [(Q:-K)], [(R:-K)] and [(K:-true)]. Fortunately, the original meta-interpreter in Prolexa can already handle this:
	
```
prove_rb(true,_Rulebase,P,P):-!.
prove_rb((A,B),Rulebase,P0,P):-!,
    find_clause((A:-C),Rule,Rulebase),
    conj_append(C,B,D),
    prove_rb(D,Rulebase,[p((A,B),Rule)|P0],P).
prove_rb(A,Rulebase,P0,P):-
    find_clause((A:-B),Rule,Rulebase),
    prove_rb(B,Rulebase,[p(A,Rule)|P0],P).
```

The 3rd clause of ```prove_rb/4``` states that to prove fact A, the meta-interpreter needs to find two rules [(A:-B)] and B. This is also known as the Modus Ponens inference rule, i.e., if B then A. B. Therefore A.
	
The 2nd clause of ```prove_rb/4``` states that to prove a conjunction list of facts (A,B), where A is the first fact/atom and B is the conjunction sublist of (A,B) without the first fact/atom, start with proving A first, then move on to the first fact/atom in B. This is also known as the conjunction inference rule, i.e., A. B. Therefore A and B.

The 1st clause of ```prove_rb/4``` is the exit condition of the recursive calls in the 2nd and 3rd clauses.
	
### Testing the Prolexa after more clauses have been added
	
To test whether Prolexa runs as intended after the clauses are added, the following commands were executed on the commandline (after running ```swipl prolexa.pl```) and expected output messages were returned:

```
1 ?- prolexa_cli.
prolexa> "forget everything".
*** utterance(forget everything)
*** goal(retractall(prolexa:stored_rule(_55846,_55848)))
*** answer(I am a blank slate)
I am a blank slate
prolexa> "peter is human".
*** utterance(peter is human)
*** rule([(human(peter):-true)])
*** answer(I will remember that peter is human)
I will remember that peter is human
prolexa> "every human is mortal".
*** utterance(every human is mortal)
*** rule([(mortal(_56324):-human(_56324))])
*** answer(I will remember that every human is mortal)
I will remember that every human is mortal
prolexa> "every human is happy".
*** utterance(every human is happy)
*** rule([(happy(_56702):-human(_56702))])
*** answer(I will remember that every human is happy)
I will remember that every human is happy
prolexa> "every human is red".
*** utterance(every human is red)
*** rule([(red(_57068):-human(_57068))])
*** answer(I will remember that every human is red)
I will remember that every human is red
prolexa> "explain why peter is mortal and happy and red".
*** utterance(explain why peter is mortal and happy and red)
*** goal(explain_question((mortal(peter),happy(peter),red(peter)),_57586,_57256))
*** answer(peter is human; every human is red; peter is human; every human is happy; peter is human; every human is mortal; therefore peter is mortal and happy and red)
peter is human; every human is red; peter is human; every human is happy; peter is human; every human is mortal; therefore peter is mortal and happy and red
prolexa>
```

## Objectives 7, 8 and 9: Disjunction <a name="disjunction"> 

### Objective 7
To achieve objective 7, the following query should be successful after running ```swipl prolexa_grammer.pl```:

```
1 ?- sentence1(A,[peter,is, happy,or,mortal,or,red,or,blue,or,green],[]). 
A = [(happy(peter);mortal(peter);red(peter);blue(peter);green(peter):-true)] .
```

This is achieved by adding the following clauses to ```prolexa_grammer.pl```:

```
sentence1([((H1;H2):-true)]) --> proper_noun(N,X),verb_phrase(N,X=>H1),property_or_list(N,(X=>H2)).
property_or_list(N,X=>M) --> [or],property(N,X=>M).
property_or_list(N,X=>(M1;M2)) --> [or],property(N,X=>M1), property_or_list(N,X=>M2).
```

Note that rule ```property_or_list/4``` is very similar to ```property_and_list``` described in Objective 4. The only difference is that ```property_or_list/4``` converts sentences with multiple disjunctions into a disjunction list (separated by semicolons) rather than a conjunction list (separated by commas). For comparison see below:
	
```
5 ?- property_and_list(A,B,[and,happy,and,mortal,and,green,and,blue,and,red],[]).  
B = _A=>(happy(_A), mortal(_A), green(_A), blue(_A), red(_A)) .

6 ?- property_or_list(A,B,[or,happy,or,mortal,or,green,or,blue,or,red],[]).       
B = _A=>(happy(_A);mortal(_A);green(_A);blue(_A);red(_A)) .
```
	
Objective 8:

To achieve objective 8, the following queries must be successful after running ```swipl prolexa_grammer.pl```:

```
7 ?- sentence1(A,[every,human,is, happy,or,mortal,or,red,or,blue,or,green],[]).    
A = [(happy(_A);mortal(_A);red(_A);blue(_A);green(_A):-human(_A))] .
```

This is achieved by adding the following clause to to ```prolexa_grammer.pl```:
```
sentence1([((H1;H2):-B)]) --> determiner(N,X=>B,X=>H1,[(H1:-B)]),noun(N,X=>B),verb_phrase(N,X=>H1),property_or_list(N,(X=>H2)).
```

Like objective 2 and 5, ```determiner/6``` is trivial here because the rules is constructed in ```sentence1/3```.

Objective 9:

To achieve objective 9, the meta-interpreter need to be be able to infer fact (mortal(peter)) give that the rulebase contains rules [(happy(X);mortal(X):-human(X))], [(not(happy(peter)):-true)], and [(human(peter):-true)]. More generally, this means the meta-interpreter need to be able to infer fact (A) given rule [(A;B:-C)] or [(B;A:-C)], [(C:-true)] and [(not(B):-true)]. Therefore, the following clauses are added to the meta-interpreter:
	
```
prove_rb(A,Rulebase,P0,P):-
    (find_clause((A;B:-C),Rule,Rulebase);
    find_clause((B;A:-C),Rule,Rulebase)),
    prove_rb((C,not(B)),Rulebase,[p(A,Rule)|P0],P).
```

where the last line attempts to find rule [(C:-true)] and [(not(B):-true)].

What I have added to the meta-interpreter is very similar to the definition of inference rule Disjunctive syllogism, i.e., A or B. not B. Therefore A. However, it cannot currently handle the more general case, that is, given a set of disjunctive facts $S$, given a subset $A \subset S$ such that all facts in $A$ are not true, therefore it can be infered that all facts in set $S - A$ are true. For example, "peter is human", "every human is happy or mortal or red or blue or green", "peter is not mortal or blue", therefore "peter is happy or red or green".

### Testing the Prolexa after more clauses have been added

To test whether Prolexa runs as intended after the clauses are added, the following commands were executed on the commandline (after running ```swipl prolexa.pl```) and expected output messages were returned:

```
1 ?- prolexa_cli.
prolexa> "forget everything".
*** utterance(forget everything)
*** goal(retractall(prolexa:stored_rule(_57286,_57288)))
*** answer(I am a blank slate)
I am a blank slate
prolexa> "peter is human". 
*** utterance(peter is human)
*** rule([(human(peter):-true)])
*** answer(I will remember that peter is human)
I will remember that peter is human
prolexa> "every human is mortal or happy".
*** utterance(every human is mortal or happy)
*** rule([(mortal(_57832);happy(_57832):-human(_57832))])
*** answer(I will remember that every human is mortal or happy)
I will remember that every human is mortal or happy
prolexa> "peter is not happy".
*** utterance(peter is not happy)
*** rule([(not(happy(peter)):-true)])
*** answer(I will remember that peter is not happy)
I will remember that peter is not happy
prolexa> "explain why peter is mortal".
*** utterance(explain why peter is mortal)
*** goal(explain_question(mortal(peter),_58580,_58370))
*** answer(peter is not happy; peter is human; every human is mortal or happy; therefore peter is mortal)
peter is not happy; peter is human; every human is mortal or happy; therefore peter is mortal
prolexa> "forget every human is mortal or happy".
*** utterance(forget every human is mortal or happy)
*** goal(retractall(prolexa:stored_rule(_59886,[(mortal(_59928);happy(_59928):-human(_59928))])))
*** answer(I erased it from my memory)
I erased it from my memory
prolexa> "tell me everything".
*** utterance(tell me everything)
*** goal(all_rules(_60156))
*** answer(peter is human. peter is not happy)
peter is human. peter is not happy
prolexa> "every human is happy or mortal" .
*** utterance(every human is happy or mortal)
*** rule([(happy(_60908);mortal(_60908):-human(_60908))])
*** answer(I will remember that every human is happy or mortal)
I will remember that every human is happy or mortal
prolexa> "explain why peter is mortal".
*** utterance(explain why peter is mortal)
*** goal(explain_question(mortal(peter),_61372,_61162))
*** answer(peter is not happy; peter is human; every human is happy or mortal; therefore peter is mortal)
peter is not happy; peter is human; every human is happy or mortal; therefore peter is mortal
prolexa>
```

# <a name="limitations">Conclusion #
In conclusion, Objectives 1-9 have all been successfully achieved.

However, there is one limitation with objective 9. What I have added to the meta-interpreter to complete objective 9 is very similar to the definition of inference rule Disjunctive syllogism, i.e., A or B. not B. Therefore A. However, it cannot currently handle the more general case, that is, given a set of disjunctive facts S, given a subset A of S such that all facts in A are not true, Prolexa cannot infer that all facts in set S - A are true. For example, given "peter is human", "every human is happy or mortal or red or blue or green", "peter is not mortal or blue", Prolexa cannot infer "peter is happy or red or green".