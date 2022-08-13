:- module(alexa_mod, [
  alexa/1,
  addDotsToFacts/2
]).

:- use_module(library(race/ape)).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_open)).

:- dynamic knowledge/1.

% ----- Prolog Webserver Part II -----

alexa(Request) :-
  http_read_json_dict(Request, DictIn),
  handle_dict(DictIn, DictOut),
  reply_json(DictOut).


% ----- Prolog Model -----

handle_dict(DictIn, DictOut) :-
  get_intent(DictIn, IntentName),
  intent_dictOut(IntentName, DictIn, DictOut).

get_intent(DictIn, IntentName) :-
  get_dict(intent, DictIn, IntentObject),
  get_dict(name, IntentObject, IntentName).

intent_dictOut("remember", DictIn, DictOut) :-
  get_dict(intent, DictIn, IntentObject),
  get_dict(value, IntentObject, ValueMaybeDot),
  maybe_dot(ValueMaybeDot, Value),
  atom_string(ValueAtom, Value),
  check_consistency(ValueAtom, Inconsistencies, Variant),
  ( Inconsistencies \= [] ->
    maplist(remove_fact, Inconsistencies, InconsistenciesFact),
    atomics_to_string(InconsistenciesFact, ' ', InconsistenciesFactReason),
    atom_concat('Inconsistent fact because: ', InconsistenciesFactReason, InconsistenciesFactOutput),
    my_json_answer(InconsistenciesFactOutput, DictOut)
  ; otherwise ->
    combine_sentences(Variant, CombinedSentences),
    !,
    check_consistency(CombinedSentences, InconsistenciesDatabase),
    ( InconsistenciesDatabase \= [] ->
      maplist(remove_fact, InconsistenciesDatabase, InconsistenciesDatabaseWithoutFact),
      atomics_to_string(InconsistenciesDatabaseWithoutFact,' ', InconsistenciesDatabaseReason),
      atom_concat('Inconsistent database because: ', InconsistenciesDatabaseReason, InconsistenciesDatabaseOutput),
      my_json_answer(InconsistenciesDatabaseOutput, DictOut)
    ; otherwise ->
      assert(knowledge(Variant)),
      atom_concat('This fact was saved in the database: ', Value, SuccessfulAnswer),
      my_json_answer(SuccessfulAnswer, DictOut)
    )
  ).

intent_dictOut("ask", DictIn, DictOut) :-
  get_dict(intent, DictIn, IntentObject),
  get_dict(value, IntentObject, ValueMaybeQuestionmark),
  maybe_questionmark(ValueMaybeQuestionmark,Value),
  get_whole_database(WholeDatabase),
  ask_with_answers(WholeDatabase, Value, Result),
  ( Result = results([ResultsString|_]) ->
    string_concat('Yes, the question could be answered with: ', ResultsString, AnswerQuery)
  ; otherwise ->
    AnswerQuery = 'The question could not be answered because the requested facts are not available in the database. Try again.'
  ),
  my_json_answer(AnswerQuery, DictOut).

intent_dictOut("prove", DictIn, DictOut) :-
  get_dict(intent, DictIn, IntentObject),
  get_dict(value, IntentObject, ValueMaybeDot),
  maybe_dot(ValueMaybeDot,Value),
  get_whole_database(WholeDatabase),
  prove_with_answers(WholeDatabase, Value, Result),
  ( Result = results([ResultsString|_]) ->
    string_concat('Yes, the theorem is right and could be proven with: ', ResultsString, AnswerProof)
  ; otherwise ->
    AnswerProof = 'The theorem could not be proven. Try again.'
  ),
  my_json_answer(AnswerProof, DictOut).

intent_dictOut(_,_,DictOut):-
my_json_answer('Error Occured : ',DictOut).

my_json_answer(Message, JSON) :-
  JSON = _{
    response: _{
      text: Message
    }
  }.

maybe_dot(MaybeDot, SureDot) :-
  ( atom_concat(_, '.', MaybeDot) ->
    MaybeDot = SureDot
  ; atom_concat(MaybeDot, '.', SureDot)
  ).

combine_sentences(Value, CombinedSentences) :-
  findall(Z, knowledge(Z), Sentences),
  flatten(Sentences, SentenceList),
  atomics_to_string(SentenceList, ' ', SentencesString),
  atom_concat(SentencesString, Value, CombinedSentences).

remove_fact(WithFact, WithoutFact) :-
  WithFact = fact(WithoutFact).

maybe_questionmark(MaybeWith, SureWith) :-
  ( string_concat(_, '?', MaybeWith) ->
    MaybeWith = SureWith
  ; string_concat(MaybeWith, '?', SureWith)
  ).

get_whole_database(WholeDatabase) :-
  findall(Z, knowledge(Z), Sentences),
  flatten(Sentences, SentenceList),
  atomics_to_string(SentenceList, ' ', WholeDatabase).

answersAll(X) :-
  findall(Z, knowledge(Z), Sentences),
  maplist(addDotsToFacts, Sentences, SentencesWithDot),
  flatten(SentencesWithDot, Xy),
  atomics_to_string(Xy, ' ', X).

addDotsToFacts(Sentence, SentenceWithDot):-
  append(Sentence, [.], SentenceWithDot).
