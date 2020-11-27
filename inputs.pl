% File:         chatbot.pl
% Author:       Ganapathy Hariharan
% Reference:    chatbot-master

:- dynamic flag_name/1.

/*******************************************************
    CONVERT INTO ASCII
*******************************************************/
% initread/2
initread([K1, K2|U]) :-
    get_code(K1),
    get_code(K2),
    readrest(K2, U).

% readrest/2
readrest(63, []):-!.  % 63 ascii for '?'
readrest(33, []):-!.  % 33 ascii for '!'
readrest(10, []):-!.  % 10 ascii for '\n'
readrest(K, [K1|U]) :- 
    K =< 32, !,
    get_code(K1),
    readrest(K1, U).
readrest(_K1, [K2|U]) :-
    get_code(K2),
    readrest(K2, U).

/*******************************************************
    LIST OF ASCII TO WORDS
*******************************************************/
/* This is a lexical parser which converts a sequence of chars to a sequence 
 * tokens
 * words (S, L, []) where L is a list of chars, and S is the result from
 * the parser, i.e. a list of tokens
 */
words([V|U]) --> word(V), !, blanks, words(U).
words([]) --> [].

word(U1) --> [K], {lc(K, K1)},!, alphanums(U2), {name(U1, [K1|U2])}.
word(N) --> [K], {digit(K)}, !, digits(U), {name(N, [K|U])}.
word(V) --> [K], {name(V, [K])}.

alphanums([K1|U]) --> [K], {alphanum(K, K1)}, !, alphanums(U).
alphanums([]) --> [].

% alphanum/2
alphanum(95, 95) :- !.
alphanum(K, K1) :- 
    lc(K, K1).
alphanum(K, K) :- 
    digit(K).

digits([K|U]) --> [K], {digit(K)}, !, digits(U).
digits([]) --> [].

blanks --> [K],{K =< 32}, !, blanks.
blanks --> [].

% digit/1
digit(K) :- 
    K > 47, 
    K < 58.

% lc/2
lc(K, K1) :- 
    \+flag_name(_), !,
    K > 64, 
    K < 91, !, 
    K1 is K + 32. 
lc(K, K) :- 
    K > 64, 
    K < 91, !. 
lc(K, K) :-
    K > 96,
    K < 123.
    
/*******************************************************
    READ INPUT: LOWERCASE STRING AND NAME STRING
*******************************************************/
% read_input/1
%
% Read input from user
read_input(S) :- 
    initread(L),
    words(A, L, []),
    atomic_list_concat(A, '', S).

read_name(S):-
    asserta(flag_name(S)),
    initread(L),
    words(A, L, []),
    atomic_list_concat(A, '', S),
    retract(flag_name(_)).
