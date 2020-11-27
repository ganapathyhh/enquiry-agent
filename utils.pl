% File:         utils.pl
% Author:       Ganapathy Hariharan
% Reference:    chatbot-master

% Include 'random' library
:- use_module(library(random)).

/*******************************************************
    RANDOM PICK FROM LIST,
    NTH ITEM SELECT,
    EXACT MATCH,
    MATCH LIKE
*******************************************************/
% random_pick/2
%
% Picks a random response (Res) from a list (R).
random_pick(Res, R):- 
    length(Res, Length),  
    Upper is Length + 1,
    random(1, Upper, Rand),
    nth_item(Res, Rand, R).

% nth_item/3
%
% Holds true if the N-th item in the List is Item.
nth_item([H|_], 1, H).
nth_item([_|T], N, X):-
    nth_item(T, N1, X),
    N is N1 + 1.

% exact_match/3
%
% Checks to see if SubSet is an intersection of Set1 and Set2.
exact_match([], _, []).
exact_match([H|T1], L2, [H|T3]):- 
    member(H, L2), !,
    exact_match(T1, L2, T3).
exact_match([_|T1], L2, L3):-
    exact_match(T1, L2, L3).

% matches_like/2
%
% True if SubSet is indeed a subset of Set.
matches_like([], _).
matches_like([H|T], L2):- 
    member(H, L2),
    matches_like(T, L2).

% matches_positive/1
%
% Matches 'yes' or 'y'
matches_positive(AL):-
	AL == y; AL == 'Y'; AL == 'yes'; AL == 'Yes'.

% matches_negative/1
%
% Matches 'no' or 'n'
matches_negative(AL):-
	AL == n; AL == 'N'; AL == 'no'; AL == 'No'.

% matches_yes_no/1
%
% check whether input is yes or no only
matches_yes_no(AL):-
	matches_positive(AL), !;
	matches_negative(AL), !.

% list_to_string/2
%
% List to string
list_to_string(A, S):-
    atomic_list_concat(A, ' ', S).

% list_to_string/3
%
% List to string
list_to_string(A, D, S):-
    atomic_list_concat(A, D, S).

% display_and_reply/2
%
% Display bot message 
% and get reply from user
display_and_reply(D, R):-
    bot_conv(D),
    print_user_name(you),
    read_input(R).

/*******************************************************
    USER GREETING, 
    ASKING QUESTION,
    THANKING, 
    QUITTING
*******************************************************/
% is_user_quitting/1
%
% Checks if the given sentence S contains the word "bye".
is_user_quitting(S):- 
    matches_like([bye], S).

% is_user_greeting/1
% 
% True if the sentence matches any greetings in the database.
is_user_greeting(S):-
    greeting_db(D),
    exact_match(S, D, A),
    A \== [].

% is_user_thanking/1
%
% Checks if the given sentence S matches any "thanks" type
% sentences in the database.
is_user_thanking(S):-
        thanks_db(D),
        exact_match(S, D, A),
        A \== [].

% is_question/1
%
% Checks if the given sentence S matches any "question" type
% sentences in the database.
is_question(S):-
    member('?', S).

/*******************************************************
    GET: USERNAME, FEEDBACK
*******************************************************/
% get_feedback/1
%
% Asks the user for a number (N) of pieces of feedback, 
% and asserts the responses into the database.
get_feedback(0).
get_feedback(N):-
        db_response(feedback, D),
        nth_item(D, N, Ques),
        bot_conv(Ques),
        print_user_name(you),
        read_input(Reply),
        assert(feedback(Ques, Reply)),
        M is N - 1,
        get_feedback(M).

% get_usr_name/1
%
% Prompts the user to input a valid name, and asserts it.
get_usr_name(X):-
    db_response(ask_user_name, Queries),
    random_pick(Queries, RandomQuery),
    bot_conv(RandomQuery), 
    print_user_name(you),
    read_name(S),
	get_usr_name(X, S),
    get_usr(X).
get_usr_name(_, RL):-
    \+is_user_quitting([RL]),
    asserta(usr_name(RL)), !.
get_usr_name(_, RL):-
    is_user_quitting([RL]),
    gen_reply(_, RL, S),
    bot_conv(S),
    halt(0).
get_usr_name(Q, _):-
	get_usr_name(Q).

get_usr(R):-
    usr_name(R).

/*******************************************************
    PRINT REPORT AFTER CHAT ENDS
*******************************************************/
% print_report/0
%
% Outputs a conversation summary based on facts gathered 
% during chat.
print_report:-
        format('~n**** Conversation report ****~n'),
	    usr_name(X),
        format('User\'s name: ~w~n',[X]),
        retract(usr_name(X)),
        fail.
print_report:-
        applicant(X), 
        format('Applicant id: ~w~n', [X]), 
        retract(applicant(X)), 
        fail.
print_report:-
        information(X, Y), 
        format('~w : ~w~n', [X, Y]), 
        retract(information(X, Y)), 
        fail.
print_report:-
        feedback(X, Y), 
        format('~w : ~w~n', [X, Y]), 
        retract(feedback(X, Y)), 
        fail.
print_report.
