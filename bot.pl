% File:         chatbot.pl
% Author:       Ganapathy Hariharan

:- [db, utils, inputs, interactions, kb].
:- dynamic applicant/1, usr_name/1, information/2, feedback/2, menu/1, yop/1, pp/1, grade/2, bio/1, loc/1.

% agent_start/0
%
% Starter text
agent_start:-
	ansi_format([bold, fg(green)],'Alexa is a BITS agent that will converse with you.~n', []), 
	ansi_format([bold, fg(green)],'Get information related to admission.~n', []), 
	ansi_format([bold, fg(green)],'To begin, <alexa.> in the prompt.~n', []), 
	ansi_format([bold, fg(green)],'To end your session simply type <bye>. Enjoy!~n', []).

% main/0
%
% Starts conversation with greeting.
main:-
	print_welcome,
	ask_user_name(X),
	greet_user(X),
	make_conversations.

% pick_menu/1
pick_menu(Y):-
	\+menu(_), !,
	Y = basic_menu,
	asserta(menu(Y)).
pick_menu(Y):-
	menu(Y), !.

% go_to_menu/1
go_to_menu(Y):-
	retract(menu(_)),
	asserta(menu(Y)).

% show_menu/1
show_menu(Y):-
	db_response(Y, Q),
	list_to_string(Q, R),
	bot_conv(R).

% print_welcome/0
%
% Prints a greeting message to the user.
print_welcome:-
    make_greeting(Greet),
    format('~w~n', [Greet]).

% make_greeting/0
%
% Make greeting (by bot)
make_greeting(Greet):-
	db_response(greeting, PickGreet),
	random_pick(PickGreet, RandomGreet),
	db_response(my_name, MyName), 
	random_pick(MyName, RandomName),
	atomic_concat(RandomGreet, RandomName, Greet).	% Greet the user

% ask_user_name/0
%
% Asks for user's name
ask_user_name(X):- 
	\+usr_name(X), !,
	get_usr_name(X).

% greet_user/1
%
% Greets user with user's name
greet_user(X):-
	atomic_concat('Welcome ', X, T),
	atomic_concat(T, '!', R),
	bot_conv(R).

% make_conversations/0
%
% Repeat conversation until the user enters 'bye' or 'quit'.
make_conversations:-
	repeat,
	pick_menu(Y),
	show_menu(Y),
	print_user_name(you),
	read_input(S),
	gen_reply(Y, S, R),
	bot_conv(R),
	is_user_quitting([S]), 
	feedback,
	print_report, !,
	clear.

% feedback/0
%
% Feedback from the user
feedback:- 
	bot_conv('Please provide your feedback.'),
	db_response(feedback, F),
	length(F, L),
	get_feedback(L).

% print_bot_name/0
%
% Print bot conversation first half
bot_conv(Obj):-
	my_icon(MyName), 
	format('~w: ~w~n', [MyName, Obj]),
	flush_output.

bot_conv_err(Obj):-
	my_icon(MyName), 
	ansi_format([bold,fg(red)],'~w: ~w~n', [MyName, Obj]),
	flush_output.

% Bot icon
my_icon('Agent').

% print_user_name/1
%
% Prints username
print_user_name(you):-
	\+usr_name(_), !,
	user_icon(UserName),
	format('~w: ', [UserName]),
	flush_output.
print_user_name(you):-
	usr_name(UserName),
	format('~w: ', [UserName]),
	flush_output.

% User icon
user_icon('User').

/*******************************************************
	ELIGIBILITY CALCULATION
*******************************************************/
% year_eligible/0
%
% Year of pass eligibility
year_eligible:-
    \+yop(_), !,
    atomic_concat('', 'Enter year of pass of 12th (4 digits - YYYY)', C),
    display_and_reply(C, S),
    atom_number(S, X),
    (
        (X >= 2000, X =< 2021 ) -> asserta(yop(X)), year_eligible, asserta(information(C, X));
        year_eligible
    ).
year_eligible:-
    yop(YOP),
    YOP >= 2019,
    YOP =< 2020.

% applicant_eligibility/0
%
% Applicant eligibility
applicant_eligibility:-
    pass_percent,
    year_eligible,
    (
        bio_check -> (pcb_eligibility, aggregate_eligible);
        (pcm_eligibility, aggregate_eligible)
    ).

course_eligibility(R):-
	applicant_eligibility,
	(
		bio_check -> findall([X-A], course_branch(X, A), R);
		findall(X-A, (course_branch(X, A), X \= 'B.Pharm.'), R)
	).

% pcm_eligibility/0
%
% Physics, Chemistry and Maths eligibility
pcm_eligibility:-
    \+bio(_), !,
    mark_check(physics),
    mark_check(chemistry),
    mark_check(maths).

% pcb_eligibility/0
%
% Physics, Chemistry and Biology eligibility
pcb_eligibility:-
    mark_check(physics),
    mark_check(chemistry),
    mark_check(biology).

% aggregate_eligible/0
%
% Aggregate eligible
aggregate_eligible:-
    \+bio(_), !,
    grade(maths, MathMark),
    grade(physics, PhyMark),
    grade(chemistry, CheMark),
    atom_number(MathMark, MMark),
    atom_number(PhyMark, PMark),
    atom_number(CheMark, CMark),
    Aggregate = (MMark + PMark + CMark) / 3,
    Aggregate >= 75.
aggregate_eligible:-
    grade(physics, PhyMark),
    grade(chemistry, CheMark),
    grade(biology, BioMark),
    atom_number(BioMark, BMark),
    atom_number(PhyMark, PMark),
    atom_number(CheMark, CMark),
    Aggregate = (BMark + PMark + CMark) / 3,
    Aggregate >= 75.

% pass_percent/0
%
% Check pass percentage meets eligible criteria
pass_percent:-
    \+pp(_), !,
    atomic_concat('', 'Enter your 12th pass percentage', C),
    display_and_reply(C, S),
    asserta(information(C, S)),
    asserta(pp(S)),
    pass_percent.
pass_percent:-
    pp(X),
    atom_number(X, PP),
    PP >= 35.

% mark_check/1
%
% Subject marks individual check
mark_check(Sub):-
    \+grade(Sub, _), !,
    atomic_concat('Enter your ', Sub, T),
    atomic_concat(T, ' marks percent (0 - 100)', Q),
    display_and_reply(Q, S),
    asserta(information(Q, S)),
    asserta(grade(Sub, S)),
    mark_check(Sub).
mark_check(Sub):-
    grade(Sub, M),
    atom_number(M, Mark),
    Mark >= 60.

% bio_check/0
%
% Biology main stream check
bio_check:-
    \+bio(_), !,
    atomic_concat('', 'Is your main stream biology? (y/n)', C),
    display_and_reply(C, S),
    (
        \+matches_yes_no(S) ->
		bot_conv_err('Incorrect input! Please try again!'),
        bio_check;
        matches_positive(S) -> asserta(bio(S)), asserta(information(C, S))
    ), !.
bio_check:-
    bio(_), !.

course_branch('B.E.', Y):-
    bebranch(Y).
course_branch('M.Sc.', Y):-
    mscbranch(Y).
course_branch('B.Pharm.', '').

% Admission confirmation
% 
% Paid fees more than admission fees 
% then it is confirmed
confirmation(X, Y):-
	getseatallocated(X, _),
    paid(X, Z),
    admfee(A), !,
    Y = Z - A.

/*******************************************************
	CLEAR EVERYTHING AT THE END
*******************************************************/
% clear/0
clear:-
	retract(usr_name(_)), 
	fail.
clear:-
	retract(information(_,_)), 
	fail.
clear:-
	retract(feedback(_,_)), 
	fail.
clear:-
	retract(application(_)),
	fail.
clear:-
	retract(loc(_)), 
	fail.
clear:-
	retract(grade(_, _)),
	retract(yop(_)),
	retract(pp(_)),
	retract(bio(_)),
	fail.
clear.

% call agent_start
:- agent_start.
