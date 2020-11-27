% File:         interactions.pl
% Author:       Ganapathy Hariharan

% gen_reply/3
%
% User quits using bye
gen_reply(_, S, R):- % User quits
        is_user_quitting([S]), !,
        db_response(bye, Res), 
	random_pick(Res, R).

/*******************************************************
        BASIC/MAIN MENU
*******************************************************/
gen_reply(basic_menu, '1', R):- % Eligibility menu
        (
                course_eligibility(X) -> append(['You are eligible to apply for the following courses:\n'], X, R), asserta(information('', R)); 
                atom_concat('', 'Sorry, You are not eligible to apply', R),
                asserta(information('', R))
        ).

gen_reply(basic_menu, '2', R):- % Application process menu
        go_to_menu(application_process_menu), !,
        get_reply(R).

gen_reply(basic_menu, '3', R):- % Application status menu
        go_to_menu(application_status_menu), !,
        get_reply(R).

gen_reply(basic_menu, '4', R):- % Payment details menu
        go_to_menu(payment_details_menu), !,
        get_reply(R).

/*******************************************************
        APPLICATION PROCESS MENU
*******************************************************/
gen_reply(application_process_menu, '1', R):- % Get deadline for applying
        deadline(X),
        make_reply('Deadline for applying is', X, R).

gen_reply(application_process_menu, '2', R):- % Get examdate
        examdate(X),
        make_reply('Examination date is', X, R).

gen_reply(application_process_menu, '3', R):- % Get exam centers if location is available and centers not available in the location
        get_usr_location(G), !,
        (
                \+examcenter(G) -> 
                atomic_concat('', 'You don\'t have an exam center near your location. Please check these exam centers\n', T),
                findall(F, examcenter(F), L),
                list_to_string(L, ',\n', H),
                atomic_concat(T, H, R),
                atomic_concat('Exam center for location ', G, E),
                asserta(information(E, R));
                make_reply('Exam centers are available in', G, R)
        ). 

gen_reply(application_process_menu, '4', R):- % Get special instructions
        db_response(special_instructions, X),
        list_to_string(X, Y),
        make_reply('Special instructions are', Y,  R).

gen_reply(application_process_menu, '5', R):- % Get syllabus
        go_to_menu(syllabus_menu), !,
        get_reply(R).

gen_reply(application_process_menu, '6', R):- % Types of questions asked in BITSAT
        db_response(exam_pattern, Y),
        list_to_string(Y, X),
        make_reply('Types of questions asked is', X, R).

gen_reply(application_process_menu, '7', R):- % Application process menu to Basic menu
        go_to_menu(basic_menu), !,
        get_reply(R).

/*******************************************************
        SYLLABUS MENU
*******************************************************/
gen_reply(syllabus_menu, '1', R):- % Get syllabus for Physics
        bagof(A, syllabus(physics, A), B),
        atomic_list_concat(B, ', ', X),
        make_reply('Syllabus for Physics', X, R).

gen_reply(syllabus_menu, '2', R):- % Get syllabus for Chemistry
        bagof(A, syllabus(chemistry, A), B),
        atomic_list_concat(B, ', ', X),
        make_reply('Syllabus for Chemistry', X, R).

gen_reply(syllabus_menu, '3', R):- % Get syllabus for Biology
        bagof(A, syllabus(biology, A), B),
        atomic_list_concat(B, ', ', X),
        make_reply('Syllabus for Biology', X, R).

gen_reply(syllabus_menu, '4', R):- % Get syllabus for Mathematics
        bagof(A, syllabus(maths, A), B),
        atomic_list_concat(B, ', ', X),
        make_reply('Syllabus for Maths', X, R).

gen_reply(syllabus_menu, '5', R):- % Go back to previous menu
        go_to_menu(application_process_menu), !,
        get_reply(R).

/*******************************************************
        APPLICATION STATUS MENU
*******************************************************/
gen_reply(application_status_menu, '1', R):- % Get BITSAT score for the applicant
        get_applicant_id(X), !,
        (
                bitsatscore(X, Y) -> make_reply('Bitsat score is', Y, R);     % BITSAT score
                atomic_concat('', 'Sorry! Your score is not available! Please contact helpline for more details', Y),
                make_reply('', Y, R)
        ).

gen_reply(application_status_menu, '2', R):- % Get cut-off details
        findall(X-Y:Z, bitscutoff2019(X, Y, Z), R),
        asserta(information('Cut-off details', R)). 

gen_reply(application_status_menu, '3', R):- % Get seat allocated
        get_applicant_id(X), !,
        (
                getseatallocated(X, Z) -> 
                atomic_concat('', Z, Y),
                make_reply('Seat allocated to you in', Y, R);
                atomic_concat('', 'Sorry! there is no seat allocated to you! Please contact helpline for more queries', Y),
                make_reply('', Y, R)
        ).

gen_reply(application_status_menu, '4', R):- % Application status menu to Basic menu
        go_to_menu(basic_menu), !,
        get_reply(R).

/*******************************************************
        PAYMENT DETAILS MENU
*******************************************************/

gen_reply(payment_details_menu, '1', R):- % Fees structure details
        db_response(fees_structure, Y),
        list_to_string(Y, X),
        make_reply('Fees structure is', X, R).

gen_reply(payment_details_menu, '2', R):- % Payment details
        get_applicant_id(X), !,
        (
                paid(X, Y), ! -> make_reply('So far you have paid the following amount towards bits', Y, R);
                make_reply('', 'You have not done any payment so far', R)
                
        ).

gen_reply(payment_details_menu, '3', R):- % Confirmations
        get_applicant_id(X), !,
        confirmation(X, F),
        (
                F >= 0 -> 
                getseatallocated(X, Y),
                make_reply('Your seat is confirmed for', Y, R);
                admfee(Z),
                paid(X, A),
                H is (Z - A),
                make_reply('You have to pay following amount to confirm the seat', H, R)
        ).
        
gen_reply(payment_details_menu, '4', R):- % Payment details menu to Basic menu
        go_to_menu(basic_menu), !,
        get_reply(R).

% gen_reply/3
%
% Generic reply
gen_reply(_, Y, R):- % Invalid option
        menu(X),
        db_response(X, Q),
        length(Q, L),
        atom_number(Y, N),
        (
                (N >= L ; N < 1) ->  invalid_reply(R),
                bot_conv_err(R), 
                fail
        ).

% get_reply/1
%
% Loading message for each menu
get_reply(R):-
        atomic_concat('', 'Loading', R), !.

% invalid_reply/1
%
% Invalid option message
invalid_reply(R):-
        atomic_concat('', 'Sorry. Invalid Option. Please try again!', R).

% make_reply/2
% 
% Make reply with message
make_reply(Msg, X, R):-
        atomic_concat('', Msg, Y),
        atomic_concat(Y, ': ', Z),
        atomic_concat(Z, X, R),
        asserta(information(Y, X)).

% get_applicant_id/1
%
% Get applicant id/number
get_applicant_id(X):-
        \+applicant(_), !,
        atomic_concat('', 'Enter your application id: ', Y),
        display_and_reply(Y, X),
        (
                bitsatscore(X, _) -> asserta(applicant(X));
                bot_conv_err('Incorrect applicant id. Please try again.'), fail
        ).
get_applicant_id(X):-
        applicant(X), !.

get_usr_location(G):- % Get exam centers getting location from user
        \+loc(_), !,
        display_and_reply('Enter your location ', G),
        asserta(loc(G)).
get_usr_location(G):-
        loc(G).
