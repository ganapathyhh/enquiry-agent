% File:         db.pl
% Author:       Ganapathy Hariharan

% Quit responses
db_response(bye, [
                'Bye!', 
                'Hope to see you again.', 
                'Have a nice day!'
        ]).

% Greeting responses
db_response(greeting, [
                'Hello!', 
                'Hello, nice to meet you.', 
                'Hi there!',
                'Welcome!',
                'Hi.'
        ]).

% Ask user's name
db_response(ask_user_name, [
                'May I know your name?',
                'What\'s your name?'
        ]).

% Bot name print            
db_response(my_name, [
                'My name is Alexa.',
                'I\'m Alexa!',
                'This is Alexa, at your service.',
                'I\'m Alexa. I will be assisting you today.',
                'Mr. Alexa at your service.'
        ]).

% User thanked
db_response(thanked, [
                'You\'re welcome!',
                'Any time.',
                'Glad to be of service.',
                'No worries.',
                'No problem.'
        ]).

% Queries menu from bot
db_response(basic_menu, [
                '\n---- BASIC MENU ----\n',
                'Enter 1 to know about eligibility criteria\n',
                'Enter 2 to know application process/registration\n',
                'Enter 3 to know application status\n',
                'Enter 4 to know about admission/payment'
        ]).

% Application process queries
db_response(application_process_menu, [
                '\n---- REGISTRATION INFORMATION ----\n',
                'Enter 1 to know deadline for application submission\n',
                'Enter 2 to know exam date\n',
                'Enter 3 to know exam centers\n',
                'Enter 4 to get instructions\n',
                'Enter 5 to get syllabus\n',
                'Enter 6 to get types of questions information\n',
                'Enter 7 to return to previous menu'
        ]).

% Application Status queries
db_response(application_status_menu, [
                '\n---- APPLICATION INFORMATION ----\n',
                'Enter 1 to know your BITSAT score\n',
                'Enter 2 to know cut-offs\n',
                'Enter 3 to know the seat allocated for you\n',
                'Enter 4 to return to previous menu'
        ]).

% Payment details queries
db_response(payment_details_menu, [
                '\n---- PAYMENT DETAILS MENU ----\n',
                'Enter 1 to know fees structure\n',
                'Enter 2 to know payment details\n',
                'Enter 3 to return to previous menu'
        ]).

% Feedback questions
db_response(feedback, [
                'Okay. Did you find any of the talks useful?',
                'So, How is/was your day?'
        ]).

% Special instructions
db_response(special_instructions, [
                '1) Make the registration before deadline as no extension will be provided\n',
                '2) Locate nearby exam centers before-hand to book for exams\n',
                '3) Be in the exam hall 30-minutes before the exam commences\n',
                '4) Pay admission fees to confirm the seat before the dead line'
        ]).

% Syllabus menu
db_response(syllabus_menu, [
                '\n---- SYLLABUS MENU ----\n',
                'Enter 1 to know syllabus for Physics\n',
                'Enter 2 to know syllabus for Chemistry\n',
                'Enter 3 to know syllabus for Biology\n',
                'Enter 4 to know syllabus for Mathematics\n',
                'Enter 5 to return to previous menu'
        ]).

% Exam pattern
db_response(exam_pattern, [
                'MCQs (Multiple Choice Questions)\n',
                'Physics - 40 questions\n',
                'Chemistry - 40 questions\n',
                'English Proficiency - 15 questions\n',
                'Logical Reasoning - 10 questions\n',
                'Maths/Biology - 45 (Each)'
        ]).

% Fees structure
db_response(fees_structure, [
                'Admission Fees: 39800INR\n',
                'Semester/Term Fees\n',
                'First & Second Semester: 178000INR\n',
                'Hostel Fees\n',
                'First & Second Semester: 12900INR\n',
                'Mess & Electricity advance\n',
                'First & Second Semester: 10000INR\n',
                'Other Advances\n',
                'First & Second Semester: 12000INR'
        ]).

greeting_db([
        hello, 
        hi, 
        hey
        ]).

thanks_db([
        thanks,
        thankyou,
        thank,
        cheers
        ]).
