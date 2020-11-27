% File:         kb.pl
% Author:       Ganapathy Hariharan
% Reference:    Data were taken from various websites by searching google. Many copyrighted content can be present.
% License:      For Development/Educational purpose only

% B.E. Branch
bebranch('Chemical').
bebranch('Civil').
bebranch('Computer Science').
bebranch('Electrical & Electronics').
bebranch('Electronics & Instrumentation').
bebranch('Mechanical').
bebranch('Manufacturing').

% M.Sc. Branch
mscbranch('Biological Sciences').
mscbranch('Chemistry').
mscbranch('Economics').
mscbranch('Mathematics').
mscbranch('Physics').
mscbranch('General Studies').

% Deadline for application
deadline('21-01-2021').

% Exam date
examdate('20-05-2021').

% Bits applicant
bitsatscore('2019b3442', 300).
bitsatscore('2019b4442', 425).
bitsatscore('2019b3542', 320).
bitsatscore('2019b3422', 255).
bitsatscore('2019f8948', 267).
bitsatscore('2019b4782', 345).
bitsatscore('2019b3672', 125).
bitsatscore('2019b3892', 100).
bitsatscore('2019b2346', 250).
bitsatscore('2019b9822', 268).
bitsatscore('2019b0222', 378).
bitsatscore('2019f3948', 350).

% Bits cut off 2019
bitscutoff2019('B.E.', 'Chemical', 281).
bitscutoff2019('B.E.', 'Civil', 291).
bitscutoff2019('B.E.', 'Electrical & Electronics', 341).
bitscutoff2019('B.E.', 'Mechanical', 313).
bitscutoff2019('B.E.', 'Manufacturing', 255).
bitscutoff2019('B.E.', 'Computer Science', 383).
bitscutoff2019('B.E.', 'Electronics & Instrumentation', 328).
bitscutoff2019('M.Sc.', 'Biological Sciences', 250).
bitscutoff2019('M.Sc.', 'Chemistry', 256).
bitscutoff2019('M.Sc.', 'Economics', 295).
bitscutoff2019('M.Sc.', 'Mathematics', 276).
bitscutoff2019('M.Sc.', 'Physics', 274).
bitscutoff2019('M.Sc.', 'General Studies', 274).
bitscutoff2019('B.Pharma', '', 195).

% Exam center locations
examcenter(hyderabad).
examcenter(dubai).
examcenter(delhi).
examcenter(agra).
examcenter(kolkata).
examcenter(bangalore).
examcenter(lucknow).
examcenter(chennai).
examcenter(patna).

% Syllabus for physics
syllabus(physics, 'Units & Measurements').
syllabus(physics, 'Kinematics').
syllabus(physics, 'Newton\'s Law of Motion').
syllabus(physics, 'Impulse and Momentum').
syllabus(physics, 'Work and Energy').
syllabus(physics, 'Rotational Motion').
syllabus(physics, 'Gravitation').
syllabus(physics, 'Mechanics of Solids and Fluids').
syllabus(physics, 'Oscillations').
syllabus(physics, 'Waves').

% Syllabus for chemistry
syllabus(chemistry, 'States of Matter').
syllabus(chemistry, 'Atomic Structure').
syllabus(chemistry, 'Periodicity').
syllabus(chemistry, 'Thermodynamics').
syllabus(chemistry, 'Physical and Chemical Equilibria').
syllabus(chemistry, 'Electrochemistry').
syllabus(chemistry, 'Chemical Kinetics').
syllabus(chemistry, 'Hydrogen and s-block Elements').
syllabus(chemistry, 'p-d and f-block Elements').
syllabus(chemistry, 'Principles of Organic Chemistry and Hydrocarbons').
syllabus(chemistry, 'Stereochemistry').
syllabus(chemistry, 'Organic Compounds with Functional Groups Containing Oxygen and Nitrogen').
syllabus(chemistry, 'Biological, Industrial and Environmental Chemistry').
syllabus(chemistry, 'Theoretical Practices of Environmental Chemistry').

% Syllabus for biology
syllabus(biology, 'Diversity in Living World').
syllabus(biology, 'Cell: The Unit of Life - Structure and Function').
syllabus(biology, 'Genetics and Evolution').
syllabus(biology, 'Structure and Function - Plants').
syllabus(biology, 'Reproduction, Growth and Movement of Plants').
syllabus(biology, 'Reproduction and Development in Humans').
syllabus(biology, 'Ecology and Environment').
syllabus(biology, 'Biology and Human Welfare').
syllabus(biology, 'Biotechnology and its Applications').
syllabus(biology, 'Structure and Function - Animals').

% Syllabus for maths
syllabus(maths, 'Algebra').
syllabus(maths, 'Trigonometry').
syllabus(maths, 'Two-Dimensional Coordinate Geometry').
syllabus(maths, 'Three Dimensional Coordinate Geometry').
syllabus(maths, 'Differential Calculus').
syllabus(maths, 'Integral Calculus').
syllabus(maths, 'Ordinary Differential Equation').
syllabus(maths, 'Probability').
syllabus(maths, 'Vectors').
syllabus(maths, 'Statistics').
syllabus(maths, 'Linear Programming').
syllabus(maths, 'Mathematical Modeling').

% Seats allocated for the applicant
getseatallocated('2019b2346', 'B.E. Computer Science').
getseatallocated('2019f3948', 'B.E. Electrical & Electronics Engineering').
getseatallocated('2019f8948', 'B.E. Chemical').
getseatallocated('2019g8998', 'M.Sc. Chemistry').

% Paid amount by applicant
paid('2019g8998', 34000).
paid('2019t3748', 25000).
paid('2019b2346', 10000).
paid('2019f3948', 70000).

% Admission fees
admfee(39800).
