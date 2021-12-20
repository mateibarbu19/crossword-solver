:- ensure_loaded('checker.pl').

test_mode(detailed).

% Considerăm următoarele reprezentări:
%
% O integramă este reprezentată prin structura (compusul)
% integ(H, W, Lista, Vocab), unde:
% H este înălțimea integramei
% W este lățimea integramei
% Lista este o listă de tupluri (Poz, Valoare), unde
%   Poz este un tuplu (R, C) conținând rândul și coloana (0-based)
%   Valoare este una dintre:
%     x - dacă celula este neagră (nu poate fi completată cu litere)
%     o literă, dacă celula este completată cu o literă
%     o listă de întrebări, reprezentate ca tupluri (Text, Dir, ID), cu
%       Text - un literal, textul întrebării
%       Dir - una dintre valorile j sau d, indicând direcția întrebării
%       ID - un identificator numeric al întrebării
% Vocab este o listă de literali reprezentând cuvinte disponibile
% pentru a rezolva întrebarea.
%
% În ieșirea predicatului intrebări, o întrebare este reprezentată ca
% ((R, C), Text, Dir, ID), unde
% R este rândul căsuței cu întrebarea (0-based)
% C este coloana căsuței cu întrebarea (0-based)
% Text este textul întrebării (un literal)
% Dir este j sau d, reprezentând direcția în care trebuie să fie plasat
% răspunsul (jos sau dreapta)
% ID este un identificator numeric al întrebării.

% Puteți vizualiza integramele cu:
% integrama(0, W), print_integrama(W).
% integrama(1, W), print_integrama(W).
% integrama(2, W), print_integrama(W).
% integrama(3, W), print_integrama(W).
%
% Testați cu
% vmtest.
% Testați teste individuale (vedeți predicatul tt din checker.pl) cu
% vmtest(Test).
% de exemplu cu vmtest(intrebari).


% intrebari/2
% intrebari(integ(+H, +W, +Lista, +Vocab), -Lista_intrebari)
% Este adevărat atunci când Lista_intrebari este o lista de tupluri
% ((R, C), Text, Dir, ID), fiecare tuplu corespunzând unei întrebări din
% integramă (rândul, coloana, textul întrebării, direcția (j/d),
% identificatorul).
%
% BONUS: intrebari are o singură soluție (o singură listă) pentru o
% anumită integramă.
intrebari(integ(_, _, [], _), []).
intrebari(integ(_, _, [(P, (Q, Dir, Id))|Rest], _), [(P, (Q, Dir, Id))|TQ]) :-
    intrebari(integ(_, _, Rest, _), TQ), !.
intrebari(integ(_, _, [(P, [(Q, Dir, Id)|T])|Rest], _), [(P, (Q, Dir, Id))|TQ]) :-
    intrebari(integ(_, _, [(P, T)|Rest], _), TQ), !.
intrebari(integ(_, _, [_|Rest], _), TQ) :-
    intrebari(integ(_, _, Rest, _), TQ), !.

% id_intrebare/2
% id_intrebare(+Integ, ?Intrebare, ?Q_ID)
% Este adevărat dacă în integrama reprezentată ca integ(...), Intrebare
% este un text iar Q_ID este un identificator care corespund aceleași
% întrebări.
id_intrebare(integ(_, _, [(_, (Q, _, Q_ID))|_], _), Q, Q_ID).
id_intrebare(integ(_, _, [(_, [(Q, _, Q_ID)|_])|_], _), Q, Q_ID).
id_intrebare(integ(_, _, [(_, [(_, _, _)|T])|Rest], _), Q, Q_ID) :-
    id_intrebare(integ(_, _, [(_, T)|Rest], _), Q, Q_ID).
id_intrebare(integ(_, _, [_|Rest], _), Q, Q_ID) :-
    id_intrebare(integ(_, _, Rest, _), Q, Q_ID), !.

% completare/3
% completare(+Integ, +Sol, -Integrama)
% Predicatul produce Integrama, o structură de forma integ(...),
% pornind de la Integ, în care au fost completate celule conform cu
% soluția Sol.
% Soluția este reprezentată ca o listă de perechi (Întrebare, Răspuns),
% unde Întrebarea este textul unei întrebări, iar Răspuns este un cuvânt
% de completat; ambele sunt atomi (literali).
% De exemplu, o soluție parțială pentru integrama 0 poate fi:
% [('Din care plouă', 'NOR'), ('Al doilea număr', 'DOI')]
%
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), solutie(0, Sol), completare(W, Sol, W2),
%   print_integrama(W2).
map_dir(j, (1, 0)).
map_dir(d, (0, 1)).

add((A, B), (C, D), (X, Y)) :-
    X is A + C,
    Y is B + D.

sub((A, B), (C, D), (X, Y)) :-
    X is A - C,
    Y is B - D.

norm((A, B), N) :-
    abs(A, A1),
    abs(B, B1),
    N is A1 + B1.

fill_ans(_, _, [], Data, Data).
fill_ans((X, Y), Dir, [H|T], FilledData, Data) :-
    (Dir == d ->
        (A = X, B is Y + 1);
        (A is X + 1, B = Y)),
    NewP = (A, B),
    (member((NewP, L), Data) ->
        (L == H ->
            NewData = Data;
            fail);
        NewData = [(NewP, H)|Data]),
    fill_ans(NewP, Dir, T, FilledData, NewData).

fill([], Data, Data).
fill([(Q, Ans)|T], L, Data) :-
    find_question(L, Q, P, D, _),
    atom_chars(Ans, LAns),
    fill_ans(P, D, LAns, NewL, L),
    fill(T, NewL, Data).

completare(integ(H, W, L, Voc),
           Sol,
           integ(H, W, Data, Voc)) :-
    fill(Sol, L, Data).

% lungime_spatiu/3
% lungime_spatiu(integ(+H, +W, +Lista, +Vocab), +Intrebare, -Lungime)
% pentru Bonus:
% lungime_spatiu(integ(+H, +W, +Lista, +Vocab), ?Intrebare, ?Lungime)
%
% Returnează lungimea spațiului asociat întrebării date.
% Întrebarea este indicată prin textul ei. De exemplu:
% lungime_spatiu pentru integrama 0 și întrebarea 'Al doilea număr'
% trebuie să lege Lungime la 3.
%
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), id_intrebare(W, Text, 3), lungime_spatiu(W, Text, X).

map_value(x, x).
map_value((_, _, _), que).
map_value([_, _], que).
map_value(V, V).

map_square(L, (X, Y), Ch) :-
    (member(((X, Y), V), L) ->
        map_value(V, Ch), !;
        Ch = free).

find_question([(P, (Q, D, Id))|_], Q, P, D, Id).
% find_question([(_, (_, _, _))|Rest], Q, P, D, Id) :- 
%     !, find_question(Rest, Q, P, D, Id).
find_question([(P, [(Q, D, Id)|_])|_], Q, P, D, Id).
find_question([(P, [(_, _, _)|T])|Rest], Q, Pos, D, Id) :-
    !, find_question([(P, T)|Rest], Q, Pos, D, Id).
find_question([_|Rest], Q, P, D, Id) :-
    find_question(Rest, Q, P, D, Id).

find_length([], _, _, 0).
find_length(L, P, D, Len) :-
    map_dir(D, CD),
    add(P, CD, NewP),
    map_square(L, NewP, Ch),
    (Ch \= free ->
        Len = 0;
        find_length(L, NewP, D, NewLen),
        Len is 1 + NewLen).

lungime_spatiu(integ(_, _, L, _), Q, Len) :-
    find_question(L, Q, P, D, _),
    find_length(L, P, D, Len).


% intersectie/5
% intersectie(integ(+H, +W, +Lista, +Voc), +I1, -Poz1, +I2, -Poz2)
% pentru Bonus:
% intersectie(integ(+H, +W, +Lista, +Voc), ?I1, ?Poz1, ?I2, ?Poz2)
%
% Pentru o integramă și două întrebări date prin textul lor (I1 și I2),
% al căror răspunsuri se intersectează, întoarce în Poz1 indicele din
% răspunsul la I1 la care este intersecția, și în Poz2 indicele din
% răspunsul la I2 la care este intersecția. Indecșii incep de la 0.
%
% De exemplu, în integrama 0:
%  █       █       2↓      3↓      █
%  █       0↓,1→   -       -       █
%  4→      -       -       -       █
%  5→      -       -       -       █
%  █       █       █       █       █
%
%  Întrebările 'Primii 3 din artă' și 'Afirmativ' (3, respectiv 1) se
%  intersectează la pozițiile 0, respectiv 2 (va fi litera A, de la
%  ART, respectiv DA).

to_list(_, _, 0, []) :- !.
to_list(P, D, Len, L) :-
    Len > 0,
    map_dir(D, CD),
    add(P, CD, NewP),
    NewLen is Len - 1,
    to_list(NewP, D, NewLen, NewL),
    append([NewP], NewL, L).

common_member([H|_], L, H) :- member(H, L).
common_member([_|T], L, M) :- common_member(T, L, M).

intersectie(integ(_, _, L, _), Q1, Poz1, Q2, Poz2) :-
    find_question(L, Q1, P1, D1, _),
    find_question(L, Q2, P2, D2, _),
    dif(Q1, Q2),
    find_length(L, P1, D1, Len1),
    find_length(L, P2, D2, Len2),
    to_list(P1, D1, Len1, L1),
    to_list(P2, D2, Len2, L2),
    common_member(L1, L2, M),
    sub(M, P1, Dist1),
    sub(M, P2, Dist2),
    norm(Dist1, R1),
    norm(Dist2, R2),
    Poz1 is R1 - 1,
    Poz2 is R2 - 1.

% solutii_posibile/2
% solutii_posibile(integ(+H, +W, +Lista, +Vocabular), -Solutii)
% Formează o listă Solutii, conținând perechi de forma
% (Întrebare, Cuvinte), unde
% Întrebare este textul unei întrebări din integramă, iar Cuvinte este o
% listă de cuvinte sunt din Vocabular și au lungimea corectă pentru a fi
% răspuns la întrebare. Solutii conține câte o pereche pentru fiecare
% întrebare din integramă.
% Cuvintele sunt reprezentate ca liste de atomi, fiecare atom
% având lungime 1 (o singură literă).
% De exemplu, pentru integrama 0, Solutii conține 6 perechi, două dintre
% ele fiind:
% ('Afirmativ', [['D', 'A'], ['N', 'U']])
% ('Din care plouă',
% [['N','O','R'],['A','R','T'],['U','I','T'],['D','O','I']])

add_word([], Length, Ch_List, Acc, LWL) :-
    ((Length =:= 1000) ->
        LWL = Acc;
        append([(Length, [Ch_List])], Acc, LWL)).
add_word([(Length, Words)|Rest], Length, Ch_List, Acc, LWL) :-
    !,
    append([Ch_List], Words, NewWords),
    append([(Length, NewWords)], Acc, NewAcc),
    add_word(Rest, 1000, [], NewAcc, LWL).
add_word([(Len, Words)|Rest], Length, Ch_List, Acc, LWL) :-
    Len =\= Length,
    append([(Len, Words)], Acc, NewAcc),
    add_word(Rest, Length, Ch_List, NewAcc, LWL).

build_length_word_list([], []).
build_length_word_list([Word|Rest], LWL) :-
    build_length_word_list(Rest, LWL2),
    atom_chars(Word, Ch_List),
    atom_length(Word, Length),
    add_word(LWL2, Length, Ch_List, [], LWL).

riffle_sol([], _, []).
riffle_sol([(Len, Q)|T], LWL, Sol) :-
    riffle_sol(T, LWL, Sol2),
    member((Len, Words), LWL),
    append([(Q, Words)], Sol2, Sol).

solutii_posibile(integ(_, _, L, Voc), Sol) :-
    build_length_word_list(Voc, LWL),
    findall((Len, Q),
        (find_question(L, Q, P, D, _),
         find_length(L, P, D, Len)),
        LQL),
    sort(1, @=<, LQL, SLQL),
    sort(1, @=<, LWL, SLWL),
    riffle_sol(SLQL, SLWL, Sol).

% rezolvare/2
% rezolvare(+Integ, -Solutie)
% Rezolvare produce în Solutie soluția integramei Integ. Soluția este
% reprezentată ca o listă de perechi de literali, fiecare pereche
% conținând textul unei întrebări și cuvântul (ca literal) care este
% răspunsul la întrebare.
%
% BONUS: rezolvare nu oferă soluții duplicate - numărul de soluții ale 
% predicatului este chiar numărul de completări posibile ale integramei.
fill_ans2(_, _, _, _, [], Data, Data).
fill_ans2(He, W, (X, Y), Dir, [H|T], FilledData, Data) :-
    (Dir == d ->
        (A = X, B is Y + 1);
        (A is X + 1, B = Y)),
    NewP = (A, B),
    Pos is A * W + B,
    arg(Pos, Data, L),
    (L = H ->
        NewData = Data;
        !, fail),
    fill_ans2(He, W, NewP, Dir, T, FilledData, NewData).

solutii_posibile2(integ(_, _, L, Voc), Sol) :-
    build_length_word_list(Voc, LWL),
    findall((Len, (Q, P, D)),
        (find_question(L, Q, P, D, _),
         find_length(L, P, D, Len)),
        LQL),
    sort(1, @>=, LQL, SLQL),
    sort(1, @>=, LWL, SLWL),
    riffle_sol(SLQL, SLWL, Sol).

step_by_step_fill(_, _, _, [], _, Sol, Sol).
step_by_step_fill(H, W, V, [Head|T], Used, Acc, Sol) :-
    ((Q, P, D), Words) = Head,
    member(Word, Words),
    atom_chars(Atom, Word),
    \+ member(Atom, Used),
    fill_ans2(H, W, P, D, Word, Res, V),
    step_by_step_fill(H, W, Res, T, [Atom|Used], [(Q, Atom)|Acc], Sol).

rezolvare(integ(H, W, L, Voc), Sol) :-
    solutii_posibile2(integ(_, _, L, Voc), SolPos),
    !,
    Max is H * W,
    length(List, Max),
    V =.. [f|List],
    get_time(T1),
    step_by_step_fill(H, W, V, SolPos, [], [], Sol),
    get_time(T2),
    T3 is T2 - T1,
    writeln(T3).
