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
% intrebari(integ(_, _, [(P, [(Int1, Dir1, Id1), (Int2, Dir2, Id2)])|Coada], _),
%           [(P, (Int1, Dir1, Id1)), (P, (Int2, Dir2, Id2))|TI]) :-
%     intrebari(integ(_, _, Coada, _), TI), !.
intrebari(integ(_, _, [_|Rest], _), TQ) :-
    intrebari(integ(_, _, Rest, _), TQ), !.

% id_intrebare/2
% id_intrebare(+Integ, ?Intrebare, ?Q_ID)
% Este adevărat dacă în integrama reprezentată ca integ(...), Intrebare
% este un text iar Q_ID este un identificator care corespund aceleași
% întrebări.
% id_intrebare(integ(_, _, [], _), _, _) :- fail.
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

fill_ans(_, _, [], Data, Data).
fill_ans(P, Dir, [H|T], FilledData, Data) :-
    map_dir(Dir, CDir),
    add(P, CDir, NewP),
    (member((NewP, H), Data) ->
        NewData = Data;
        append(Data, [(NewP, H)], NewData)),
    fill_ans(NewP, Dir, T, FilledData, NewData), !.

fill([], _, Data, Data).
fill(_, [], Data, Data).

fill([(P, (Q, Dir, _))|Rest],
    Sol, Data, Acc) :-
    (member((Q, Ans), Sol) ->
        atom_chars(Ans, LAns),
        fill_ans(P, Dir, LAns, NewAcc, Acc);
        NewAcc = Acc),
    fill(Rest, Sol, Data, NewAcc), !.


fill([(P, [(Q, Dir, _)|T])|Rest],
    Sol, Data, Acc) :-
    (member((Q, Ans), Sol) ->
        atom_chars(Ans, LAns),
        fill_ans(P, Dir, LAns, NewAcc, Acc);
        NewAcc = Acc),
    fill([(P, T)|Rest], Sol, Data, NewAcc), !.

fill([(_, _)|Rest], Sol, Data, Acc) :-
    fill(Rest, Sol, Data, Acc), !.

completare(integ(H, W, L, Voc),
           Sol,
           integ(H, W, Data, Voc)) :-
    fill(L, Sol, Data, L).


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
lungime_spatiu(_, _, _) :- false.

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
intersectie(_, _, _, _, _) :- false.

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
solutii_posibile(_, _) :- false.

% rezolvare/2
% rezolvare(+Integ, -Solutie)
% Rezolvare produce în Solutie soluția integramei Integ. Soluția este
% reprezentată ca o listă de perechi de literali, fiecare pereche
% conținând textul unei întrebări și cuvântul (ca literal) care este
% răspunsul la întrebare.
%
% BONUS: rezolvare nu oferă soluții duplicate - numărul de soluții ale 
% predicatului este chiar numărul de completări posibile ale integramei.
rezolvare(_, _) :- false.
