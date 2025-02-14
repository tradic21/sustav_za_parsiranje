student('Ana', 'Horvat', 3).
student('Marko', 'Novak', 2).
student('Ivana', 'Kovač', 1).

prijava('Ana', 'Matematika', 5).
prijava('Marko', 'Fizika', 4).
prijava('Ivana', 'Informatika', 5).
prijava('Ana', 'Fizika', 3).


parsiraj(Input, ParsedQuery) :-
    atom_codes(Input, CharList),
    phrase(query(ParsedQuery), CharList),
    !.

ws --> [].
ws --> [32], ws.

query(Q) --> cartesian(Q).       
query(Q) --> union(Q).           
query(Q) --> intersection(Q).    
query(Q) --> difference(Q).      
query(Q) --> projection(Q).
query(Q) --> selection(Q).
query(Q) --> join(Q).
query(Q) --> paren_query(Q).
query(Q) --> base_query(Q).

cartesian(×(Q1, Q2)) -->
    "×(", query(Q1), ",", ws, query(Q2), ")".

projection(π(Atributi, Query)) --> 
    "π(", atributi(Atributi), ")", ws, query(Query).

selection(σ(Cond, Query)) -->
    "σ(", uvjet(Cond), ")", ws, query(Query).

join(⨝(Q1, Q2, Atribut)) -->
    "⨝(", query(Q1), ",", ws, query(Q2), ",", ws, atribut(Atribut), ")".

union(∪(Q1, Q2)) -->
    "∪(", query(Q1), ",", ws, query(Q2), ")".

intersection(∩(Q1, Q2)) -->
    "∩(", query(Q1), ",", ws, query(Q2), ")".

difference(- (Q1, Q2)) -->
    "-(", query(Q1), ",", ws, query(Q2), ")".

paren_query(Q) --> "(", query(Q), ")".

base_query(student) --> "STUDENT".
base_query(prijava) --> "PRIJAVA".

atributi([A|T]) --> 
    atribut(A), ("," , atributi(T) | { T = [] }).

atribut(ime) --> "ime".
atribut(prezime) --> "prezime".
atribut(godina) --> "godina".
atribut(predmet) --> "predmet".
atribut(ocjena) --> "ocjena".

uvjet(cond(Atr, Op, Lit)) -->
    atribut(Atr), ws, operator(Op), ws, literal(Lit).

operator('=<') --> "=<".
operator('>=') --> ">=".
operator('≠') --> "≠".
operator('=')  --> "=".
operator('<')  --> "<".
operator('>')  --> ">".

literal(Lit) -->
    "'", literal_chars(Codes), "'",
    { atom_codes(AtomLit, Codes),
      ( atom_number(AtomLit, Number) -> Lit = Number ; Lit = AtomLit )
    }.

literal_chars([C|Cs]) -->   
    [C], { C \= 39 }, literal_chars(Cs).
literal_chars([]) --> [].



evaluiraj(π(Atributi, Query), Rezultat) :-
    projekcija(Atributi, Query, Rezultat).

evaluiraj(σ(Cond, Query), Rezultat) :-
    selektiraj(Cond, Query, Rezultat).

evaluiraj(⨝(Q1, Q2, Atribut), Rezultat) :-
    spajanje(Q1, Q2, Atribut, Rezultat).

evaluiraj(∪(Q1, Q2), Rezultat) :-
    evaluiraj_rel(Q1, Rows1),
    evaluiraj_rel(Q2, Rows2),
    union_rows(Rows1, Rows2, Rezultat).

evaluiraj(∩(Q1, Q2), Rezultat) :-
    evaluiraj_rel(Q1, Rows1),
    evaluiraj_rel(Q2, Rows2),
    intersection_rows(Rows1, Rows2, Rezultat).

evaluiraj(- (Q1, Q2), Rezultat) :-
    evaluiraj_rel(Q1, Rows1),
    evaluiraj_rel(Q2, Rows2),
    difference_rows(Rows1, Rows2, Rezultat).

evaluiraj(×(Q1, Q2), Rezultat) :-
    evaluiraj_rel(Q1, Rows1),
    evaluiraj_rel(Q2, Rows2),
    cartesian_product(Rows1, Rows2, Rezultat).

evaluiraj(student, Rows) :-
    findall([A,B,C], student(A,B,C), Rows).

evaluiraj(prijava, Rows) :-
    findall([A,B,C], prijava(A,B,C), Rows).

evaluiraj_rel(Q, Rows) :-
    evaluiraj(Q, Rows).



projekcija(Atributi, Query, Rezultat) :-
    evaluiraj_rel(Query, Rows),
    findall(Projekcija, (
         member(Row, Rows),
         Row = [A,B,C],
         filtriraj(Atributi, A, B, C, Projekcija)
    ), Rezultat).

filtriraj([ime], Aime, _, _, [Aime]).
filtriraj([prezime], _, Bprezime, _, [Bprezime]).
filtriraj([godina], _, _, Cgodina, [Cgodina]).
filtriraj([ime, prezime], Aime, Bprezime, _, [Aime, Bprezime]).
filtriraj([ime, godina], Aime, _, Cgodina, [Aime, Cgodina]).
filtriraj([prezime, godina], _, Bprezime, Cgodina, [Bprezime, Cgodina]).
filtriraj([ime, prezime, godina], Aime, Bprezime, Cgodina, [Aime, Bprezime, Cgodina]).

filtriraj([predmet], _, Bpredmet, _, [Bpredmet]).
filtriraj([ocjena], _, _, Cocjena, [Cocjena]).
filtriraj([predmet, ocjena], _, Bpredmet, Cocjena, [Bpredmet, Cocjena]).
filtriraj([ime, predmet], Aime, Bpredmet, _, [Aime, Bpredmet]).
filtriraj([ime, ocjena], Aime, _, Cocjena, [Aime, Cocjena]).
filtriraj([ime, predmet, ocjena], Aime, Bpredmet, Cocjena, [Aime, Bpredmet, Cocjena]).




selektiraj(cond(Atr, Op, Lit), Query, Rezultat) :-
    evaluiraj_rel(Query, Rows),
    findall(Row, (
         member(Row, Rows),
         row_attribute_value(Row, Atr, Val),
         provjeri(Val, Op, Lit)
    ), Rezultat).




spajanje(Q1, Q2, Atribut, Rezultat) :-
    evaluiraj_rel(Q1, Rows1),
    evaluiraj_rel(Q2, Rows2),
    findall(R, (
         member(Row1, Rows1),
         member(Row2, Rows2),
         Row1 = [A1,B1,C1],
         Row2 = [A2,B2,C2],
         row_attribute_value([A1,B1,C1], Atribut, Val1),
         row_attribute_value([A2,B2,C2], Atribut, Val2),
         Val1 = Val2,
         R = [A1,B1,C1, B2,C2]
    ), Rezultat).



row_attribute_value(Row, Atr, Val) :-
    ( Atr = ime, Row = [Val, _, _] );
    ( Atr = prezime, Row = [_, Val, _] );
    ( Atr = godina, Row = [_, _, Val] );
    ( Atr = predmet, Row = [_, Val, _] );
    ( Atr = ocjena, Row = [_, _, Val] ).


provjeri(Val, '=', Lit)  :- Val = Lit.
provjeri(Val, '<', Lit)  :- Val < Lit.
provjeri(Val, '>', Lit)  :- Val > Lit.
provjeri(Val, '=<', Lit) :- Val =< Lit.
provjeri(Val, '>=', Lit) :- Val >= Lit.
provjeri(Val, '≠', Lit) :- Val \= Lit.




union_rows(Rows1, Rows2, Union) :-
    append(Rows1, Rows2, Combined),
    sort(Combined, Union).



intersection_rows(Rows1, Rows2, Intersection) :-
    findall(Row, (member(Row, Rows1), member(Row, Rows2)), Unsorted),
    sort(Unsorted, Intersection).



difference_rows(Rows1, Rows2, Difference) :-
    findall(Row, (member(Row, Rows1), \+ member(Row, Rows2)), Unsorted),
    sort(Unsorted, Difference).



cartesian_product([], _, []).
cartesian_product([R1|Rows1], Rows2, Product) :-
    findall(Combined, (member(R2, Rows2), append(R1, R2, Combined)), Prod1),
    cartesian_product(Rows1, Rows2, ProdRest),
    append(Prod1, ProdRest, Product).



izvrsi_upit(Input, Rezultat) :-
    parsiraj(Input, ParsedQuery),
    evaluiraj(ParsedQuery, Rezultat),
    ispisi_json(Rezultat).



ispisi_json(Rezultat) :-
    with_output_to(string(JsonString), write_term(Rezultat, [quoted(true), ignore_ops(true)])),
    format('~w~n', [JsonString]).
