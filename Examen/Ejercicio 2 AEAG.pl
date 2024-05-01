% Hechos
amistad(barbara, roberto).
amistad(susana, juan).
amistad(barbara, juan).
amistad(barbara, maria).
amistad(susana, pedro).

persona(barbara, mujer).
persona(susana, mujer).
persona(maria, mujer).
persona(roberto, hombre).
persona(juan, hombre).
persona(pedro, hombre).

% Reglas con nombres modificados
amigos_de_susana(Hombre) :-
    persona(Hombre, hombre),
    amistad(susana, Hombre).

mujeres_relacionadas_con_hombres_de_susana(Mujer) :-
    amistad(susana, Hombre),
    amistad(Mujer, Hombre),
    persona(Mujer, mujer),
    persona(Hombre, hombre),
    Mujer \= susana.

amigos_de_mujeres_relacionadas_con_hombres_de_susana(Amigo) :-
    amistad(Mujer, Amigo),
    mujeres_relacionadas_con_hombres_de_susana(Mujer).
