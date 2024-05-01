% Hechos

tiene_auto(juan).
tiene_auto(josé).
tiene_auto(jeremías).
tiene_auto(jorge).

vive_cerca(aída).
vive_cerca(analía).

vive_lejos(roberto).
vive_lejos(raúl).
vive_lejos(rodrigo).
vive_lejos(ana).

bebió_jugo(josé).
bebió_jugo(jeremías).

amigos(anacleta, juan).
amigos(anastasia, josé).
amigos(roberto, jeremías).
amigos(raúl, josé).
amigos(ana, josé).
amigos(analía, juan).

bebió_vino(juan).

se_dedicó_a_bailar(jorge).

% Reglas

llega_seguro(X) :-
    tiene_auto(X),
    \+ bebió_vino(X).

llega_seguro(X) :-
    vive_cerca(X).

llega_seguro(X) :-
    amigos(X, Y),
    tiene_auto(Y),
    llega_seguro(Y).

% Consultas

