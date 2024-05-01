padre_persona(marcelo, raul).
padre_persona(marcelo, rita).
padre_persona(juan, marcelo).
padre_persona(juan, maria).
padre_persona(raul, sergio).
madre_persona(miriam, ricardo).
madre_persona(rita, victor).
madre_persona(rita, veronica).
linda(miriam).
linda(rita).


% Reglas

%pareja(X, Y).

abuelo_persona(Abuelo, Nieto) :-
    (padre_persona(Abuelo, Hijo); madre_persona(Abuelo, Hijo)),
    (padre_persona(Hijo, Nieto); madre_persona(Hijo, Nieto)).

hermano_persona(Hermana, Hermano) :-
    ((madre_persona(Madre1, Hermana), madre_persona(Madre2, Hermano), Madre1 = Madre2);
    (padre_persona(Padre1, Hermana), padre_persona(Padre2, Hermano)), Padre1 = Padre2),
    Hermana \= Hermano.

hijo_persona(Padre, Hijo ) :-
    (madre_persona(Padre, Hijo); padre_persona(Padre, Hijo)).

tio_persona(Tio, Sobrino) :-
    (padre_persona(Padre, Sobrino); madre_persona(Padre, Sobrino)),
    hermano_persona(Tio, Padre),
    Tio \= Padre.

casados_persona(X, Y) :-
    ((padre_persona(X, Hijo1), madre_persona(Y, Hijo2), (Hijo1 = Hijo2));
    (padre_persona(Y, Hijo1), madre_persona(X, Hijo2), (Hijo1 = Hijo2))),
    (X \= Y).

casado_linda(Casado) :-
    casados_persona(Casado, Pareja),
    linda(Pareja).

relacion_familiar_persona(X, Y, Tipo) :-
    (padre_persona(X, Y), Tipo = 'padre-hijo';
    madre_persona(X, Y), Tipo = 'madre-hijo';
    abuelo_persona(X, Y), Tipo = 'abuelo-nieto';
    hermano_persona(X, Y), Tipo = 'hermano-hermana';
    hijo_persona(X, Y), Tipo = 'hijo-padre';
    tio_persona(X, Y), Tipo = 'tio-sobrino',
    casados_persona(X, Y), Tipo = 'Matrimonio'),
    (linda(X); linda(Y)).
