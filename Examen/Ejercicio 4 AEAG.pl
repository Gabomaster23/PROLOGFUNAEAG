% Definición de las rutas
ruta('alta cordoba', 'villa allende', 750).
ruta('nueva cordoba', 'alta cordoba', 3000).
ruta('nueva cordoba', 'centro historico', 200).
ruta('centro historico', 'villa allende', 4000).
ruta('nueva cordoba', 'villa serena', 5000).
ruta('villa serena', 'villa allende', 3000).
ruta('nueva cordoba', 'las palmas', 4000).
ruta('las palmas', 'villa allende', 3200).
ruta('nueva cordoba', 'los robles', 2000).
ruta('los robles', 'san fernando', 3000).
ruta('san fernando', 'villa allende', 1700).

% Reglas

ruta_mas_corta(Inicio, Destino, Ruta, Distancia) :-
    ruta_mas_corta(Inicio, Destino, [Inicio], Ruta, Distancia).

ruta_mas_corta(Destino, Destino, Ruta, Ruta, 0).

ruta_mas_corta(Inicio, Destino, Visitados, Ruta, Distancia) :-
    ruta(Inicio, Nodo, Distancia1),
    \+ member(Nodo, Visitados),
    append(Visitados, [Nodo], NuevaLista),
    ruta_mas_corta(Nodo, Destino, NuevaLista, Ruta, Distancia2),
    Distancia is Distancia1 + Distancia2.


ruta_mas_larga(Inicio, Destino, Ruta, Distancia) :-
    findall(Dist, ruta_mas_corta(Inicio, Destino, Ruta, Dist), Distancias),
    max_list(Distancias, Distancia).


:- initialization(main).

main :-
    ruta_mas_corta('nueva cordoba', 'villa allende', RutaCorta, DistanciaCorta),
    ruta_mas_larga('nueva cordoba', 'villa allende', RutaLarga, DistanciaLarga),
    write('La ruta más corta es: '), write(RutaCorta), write(' con una distancia de '), write(DistanciaCorta), write(' metros.'), nl,
    write('La ruta más larga es: '), write(RutaLarga), write(' con una distancia de '), write(DistanciaLarga), write(' metros.'), nl,
    halt.

