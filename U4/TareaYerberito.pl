planta(ahuehuete).
planta(ajenjo).
planta(ajo).


nombre_cientifico(ahuehuete,'Taxodium mucronatum').
nombre_cientifico(ajenjo,'Artemisia absinthium').
nombre_cientifico(ajo,'Allium sativum').


% Ahuehuete
propiedad(ahuehuete,'gran contenido de silicato de sodio').
propiedad(ahuehuete,'arterosclerosis avanzada').
propiedad(ahuehuete,'várices').
propiedad(ahuehuete,hemorroides).
propiedad(ahuehuete,'mala circulación').

% Ajenjo
propiedad(ajenjo,cólicos).
propiedad(ajenjo,anorexia).
propiedad(ajenjo,'invasión de lombrices').
propiedad(ajenjo,disentería).
propiedad(ajenjo,'atonia intestinal').
propiedad(ajenjo,buen_estomacal).
propiedad(ajenjo,diurético).
propiedad(ajenjo,mal_aliento).


% Ajo
propiedad(ajo,solitaria).
propiedad(ajo,lombrices).
propiedad(ajo,reumas).
propiedad(ajo,vermífugo).
propiedad(ajo,febrífugo).
propiedad(ajo,diurético).
propiedad(ajo,expectorante).
propiedad(ajo,sarna).
propiedad(ajo,tiña).
propiedad(ajo,callos).

% Ahuehuete
planta_alivia(ahuehuete,'gran contenido de silicato de sodio').
planta_alivia(ahuehuete,'arterosclerosis avanzada').
planta_alivia(ahuehuete,'várices').
planta_alivia(ahuehuete,hemorroides).
planta_alivia(ahuehuete,'mala circulación').

% Ajenjo
planta_alivia(ajenjo,cólicos).
planta_alivia(ajenjo,anorexia).
planta_alivia(ajenjo,'invasión de lombrices').
planta_alivia(ajenjo,disentería).
planta_alivia(ajenjo,'atonia intestinal').
planta_alivia(ajenjo,buen_estomacal).
planta_alivia(ajenjo,diurético).
planta_alivia(ajenjo,mal_aliento).

% Ajo
planta_alivia(ajo,solitaria).
planta_alivia(ajo,lombrices).
planta_alivia(ajo,reumas).
planta_alivia(ajo,vermífugo).
planta_alivia(ajo,febrífugo).
planta_alivia(ajo,diurético).
planta_alivia(ajo,expectorante).
planta_alivia(ajo,sarna).
planta_alivia(ajo,tiña).
planta_alivia(ajo,callos).

%Ahuehuete
metodo_de_aplicacion(ahuehuete, 'Té').

%Ajenjo
metodo_de_aplicacion(ajenjo, 'Cocinar las hojas y tomar 3 veces al día').

%Ajo
metodo_de_aplicacion(ajo, 'Coctel de leche y ajo molido en ayunas').
metodo_de_aplicacion(ajo, 'Aplicar aplastado directamente en caso de sarna o tiña').
planta_y_metodo_para_malestar(Malestar, Planta, Metodo) :-
    malestar(Malestar),
    planta(Planta), 
    planta_alivia(Planta, Malestar),
    metodo_de_aplicacion(Planta, Metodo).
% Ejemplo: planta_y_metodo_para_malestar("hemorragia", Planta, Metodo).

planta_alivia_malestar(Planta, Malestar) :-
    planta_alivia(Planta, Malestar),
    malestar(Malestar).
% Ejemplo: planta_alivia_malestar("helenio", Malestar).

obtener_nombre_cientifico(NombreNormal, NombreCientifico) :-
    planta(NombreNormal),
    nombre_cientifico(NombreNormal, NombreCientifico).
% Ejemplo: obtener_nombre_cientifico("helenio", NombreCientifico).



