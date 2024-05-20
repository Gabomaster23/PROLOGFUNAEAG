:- use_module(library(pce)).

% Hechos de ejemplo para la base de conocimiento
planta(belladona).
planta(berro).
planta(boldo).
planta(barbasco).
planta(mangle).
planta(marrubio).
planta(manzanilla).
planta(ahuehuete).
planta(ajenjo).
planta(ajo).

nombre_cientifico(belladona, 'atropa belladona').
nombre_cientifico(berro, 'nasturtium off').
nombre_cientifico(boldo, 'pneumus boldo').
nombre_cientifico(barbasco, 'dioscorea mexicana').
nombre_cientifico(mangle, 'Rusophora mangle').
nombre_cientifico(manzanilla, 'Matricaria chamomilla').
nombre_cientifico(marrubio, 'Marrubium vulgare').
nombre_cientifico(ahuehuete,'Taxodium mucronatum').
nombre_cientifico(ajenjo,'Artemisia absinthium').
nombre_cientifico(ajo,'Allium sativum').

% Elementos de las plantas
elementos(barbasco, raiz).
elementos(barbasco, hojas).
elementos(belladona, veneno).
elementos(belladona, hojas).
elementos(berro, hojas).
elementos(berro, ramas).
elementos(boldo, hojas).
elementos(boldo, ramas).


% Oringen de plantas
origen(boldo, chile).
origen(manzanilla, europa).

% Propiedades de las plantas
propiedad(belladona, antiepileptica).
propiedad(berro, desinflama).
propiedad(berro, descongestiona).
propiedad(boldo, depurativo).
propiedad(boldo, digestivo).
propiedad(mangle, tonica).
propiedad(mangle, colagoga).
propiedad(mangle, antidisenterica).
propiedad(manzanilla, tonico).
propiedad(manzanilla, estimulante).
propiedad(manzanilla, emenagogo).
propiedad(manzanilla, carminativo).
propiedad(manzanilla, estomaquico).
propiedad(marrubio, colagogo).
propiedad(marrubio, digestiva).
propiedad(ahuehuete,'gran contenido de silicato de sodio').
propiedad(ahuehuete,'arterosclerosis avanzada').
propiedad(ahuehuete,'v�rices').
propiedad(ahuehuete,hemorroides).
propiedad(ahuehuete,'mala circulaci�n').
propiedad(ajenjo,c�licos).
propiedad(ajenjo,anorexia).
propiedad(ajenjo,'invasi�n de lombrices').
propiedad(ajenjo,disenter�a).
propiedad(ajenjo,'atonia intestinal').
propiedad(ajenjo,buen_estomacal).
propiedad(ajenjo,diur�tico).
propiedad(ajenjo,mal_aliento).
propiedad(ajo,solitaria).
propiedad(ajo,lombrices).
propiedad(ajo,reumas).
propiedad(ajo,verm�fugo).
propiedad(ajo,febr�fugo).
propiedad(ajo,diur�tico).
propiedad(ajo,expectorante).
propiedad(ajo,sarna).
propiedad(ajo,ti�a).
propiedad(ajo,callos).


% Plantas y sus efectos medicinales
planta_alivia(belladona, 'tos ferina').
planta_alivia(belladona, epilepsia).
planta_alivia(belladona, 'colicos hepaticos').
planta_alivia(belladona, gastralgia).

planta_alivia(barbasco, sabanones).
planta_alivia(barbasco, tina).
planta_alivia(barbasco, sarna).
planta_alivia(barbasco, empeines).
planta_alivia(barbasco, ciatia).
planta_alivia(barbasco, gota).
planta_alivia(barbasco, reumas).
planta_alivia(barbasco, artritis).

planta_alivia(berro, 'desinflama las anginas').
planta_alivia(berro, 'dolor estomacal').
planta_alivia(berro, escorbuto).
planta_alivia(berro, anemia).
planta_alivia(berro, sifilis).

planta_alivia(boldo, 'calculos biliares').

planta_alivia(mangle, estrenimiento).
planta_alivia(mangle, ictericia).
planta_alivia(mangle, 'litiasis colecistisis').
planta_alivia(mangle, angiocolitis).

planta_alivia(manzanilla, digestion).
planta_alivia(manzanilla, 'diarrea infantil').
planta_alivia(manzanilla, 'menstruación irregular').
planta_alivia(manzanilla, 'irritacion ocular').
planta_alivia(manzanilla, 'control de gases').
planta_alivia(manzanilla, 'aclarar cabello').

planta_alivia(marrubio, 'menstruaciones dificiles').
planta_alivia(marrubio, bronquitis).
planta_alivia(marrubio, 'problemas biliosos').
planta_alivia(marrubio, 'caída del cabello').


% Ahuehuete
planta_alivia(ahuehuete,'gran contenido de silicato de sodio').
planta_alivia(ahuehuete,'arterosclerosis avanzada').
planta_alivia(ahuehuete,'v�rices').
planta_alivia(ahuehuete,hemorroides).
planta_alivia(ahuehuete,'mala circulaci�n').

% Ajenjo
planta_alivia(ajenjo,c�licos).
planta_alivia(ajenjo,anorexia).
planta_alivia(ajenjo,'invasi�n de lombrices').
planta_alivia(ajenjo,disenter�a).
planta_alivia(ajenjo,'atonia intestinal').
planta_alivia(ajenjo,buen_estomacal).
planta_alivia(ajenjo,diur�tico).
planta_alivia(ajenjo,mal_aliento).

% Ajo
planta_alivia(ajo,solitaria).
planta_alivia(ajo,lombrices).
planta_alivia(ajo,reumas).
planta_alivia(ajo,verm�fugo).
planta_alivia(ajo,febr�fugo).
planta_alivia(ajo,diur�tico).
planta_alivia(ajo,expectorante).
planta_alivia(ajo,sarna).
planta_alivia(ajo,ti�a).
planta_alivia(ajo,callos).


% Formas de empleo
forma_empleo('tos ferina', 'consulte a su medico').
forma_empleo(epilepsia, 'consulte a su medico').
forma_empleo('colicos hepaticos', 'consulte a su medico').
forma_empleo(gastralgia, 'consulte a su medico').

forma_empleo('desinflama las anginas', 'se come crudo').
forma_empleo('dolor estomacal', 'se come crudo').
forma_empleo(escorbuto, 'se come crudo').
forma_empleo(anemia, 'se come crudo').
forma_empleo(sifilis, 'se come crudo').

forma_empleo('calculos biliares', 'Se utilizan las hojas').

forma_empleo(ciatica, 'cocida en te').
forma_empleo(gota, 'cocida en te').
forma_empleo(reumas, 'cocida en te').
forma_empleo(artritis, 'cocida en te').
forma_empleo(sabanones, 'molida').
forma_empleo(tina, 'molida').
forma_empleo(sarna, 'molida').
forma_empleo(empeines, 'molida').

forma_empleo(estrenimiento, 'Se debe remojar y cocer, se debe tomar media hora antes de comer').
forma_empleo(ictericia, 'Se debe remojar y cocer, se debe tomar media hora antes de comer').
forma_empleo('litiasis colecistisis', 'Se debe remojar y cocer, se debe tomar media hora antes de comer').
forma_empleo(angiocolitis, 'Se debe remojar y cocer, se debe tomar media hora antes de comer').

forma_empleo(digestion, 'Se prepara como te').
forma_empleo('diarrea infantil', 'Se prepara como te').
forma_empleo('menstruación irregular', 'Se prepara como te').
forma_empleo('irritación ocular', 'Se prepara como te').
forma_empleo('control de gases', 'Se prepara como te').
forma_empleo('aclarar cabello', 'Se prepara como te').

forma_empleo('menstruaciones dificiles', 'Se prepara como te').
forma_empleo(bronquitis, 'Se prepara como te').
forma_empleo('problemas biliosos', 'Se prepara como te').
forma_empleo('caída del cabello', 'Se prepara como te').


%Ahuehuete
forma_empleo(ahuehuete, 'T�').

%Ajenjo
forma_empleo(ajenjo, 'Cocinar las hojas y tomar 3 veces al d�a').

%Ajo
forma_empleo(ajo, 'Coctel de leche y ajo molido en ayunas').
forma_empleo(ajo, 'Aplicar aplastado directamente en caso de sarna o ti�a').

% Medicamento que genera
medicamento(barbasco, cortisona).
medicamento(barbasco, progesterona).

% Botiquin
planta_botiquin(anis).
planta_botiquin(estrella).
planta_botiquin(menta).
planta_botiquin(arnica).
planta_botiquin(savila).
planta_botiquin(tila).
planta_botiquin(eucalipto).
planta_botiquin(yerbabuena).
planta_botiquin(manzanilla).
planta_botiquin('cola de caballo').
planta_botiquin(romero).
planta_botiquin(toronjil).
planta_botiquin(sanguinaria).
planta_botiquin(linaza).
planta_botiquin(hamamelis).
planta_botiquin(zarzaparrilla).
planta_botiquin(boloo).
planta_botiquin('diente de leon').
planta_botiquin(azahar).
planta_botiquin(malva).
planta_botiquin(marrubio).
planta_botiquin(rosal).
planta_botiquin(ajo).
planta_botiquin(ajenjo).
planta_botiquin(ahuehuete).


% waos
significado(analgesico, 'Sustancia que alivia o elimina el dolor.').
significado(antiinflamatorio, 'Sustancia que reduce la inflamacion.').
significado(antiseptico, 'Sustancia que previene la infeccion eliminando o inhibiendo el crecimiento de microorganismos.').
significado(diuretico, 'Sustancia que aumenta la produccion de orina.').
significado(expectorante, 'Sustancia que facilita la expulsion de mucosidades de las vias respiratorias.').
significado(laxante, 'Sustancia que facilita o acelera la evacuacion intestinal.').
significado(sedante, 'Sustancia que calma o tranquiliza.').
significado(estimulante, 'Sustancia que aumenta la actividad fisica o mental.').
significado(antioxidante, 'Sustancia que previene o retrasa el danio celular oxidativo.').
significado(inmunomodulador, 'Sustancia que ayuda a regular el sistema inmunologico.').



% �Cuales son plantas o plantas medicinales?
plantas_medicinales(Planta) :-
    planta(Planta).

% �Qu� elementos se encuentran en las plantas?
elementos_planta(Planta, Propiedad) :-
    propiedad(Planta, Propiedad).

% �Qu� elementos tiene una planta en especifica? Como por ejemplo la manzanilla
elementos_planta_especifica(Planta) :-
    planta(Planta),
    propiedad(Planta, Propiedad),
    write(Planta), write(' tiene la propiedad '), write(Propiedad), nl,
    fail.

% �Qu� plantas producen medicamentos?
plantas_medicinales_prod_med(Planta) :-
    planta_alivia(Planta, _).

% �Qu� medicamentos producen una planta en especifico?
medicamentos_planta_especifica(Planta, Medicamento) :-
    planta_alivia(Planta, Medicamento).

% �Qu� medicamentos provienen de plantas?
medicamentos_plantas(Medicamento) :-
    planta_alivia(_, Medicamento).

% Definir el predicado acciones_medicamentos_plantas/2 basado en las reglas proporcionadas
acciones_medicamentos_plantas(Planta, Medicamento, Accion) :-
    planta_alivia(Planta, Enfermedad),
    forma_empleo(Enfermedad, Accion),
    medicamento(Planta, Medicamento).

% �Cuales son los efectos o acciones de un medicamento en especifico?
acciones_medicamento_especifico(Medicamento) :-
    forma_empleo(Medicamento, Accion),
    write('El medicamento '), write(Medicamento), write(' se emplea para '), write(Accion), nl,
    fail.

% �Cuales son las acciones o efectos que tienen las plantas?
acciones_plantas(Planta, Accion) :-
    planta_alivia(Planta, _),
    forma_empleo(Planta, Accion).

% Significado de palabras que son acciones o efectos de plantas sobre organismo
significado_accion(Accion, Significado) :-
    forma_empleo(_, Accion),
    % Definir el significado de cada acci�n seg�n sea necesario
    (Accion = 'antiepileptica' -> Significado = 'Reduce la actividad el�ctrica anormal en el cerebro' ;
    Accion = 'desinflama' -> Significado = 'Reduce la inflamaci�n en el cuerpo' ;
    Accion = 'sedante' -> Significado = 'Induce la calma y la relajaci�n' ;
    Accion = 'antioxidante' -> Significado = 'Ayuda a proteger contra el da�o oxidativo en las c�lulas' ;
    % Agrega m�s reglas seg�n sea necesario
    true).

% Listado De plantas y sus acciones o efectos sobre el organismo
% Listado De plantas y sus acciones o efectos sobre el organismo
listado_plantas_acciones :-
    forall((planta_alivia(Planta, Medicamento),
            forma_empleo(Medicamento, Accion)),
           (write('Planta: '), write(Planta), write(', Accion: '), write(Accion), nl)).


% �Acciones o efectos de una planta en especifico?
:- discontiguous consultar_acciones_planta_especifica/0.
% Definir las acciones o efectos de una planta en espec�fico.
acciones_planta_especifica(Planta) :-
    planta_alivia(Planta, Medicamento),
    forma_empleo(Medicamento, Accion),
    format('La planta ~w se emplea para ~w.~n', [Planta, Accion]),
    fail.
acciones_planta_especifica(_).


% �Qu� plantas son analg�sicas?
plantas_analgesicas(Planta) :-
    planta_alivia(Planta, 'analgesico').

% �Cuales son las enfermedades que curan las plantas?
enfermedades_curan_plantas(Enfermedad) :-
    planta_alivia(_, Enfermedad).

% �Cuales son las enfermedades que que cura una planta en espec�fico? Zabila
enfermedades_curadas_planta_especifica(Planta) :-
    planta_alivia(Planta, Enfermedad),
    write(Planta), write(' cura '), write(Enfermedad), nl,
    fail.

% �Cuales son las plantas que curan una enfermedad como por ejemplo el herpes?
plantas_curan_enfermedad(Enfermedad, Planta) :-
    planta_alivia(Planta, Enfermedad).

% �Cuales son las formas de preparaci�n para tratamiento de enfermedades con uso de plantas?
% formas_preparacion_tratamiento(Enfermedad, Empleo) :-
%   planta_alivia(Planta, Enfermedad),
%    forma_empleo(Enfermedad, Empleo),
%    write('La enfermedad '), write(Enfermedad), write(' se cura con la planta '), write(Planta), write(' la cual se prepara: '), write(Empleo),
%    fail.

 formas_preparacion_tratamiento(Planta, Empleo) :-
     planta_alivia(Planta, Enfermedad),
     forma_empleo(Enfermedad, Empleo),
     format('La enfermedad ~w se cura con la planta ~w la cual se prepara: ~w~n', [Enfermedad, Planta, Empleo]).

% �Cuales son los modos de preparaci�n de una planta en especifico?
modos_preparacion_planta_especifica(Planta) :-
    planta_alivia(Planta, Enfermedad),
    forma_empleo(Enfermedad, Empleo),
    write('La planta '), write(Planta), write(' se emplea de la siguiente manera para tratar '), write(Enfermedad), write(': '), write(Empleo), nl,
    fail.

% Listar plantas medicinales y su nombre cient�fico
% Definir el predicado plantas_nombre_cientifico/2 basado en las reglas proporcionadas
plantas_nombre_cientifico(Planta, NombreCientifico) :-
    planta(Planta),
    nombre_cientifico(Planta, NombreCientifico).

% �Cual es el tratamiento y su preparaci�n para alguna enfermedad?
tratamiento_preparacion_enfermedad(Enfermedad, Planta, Empleo) :-
    planta_alivia(Planta, Enfermedad),
    forma_empleo(Enfermedad, Empleo).

% �Cuales son los or�genes de las plantas medicinales?
origenes_plantas(Planta, Origen) :-
    planta(Planta),
    % Define los or�genes de las plantas seg�n tu conocimiento
    (Planta = belladona -> Origen = 'Europa y Asia' ;
    Planta = berro -> Origen = 'Europa, Asia y Africa' ;
    Planta = boldo -> Origen = 'Sudamerica' ;
    % Agrega mas reglas segun sea necesario
    true).

% �Cual es el origen de una planta?
origen_planta(Planta, Origen) :-
    planta(Planta),
    origenes_plantas(Planta, Origen).

% �Cual es el tratamiento para una enfermedad (ya sea con plantas o medicamentos)?
tratamiento_enfermedad(Enfermedad, Planta, Medicamento, FormaEmpleo) :-
    planta_alivia(Planta, Enfermedad),
    forma_empleo(Enfermedad, Medicamento),
    forma_empleo(Medicamento, FormaEmpleo).

% Botiqu�n de plantas.
botiquin_plantas(Botiquin) :-
    findall(Planta, planta(Planta), Botiquin).


% Liz consultas
% modos de preparación para enfermedad

mostrar_tratamientos(Enfermedad) :-
    new(ResultadoDialogo, dialog('Resultados del Tratamiento')),
    (   planta_alivia(Planta, Enfermedad),
        forma_empleo(Enfermedad, Accion),
        format(atom(Texto), 'Planta: ~w, Enfermedad: ~w, Forma de empleo: ~w', [Planta, Enfermedad, Accion]),
        send(ResultadoDialogo, append, text(Texto)),
        fail
    ;   true
    ),
    send(ResultadoDialogo, append, button('Cerrar', message(ResultadoDialogo, destroy))),
    send(ResultadoDialogo, open).

consultar_forma_preparacion_tratamiento :-
    new(Dialogo, dialog('Consulta de Tratamiento')),
    send(Dialogo, append, text('Ingrese la enfermedad:')),
    send(Dialogo, append, new(EnfermedadInput, text_item(enfermedad, ''))),
    send(Dialogo, append, button('Consultar', message(@prolog, mostrar_tratamientos, EnfermedadInput?selection))),
    send(Dialogo, open).

% Modos de preparacion de la planta

consultar_modos_preparacion_planta :-
    new(Dialogo, dialog('Consulta de Modos de Preparación')),
    send(Dialogo, append, text('Ingrese el nombre de la planta:')),
    send(Dialogo, append, new(PlantaInput, text_item(planta, ''))),
    send(Dialogo, append, button('Consultar', message(@prolog, mostrar_modos_preparacion, PlantaInput?selection))),
    send(Dialogo, open).

% Predicado para mostrar los modos de preparación
mostrar_modos_preparacion(Planta) :-
    new(ResultadoDialogo, dialog('Modos de Preparación')),
    findall(Forma, (planta_alivia(Planta, Enfermedad), forma_empleo(Enfermedad, Forma)), FormasLista),
    list_to_set(FormasLista, FormasUnicas),  % Elimina duplicados
    mostrar_formas(ResultadoDialogo, FormasUnicas),
    send(ResultadoDialogo, append, button('Cerrar', message(ResultadoDialogo, destroy))),
    send(ResultadoDialogo, open).

% Predicado para mostrar cada forma de empleo
mostrar_formas(_, []).
mostrar_formas(Dialogo, [Forma | Resto]) :-
    format(atom(TextoForma), 'Forma de empleo: ~w', [Forma]),
    send(Dialogo, append, text(TextoForma)),
    mostrar_formas(Dialogo, Resto).


% Mostrar origen de todas las plantas medicinales
mostrar_origen_todas :-
    new(ResultadoDialogo, dialog('Origen de Plantas Medicinales')),
    forall(
        (planta(Planta), origen(Planta, Origen)),
        (
            format(atom(Texto), 'Planta: ~w, Origen: ~w', [Planta, Origen]),
            send(ResultadoDialogo, append, text(Texto))
        )
    ),
    send(ResultadoDialogo, append, button('Cerrar', message(ResultadoDialogo, destroy))),
    send(ResultadoDialogo, open).

% Mostrar origen de una planta

consultar_origen_planta :-
    new(Dialogo, dialog('Consulta de Origen de Planta')),
    send(Dialogo, append, text('Ingrese el nombre de la planta:')),
    send(Dialogo, append, new(PlantaInput, text_item(planta, ''))),
    send(Dialogo, append, button('Consultar', message(@prolog, mostrar_origen, PlantaInput?selection))),
    send(Dialogo, open).

% Mostrar el origen de la planta en una nueva ventana
mostrar_origen(Planta) :-
    new(ResultadoDialogo, dialog('Origen de la Planta')),
    (   origen(Planta, Origen) ->
        format(atom(Texto), 'Planta: ~w, Origen: ~w', [Planta, Origen]),
        send(ResultadoDialogo, append, text(Texto))
    ;   format(atom(NoDatosTexto), 'No tenemos datos sobre la planta: ~w', [Planta]),
        send(ResultadoDialogo, append, text(NoDatosTexto))
    ),
    send(ResultadoDialogo, append, button('Cerrar', message(ResultadoDialogo, destroy))),
    send(ResultadoDialogo, open).


% Mostrar botiquin

mostrar_plantas_botiquin :-
    new(ResultadoDialogo, dialog('Plantas del Botiquín')),
    forall(
        planta_botiquin(Planta),
        send(ResultadoDialogo, append, text(Planta))
    ),
    send(ResultadoDialogo, append, button('Cerrar', message(ResultadoDialogo, destroy))),
    send(ResultadoDialogo, open).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Consultas
consultar_plantas_medicinales :-
    new(Dialogo, dialog('Plantas Medicinales')),
    send(Dialogo, append, text('Plantas Medicinales:')), send(Dialogo, append, text('')),
    forall(plantas_medicinales(Planta), send(Dialogo, append, text(Planta))),
    send(Dialogo, open).

consultar_elementos_planta :-
    new(Dialogo, dialog('Elementos en las Plantas')),
    send(Dialogo, append, text('Elementos en las Plantas:')),
    send(Dialogo, append, text('')),
    forall(elementos(Planta, Elemento),
           (format(atom(Texto), '~w: ~w', [Planta, Elemento]), send(Dialogo, append, text(Texto)))),
    send(Dialogo, open).

consultar_elementos_planta_especifica :-
    new(Dialogo, dialog('Elementos en Planta Específica')),
    send(Dialogo, append, text('Ingrese el nombre de la planta:')),
    send(Dialogo, append, new(PlantaInput, text_item(planta, ''))),
    send(Dialogo, append, button('Consultar', message(@prolog, obtener_elementos_planta_especifica, PlantaInput?selection))),
    send(Dialogo, open).

obtener_elementos_planta_especifica(Planta) :-
    (   elementos(Planta, _) ->  % Verificar si la planta tiene elementos registrados
        new(DialogoResultado, dialog('Elementos en Planta Específica')),
        send(DialogoResultado, append, text('Elementos en ')),
        send(DialogoResultado, append, text(Planta)),
        send(DialogoResultado, append, text(':')),
        send(DialogoResultado, append, text('')),
        forall(elementos(Planta, Elemento),
               send(DialogoResultado, append, text(Elemento))),
        send(DialogoResultado, open)
    ;   % Si la planta no tiene elementos registrados, mostrar un mensaje
        new(DialogoResultado, dialog('Planta no existente')),
        send(DialogoResultado, append, text('La planta ')),
        send(DialogoResultado, append, text(Planta)),
        send(DialogoResultado, append, text(' no tiene elementos registrados.')),
        send(DialogoResultado, open)
    ).

plantas_que_producen_medicamentos(Plantas) :-
    setof(Planta, Medicamento^(medicamento(Planta, Medicamento)), Plantas).

consultar_plantas_medicinales_prod_med :-
    plantas_que_producen_medicamentos(Plantas),
    mostrar_plantas_en_ventana(Plantas).

mostrar_plantas_en_ventana(Plantas) :-
    new(Dialogo, dialog('Plantas que Producen Medicamentos')),
    forall(member(Planta, Plantas),
           send(Dialogo, append, text(Planta))),
    send(Dialogo, open).


consultar_medicamentos_planta_especifica :-
    new(Dialogo, dialog('Medicamentos de Planta Específica')),
    send(Dialogo, append, text('Ingrese el nombre de la planta:')),
    send(Dialogo, append, new(PlantaInput, text_item(planta, ''))),
    send(Dialogo, append, button('Consultar', message(@prolog, mostrar_resultado, PlantaInput?selection))),
    send(Dialogo, open).

mostrar_resultado(Planta) :-
    new(Ventana, dialog('Resultado')),
    send(Ventana, append, label(nombre_planta, 'Planta:')),
    send(Ventana, append, text(Planta, right)),
    findall(Medicamento, medicamento(Planta, Medicamento), Medicamentos),
    (   Medicamentos = [] ->
        send(Ventana, append, label(mensaje, 'Esta planta no tiene medicamentos'), right)
    ;   send(Ventana, append, label(mensaje, 'Medicamentos:'), right),
        forall(member(M, Medicamentos), send(Ventana, append, text(M), right))
    ),
    send(Ventana, open).




consultar_medicamentos_plantas :-
    new(Dialogo, dialog('Medicamentos de Plantas')),
    send(Dialogo, append, text('Medicamentos provenientes de plantas:')),
    send(Dialogo, append, text('')), % Espacio en blanco
    forall(medicamento(_, Medicamento),
           (   send(Dialogo, append, text(Medicamento)),
               send(Dialogo, append, text(' ')) % Espacio en blanco para simular un salto de línea
           )),
    send(Dialogo, open).


consultar_acciones_medicamentos_plantas :-
    new(Dialogo, dialog('Acciones de Medicamentos de Plantas')),
    send(Dialogo, append, text('Acciones de Medicamentos de Plantas:')),
    send(Dialogo, append, text('')),

    % Usar findall para recopilar todas las acciones en una lista
    findall((Medicamento, Planta, Enfermedad, Accion),
            (medicamento(Planta, Medicamento),  % Obtener la planta asociada con el medicamento
             planta_alivia(Planta, Enfermedad),  % Obtener las enfermedades aliviadas por la planta
             forma_empleo(Enfermedad, Accion)),
            Acciones),

    % Verificar si hay resultados antes de intentar mostrarlos
    (   Acciones \= [] ->
        % Si hay resultados, mostrarlos en la ventana
        forall(member((Medicamento, Planta, Enfermedad, Accion), Acciones),
               (format(atom(Texto), 'Planta: ~w, Medicamento: ~w, Enfermedad: ~w, Forma de Empleo: ~w', [Planta, Medicamento, Enfermedad, Accion]),
                send(Dialogo, append, text(Texto)), nl))
    ;   % Si no hay resultados, mostrar un mensaje indicando que no se encontraron acciones
        send(Dialogo, append, text('No se encontraron acciones para los medicamentos de plantas.'))
    ),

    % Finalmente, abrir la ventana de diálogo
    send(Dialogo, open).


consultar_acciones_plantas :-
    new(Dialogo, dialog('Acciones de Plantas')),
    send(Dialogo, append, text('Acciones de Plantas:')),
    send(Dialogo, append, text('')),

    % Obtener todas las plantas y acciones en una lista
    findall((Planta, Accion), planta_alivia(Planta, Accion), PlantasAcciones),

    % Mostrar cada planta y acción en el diálogo
    forall(member((Planta, Accion), PlantasAcciones),
           (
               format(atom(Texto), 'Planta: ~w, Accion: ~w', [Planta, Accion]),
               send(Dialogo, append, text(Texto)),
               nl
           )),
    send(Dialogo, open).

% consultar significado
consultar_significado_palabra :-
    new(Dialogo, dialog('Significado de Palabras Relacionadas con Plantas')),
    send(Dialogo, append, text('Ingrese la palabra:')),
    send(Dialogo, append, new(Palabra, text_item(palabra, ''))),
    send(Dialogo, append, button('Consultar', message(@prolog, obtener_significado_palabra, Palabra?selection))),
    send(Dialogo, open).

obtener_significado_palabra(Palabra) :-
    new(ResultadoDialogo, dialog('Resultados')),
    send(ResultadoDialogo, append, text('Significado de la palabra:')),
    (   significado(Palabra, Significado)
    ->  format(atom(Texto), '~w: ~w', [Palabra, Significado]),
        send(ResultadoDialogo, append, text(Texto))
    ;   send(ResultadoDialogo, append, text('No se encontro el significado de esta palabra'))
    ),
    send(ResultadoDialogo, open).


% Ejemplo de llamada en la interfaz gr�fica
consultar_listado_plantas_acciones :-
    new(Dialogo, dialog('Listado de Plantas y Acciones')),
    send(Dialogo, append, text('Listado de Plantas y Acciones:')),
    send(Dialogo, append, text('')),
    forall((planta_alivia(Planta, Medicamento),
            forma_empleo(Medicamento, Accion)),
           (format(atom(Texto), 'Planta: ~w, Accion: ~w', [Planta, Accion]),
            send(Dialogo, append, text(Texto)), nl)),
    send(Dialogo, open).



consultar_acciones_planta_especifica :-
    new(Dialogo, dialog('Acciones de Planta Específica')),
    send(Dialogo, append, text('Ingrese el nombre de la planta:')),
    send(Dialogo, append, new(PlantaInput, text_item(planta, ''))),
    send(Dialogo, append, button('Consultar', message(@prolog, mostrar_acciones, PlantaInput?selection))),
    send(Dialogo, open).

mostrar_acciones(Planta) :-
    new(Ventana, dialog('Acciones de Planta Específica')),
    send(Ventana, append, label(nombre_planta, 'Planta:')),
    send(Ventana, append, text(Planta, right)),
    findall(Accion, planta_alivia(Planta, Accion), Acciones),
    (   Acciones = [] ->
        send(Ventana, append, label(mensaje, 'Esta planta no tiene acciones'), right)
    ;   send(Ventana, append, label(mensaje, 'Acciones:'), right),
        forall(member(A, Acciones), send(Ventana, append, text(A), right))
    ),
    send(Ventana, open).




% Consulta para mostrar las acciones de una planta espec�fica en una interfaz gr�fica.
% Consulta para mostrar las acciones de una planta espec�fica en una interfaz gr�fica.
consultar_acciones_planta_especifica :-
    new(Dialogo, dialog('Acciones de una Planta')),
    send(Dialogo, append, text('Ingrese el nombre de la planta:')),
    send(Dialogo, append, new(Planta, text_item(planta, ''))),
    send(Dialogo, append, button('Consultar', message(@prolog, obtener_acciones_planta_especifica, Planta?selection))),
    send(Dialogo, open).

obtener_acciones_planta_especifica(Planta) :-
    new(Resultado, dialog('Resultados de Acciones de Planta')),
    send(Resultado, append, text('Acciones de la planta:')),
    (   acciones_planta_especifica(Planta)
    ->  true
    ;   send(Resultado, append, text('No se encontraron acciones para la planta especificada.'))
    ),
    send(Resultado, open).


% Consulta para mostrar las plantas analg�sicas en una interfaz gr�fica.
consultar_plantas_analgesicas :-
    new(Dialogo, dialog('Plantas Analgesicas')),
    send(Dialogo, append, text('Listado de Plantas Analgesicas:')),
    send(Dialogo, append, text('')),
    forall(plantas_analgesicas(Planta),
           (format(atom(Texto), 'Planta: ~w', [Planta]),
            send(Dialogo, append, text(Texto)), nl)),
    send(Dialogo, open).

% Consulta para listar plantas medicinales y su nombre cient�fico
consultar_plantas_nombre_cientifico :-
    new(Dialogo, dialog('Plantas y su Nombre Cientifico')),
    send(Dialogo, append, text('Plantas y su Nombre Cientifico:')),
    send(Dialogo, append, text('')),
    forall(plantas_nombre_cientifico(Planta, NombreCientifico),
           (format(atom(Texto), 'Planta: ~w, Nombre Cientifico: ~w', [Planta, NombreCientifico]),
            send(Dialogo, append, text(Texto)), nl)),
    send(Dialogo, open).

%�Cuales son las enfermedades que curan las plantas?

consultar_enfermedades_curan_plantas :-
    new(Dialogo, dialog('Enfermedades que curan las Plantas')),
    send(Dialogo, append, text('Enfermedades que curan las Plantas:')),
    send(Dialogo, append, text('')),
    forall(planta_alivia(Planta, Enfermedad),
           (format(atom(Texto), 'Planta: ~w, Enfermedad: ~w', [Planta, Enfermedad]),
            send(Dialogo, append, text(Texto)), nl)),
    send(Dialogo, open).


%�Cuales son las enfermedades que que cura una planta en espec�fico? (Por ejemplo, la sabila)
% Predicado para consultar enfermedades curadas por una planta espec�fica
consultar_enfermedades_curadas_planta_especifica :-
    new(Dialogo, dialog('Enfermedades Curadas por Planta Especifica')),
    send(Dialogo, append, text('Ingrese el nombre de la planta:')),
    send(Dialogo, append, new(Planta, text_item(planta, ''))),
    send(Dialogo, append, button('Consultar', message(@prolog, obtener_enfermedades_curadas_planta_especifica, Planta?selection))),
    send(Dialogo, open).

% Predicado para obtener y mostrar las enfermedades curadas por una planta espec�fica
obtener_enfermedades_curadas_planta_especifica(Planta) :-
    new(ResultadoDialogo, dialog('Resultados')),
    send(ResultadoDialogo, append, text('Enfermedades curadas por la planta:')),
    (   setof(Enfermedad, planta_alivia(Planta, Enfermedad), Enfermedades)
    ->  forall(member(Enfermedad, Enfermedades),
               (format(atom(Texto), '~w', [Enfermedad]),
                send(ResultadoDialogo, append, text(Texto)), nl))
    ;   send(ResultadoDialogo, append, text('No se encontraron enfermedades para esta planta'))
    ),
    send(ResultadoDialogo, open).



% Predicado para consultar plantas que curan una enfermedad espec�fica
consultar_plantas_curan_enfermedad :-
    new(Dialogo, dialog('Plantas que Curan una Enfermedad Especifica')),
    send(Dialogo, append, text('Ingrese el nombre de la enfermedad:')),
    send(Dialogo, append, new(Enfermedad, text_item(enfermedad, ''))),
    send(Dialogo, append, button('Consultar', message(@prolog, obtener_plantas_que_curan_enfermedad, Enfermedad?selection))),
    send(Dialogo, open).

% Predicado para obtener y mostrar las plantas que curan una enfermedad espec�fica
obtener_plantas_que_curan_enfermedad(Enfermedad) :-
    new(ResultadoDialogo, dialog('Resultados')),
    send(ResultadoDialogo, append, text('Plantas que curan la enfermedad:')),
    (   setof(Planta, planta_alivia(Planta, Enfermedad), Plantas)
    ->  forall(member(Planta, Plantas),
               (format(atom(Texto), '~w', [Planta]),
                send(ResultadoDialogo, append, text(Texto)), nl))
    ;   send(ResultadoDialogo, append, text('No se encontraron plantas para esta enfermedad'))
    ),
    send(ResultadoDialogo, open).


consultar_formas_preparacion_tratamiento :-
    new(Dialogo, dialog('Formas de Preparacion para Tratamiento')),
    send(Dialogo, append, text('Ingrese el nombre de la planta:')), send(Dialogo, append, new(Planta, text_item(planta, ''))),
    send(Dialogo, append, button('Consultar', message(@prolog, obtener_formas_preparacion_tratamiento, Planta))),
    send(Dialogo, open).

obtener_formas_preparacion_tratamiento(Planta) :-
    new(Dialogo, dialog('Formas de Preparacion para Tratamiento')),
    send(Dialogo, append, text('Formas de Preparacion para Tratar con '), send(Dialogo, append, text(Planta)), send(Dialogo, append, text(':'))),
    forall(formas_preparacion_tratamiento(Planta, Empleo), send(Dialogo, append, text(Empleo))),
    send(Dialogo, open).

consultar_modos_preparacion_planta_especifica :-
    new(Dialogo, dialog('Modos de Preparacion de Planta Especifica')),
    send(Dialogo, append, text('Ingrese el nombre de la planta:')), send(Dialogo, append, new(Planta, text_item(planta, ''))),
    send(Dialogo, append, button('Consultar', message(@prolog, obtener_modos_preparacion_planta_especifica, Planta))),
    send(Dialogo, open).

obtener_modos_preparacion_planta_especifica(Planta) :-
    new(Dialogo, dialog('Modos de Preparacion de Planta Especifica')),
    send(Dialogo, append, text('Modos de Preparacion de '), send(Dialogo, append, text(Planta)), send(Dialogo, append, text(':'))),
    forall(modos_preparacion_planta_especifica(Planta), send(Dialogo, append, text(Planta))),
    send(Dialogo, open).

consultar_tratamiento_preparacion_enfermedad :-
    new(Dialogo, dialog('Tratamiento y Preparacion para Enfermedad')),
    send(Dialogo, append, text('Ingrese el nombre de la enfermedad:')), send(Dialogo, append, new(Enfermedad, text_item(enfermedad, ''))),
    send(Dialogo, append, button('Consultar', message(@prolog, obtener_tratamiento_preparacion_enfermedad, Enfermedad))),
    send(Dialogo, open).

obtener_tratamiento_preparacion_enfermedad(Enfermedad) :-
    new(Dialogo, dialog('Tratamiento y Preparacion para Enfermedad')),
    send(Dialogo, append, text('Tratamiento y Preparacion para '), send(Dialogo, append, text(Enfermedad)), send(Dialogo, append, text(':'))),
    forall(tratamiento_preparacion_enfermedad(Enfermedad, Planta, Medicamento, FormaEmpleo), send(Dialogo, append, text('Planta: '), send(Dialogo, append, text(Planta)), send(Dialogo, append, text(', Medicamento: ')), send(Dialogo, append, text(Medicamento)), send(Dialogo, append, text(', Forma de Empleo: ')), send(Dialogo, append, text(FormaEmpleo)))),
    send(Dialogo, open).

consultar_origenes_plantas :-
    new(Dialogo, dialog('Origenes de Plantas Medicinales')),
    send(Dialogo, append, text('Origenes de Plantas Medicinales:')), send(Dialogo, append, text('')),
    forall(origen_planta(Planta, Origen), send(Dialogo, append, text(Planta - Origen))),
    send(Dialogo, open).

consultar_botiquin_plantas :-
    new(Dialogo, dialog('Botiquin de Plantas')),
    send(Dialogo, append, text('Botiquin de Plantas:')), send(Dialogo, append, text('')),
    forall(botiquin_plantas(Botiquin), send(Dialogo, append, text(Botiquin))),
    send(Dialogo, open).

consultar_acciones_medicamento_especifico :-
    new(Dialogo, dialog('Consulta de Acciones de Medicamento Específico')),
    send(Dialogo, append, text('Ingrese el nombre del medicamento:')),
    send(Dialogo, append, new(MedicamentoInput, text_item(medicamento, ''))),
    send(Dialogo, append, button('Consultar', message(@prolog, mostrar_acciones_medicamento, MedicamentoInput?selection))),
    send(Dialogo, open).

mostrar_acciones_medicamento(Medicamento) :-
    new(ResultadoDialogo, dialog('Acciones del Medicamento')),
    (   medicamento(Planta, Medicamento),
        propiedad(Planta, Propiedad),
        format(atom(Texto), 'Medicamento: ~w, Planta: ~w, Propiedad: ~w', [Medicamento, Planta, Propiedad]),
        send(ResultadoDialogo, append, text(Texto)),
        fail
    ;   true
    ),
    (   send(ResultadoDialogo?members?size, equal, 1) ->  % Si solo está el botón de cerrar, no se encontraron datos
        (   medicamento(_, Medicamento) ->
            send(ResultadoDialogo, append, text('No se encontraron acciones para este medicamento.'))
        ;   send(ResultadoDialogo, append, text('El medicamento no existe en la base de datos.'))
        )
    ;   true
    ),
    send(ResultadoDialogo, append, button('Cerrar', message(ResultadoDialogo, destroy))),
    send(ResultadoDialogo, open).



% Crear la interfaz
crear_interfaz :-
    new(MainWindow, dialog('Sistema Experto en Plantas Medicinales')),
    % Establecer tamanio predefinido para la ventana
    send(MainWindow, size, size(400, 600)),

    % Crear menu de barras
    new(MenuBar, menu_bar),
    send(MainWindow, append, MenuBar),

    % Crear menu "Consultas"
    new(ConsultasMenu, popup('Consultas')),
    send(MenuBar, append, ConsultasMenu),

    % Opciones de consulta
    send(ConsultasMenu, append,
        menu_item('Plantas Medicinales', message(@prolog, consultar_plantas_medicinales))
    ),
    send(ConsultasMenu, append,
        menu_item('Elementos en las Plantas', message(@prolog, consultar_elementos_planta))
    ),
    send(ConsultasMenu, append,
        menu_item('Elementos en Planta Especifica', message(@prolog, consultar_elementos_planta_especifica))
    ),
    send(ConsultasMenu, append,
        menu_item('Plantas que Producen Medicamentos', message(@prolog, consultar_plantas_medicinales_prod_med))
    ),
    send(ConsultasMenu, append,
        menu_item('Medicamentos de Planta Especifica', message(@prolog, consultar_medicamentos_planta_especifica))
    ),
    send(ConsultasMenu, append,
        menu_item('Medicamentos de Plantas', message(@prolog, consultar_medicamentos_plantas))
    ),
    send(ConsultasMenu, append,
        menu_item('Acciones de Medicamentos de Plantas', message(@prolog, consultar_acciones_medicamentos_plantas))
    ),
    send(ConsultasMenu, append,
        menu_item('Acciones de Medicamento Especifico', message(@prolog, consultar_acciones_medicamento_especifico))
    ),
    send(ConsultasMenu, append,
        menu_item('Acciones de Plantas', message(@prolog, consultar_acciones_plantas))
    ),
    send(ConsultasMenu, append,
        menu_item('Significado de Accion', message(@prolog, consultar_significado_palabra))
    ),
    send(ConsultasMenu, append,
        menu_item('Listado de Plantas y Acciones', message(@prolog, consultar_listado_plantas_acciones))
    ),
    send(ConsultasMenu, append,
        menu_item('Acciones de Planta Especifica', message(@prolog, consultar_acciones_planta_especifica))
    ),
    send(ConsultasMenu, append,
        menu_item('Plantas Analgesicas', message(@prolog, consultar_plantas_analgesicas))
    ),
    send(ConsultasMenu, append,
        menu_item('Plantas y su Nombre Cientifico', message(@prolog, consultar_plantas_nombre_cientifico))
    ),
    send(ConsultasMenu, append,
        menu_item('Enfermedades que Curan las Plantas', message(@prolog, consultar_enfermedades_curan_plantas))
    ),
    send(ConsultasMenu, append,
        menu_item('Enfermedades Curadas por Planta Especifica', message(@prolog, consultar_enfermedades_curadas_planta_especifica))
    ),
    send(ConsultasMenu, append,
        menu_item('Plantas que Curan una Enfermedad', message(@prolog, consultar_plantas_curan_enfermedad))
    ),
    send(ConsultasMenu, append,
        menu_item('Formas de Preparacion para Tratamiento', message(@prolog, consultar_forma_preparacion_tratamiento))
    ),
    send(ConsultasMenu, append,
        menu_item('Modos de Preparacion de Planta Especifica', message(@prolog, consultar_modos_preparacion_planta))
    ),
    send(ConsultasMenu, append,
        menu_item('Tratamiento y Preparacion para Enfermedad', message(@prolog, consultar_forma_preparacion_tratamiento))
    ),
    send(ConsultasMenu, append,
        menu_item('Origenes de Plantas Medicinales', message(@prolog, mostrar_origen_todas))
    ),
    send(ConsultasMenu, append,
        menu_item('Origen de Planta especifica', message(@prolog, consultar_origen_planta))
    ),
    send(ConsultasMenu, append,
        menu_item('Botiquin de Plantas', message(@prolog, mostrar_plantas_botiquin))
    ),

    % Bot�n para cerrar la aplicaci�n
    new(CerrarBoton, button('Cerrar', message(@prolog, halt))),
    send(MainWindow, append, CerrarBoton),

    % Mostrar la ventana principal
    send(MainWindow, open).

:- crear_interfaz.
