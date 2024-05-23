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
propiedad(ajenjo,'c�licos').
propiedad(ajenjo,anorexia).
propiedad(ajenjo,'invasi�n de lombrices').
propiedad(ajenjo,'disenter�a').
propiedad(ajenjo,'atonia intestinal').
propiedad(ajenjo,buen_estomacal).
propiedad(ajenjo,'diur�tico').
propiedad(ajenjo,mal_aliento).
propiedad(ajo,solitaria).
propiedad(ajo,lombrices).
propiedad(ajo,reumas).
propiedad(ajo,'verm�fugo').
propiedad(ajo,'febr�fugo').
propiedad(ajo,'diur�tico').
propiedad(ajo,expectorante).
propiedad(ajo,sarna).
propiedad(ajo,'ti�a').
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
planta_alivia(ajenjo,'c�licos').
planta_alivia(ajenjo,anorexia).
planta_alivia(ajenjo,'invasi�n de lombrices').
planta_alivia(ajenjo,'disenter�a').
planta_alivia(ajenjo,'atonia intestinal').
planta_alivia(ajenjo,buen_estomacal).
planta_alivia(ajenjo,'diur�tico').
planta_alivia(ajenjo,mal_aliento).

% Ajo
planta_alivia(ajo,solitaria).
planta_alivia(ajo,lombrices).
planta_alivia(ajo,reumas).
planta_alivia(ajo,'verm�fugo').
planta_alivia(ajo,'febr�fugo').
planta_alivia(ajo,'diur�tico').
planta_alivia(ajo,expectorante).
planta_alivia(ajo,sarna).
planta_alivia(ajo,'ti�a').
planta_alivia(ajo,callos).


planta_alivia(eucalipto, bronquitis).
planta_alivia(borraja, bronquitis).
planta_alivia(anacahuite, bronquitis).
planta_alivia(gordolobo, bronquitis).
planta_alivia(tilo, bronquitis).
planta_alivia(benjui, bronquitis).
planta_alivia(marrubio, bronquitis).
planta_alivia(rabano, bronquitis).
planta_alivia(gordolobo, bronconeumonia).
planta_alivia(eucalipto, bronconeumonia).
planta_alivia(mostaza, bronconeumonia).
planta_alivia(ipecacuana, bronconeumonia).
planta_alivia(ortiga, cabello).
planta_alivia(espinosilla, cabello).
planta_alivia(marrubio, cabello).
planta_alivia(romero, cabello).
planta_alivia(anis, calambres).
planta_alivia(tila, calambres).
planta_alivia(manzanilla, calambres).
planta_alivia(ajenjo, calambres).
planta_alivia('diente de leon', 'calculos biliares').
planta_alivia('aceite de oliva', 'calculos biliares').
planta_alivia(retana, 'calculos biliares').
planta_alivia('cabellos de elote', 'calculos renales').
planta_alivia(pinguica, 'calculos renales').
planta_alivia('cola de caballo', 'calculos renales').
planta_alivia(ajo, callos).
planta_alivia(cebolla, callos).
planta_alivia(hiedra, caries).
planta_alivia('cola de caballo', caries).
planta_alivia(ortiga, caspa).
planta_alivia(limon, caspa).
planta_alivia(romero, caspa).
planta_alivia(cuachalalate, 'cancer de utero').
planta_alivia(siempreviva, 'cancer de utero').
planta_alivia(mastuerzo, ciatica).
planta_alivia(higuera, ciatica).
planta_alivia(sauco, ciatica).
planta_alivia(toronjil, circulacion).
planta_alivia(sanguinaria, circulacion).
planta_alivia(salvia, circulacion).
planta_alivia(hamomelis, circulacion).
planta_alivia('cola de caballo', cistitis).
planta_alivia(doradilla, cistitis).
planta_alivia(ajo, cistitis).
planta_alivia('cabellos de elote', cistitis).
planta_alivia(menta, colicos).
planta_alivia(hinojo, colicos).
planta_alivia(manzanilla, colicos).
planta_alivia(toronjil, colicos).
planta_alivia(boldo, colicos).
planta_alivia(linaza, colitis).
planta_alivia(anis, colitis).
planta_alivia(romero, colitis).
planta_alivia('cola de caballo', colitis).
planta_alivia(arnica, contusiones).
planta_alivia(laurel, contusiones).
planta_alivia(brionia, contusiones).
planta_alivia(digital, corazon).
planta_alivia(salvia, corazon).
planta_alivia('nuez de kcla', corazon).
planta_alivia(tejocote, corazon).
planta_alivia(achicoria, 'depurativos de sangre').
planta_alivia('diente de leon', 'depurativos de sangre').
planta_alivia(apio, 'depurativos de sangre').
planta_alivia(sanguinaria, 'depurativos de sangre').
planta_alivia(zarzaparrilla, 'depurativos de sangre').
planta_alivia(berro, 'depurativos de sangre').
planta_alivia(matarique, diabetes).
planta_alivia(tronadora, diabetes).
planta_alivia(eucalipto, diabetes).
planta_alivia(damiana, diabetes).
planta_alivia(capulin, 'diarrea cronica').
planta_alivia(mezquite, 'diarrea cronica').
planta_alivia(tlalchichinole, 'diarrea cronica').
planta_alivia(linaza, 'diarrea por irritacion').
planta_alivia(membrillo, 'diarrea por irritacion').
planta_alivia(arroz, 'diarrea por irritacion').
planta_alivia(cebada, 'diarrea por irritacion').
planta_alivia(malva, abscesos).
planta_alivia(zarzaparrilla, 'absceso hepatico').
planta_alivia(anis, 'acidez estomacal').
planta_alivia(perejil, 'acidez estomacal').
planta_alivia(sanguinaria, 'acido urico').
planta_alivia(limon, 'acido urico').
planta_alivia(sauco, 'acido urico').
planta_alivia(arnica, acne).
planta_alivia(llanten, aftas).
planta_alivia(fenogreco, aftas).
planta_alivia(zarzamora, aftas).
planta_alivia(salvia, agotamiento).
planta_alivia(tilo, agotamiento).
planta_alivia(valeriana, agotamiento).
planta_alivia(yerbabuena, agruras).
planta_alivia(manzanilla, agruras).
planta_alivia('jugo de limon o toronja', agruras).
planta_alivia(pinguica, albuminaria).
planta_alivia('quina roja', albuminaria).
planta_alivia('encino rojo', albuminaria).
planta_alivia(pimiento, alcoholismo).
planta_alivia(salvia, almorranas).
planta_alivia(hemamelis, almorranas).
planta_alivia(sanguinaria, almorranas).
planta_alivia('cola de caballo', almorranas).
planta_alivia(arnica, almorranas).
planta_alivia(sauco, almorranas).
planta_alivia(ajenjo, anemia).
planta_alivia('germen de trigo', anemia).
planta_alivia(quira, anemia).
planta_alivia(canela, anemia).
planta_alivia(alholva, anemia).
planta_alivia(eucalipto, anginas).
planta_alivia(cebada, anginas).
planta_alivia(salvia, anginas).
planta_alivia(tabachin, anginas).
planta_alivia(borraja, anginas).
planta_alivia(ajenja, anorexia).
planta_alivia(gerciana, anorexia).
planta_alivia(yerbabuena, anorexia).
planta_alivia(limon, arteroscuerosis).
planta_alivia(limon, artritis).
planta_alivia(genciana, arteroscuerosis).
planta_alivia(genciana, artritis).
planta_alivia(cardo, arteroscuerosis).
planta_alivia(cardo, artritis).
planta_alivia(zarzaparrilla, arteroscuerosis).
planta_alivia(zarzaparrilla, artritis).
planta_alivia(arnica, arteroscuerosis).
planta_alivia(arnica, artritis).
planta_alivia(chicalote, arteroscuerosis).
planta_alivia(chicalote, artritis).
planta_alivia(toronja, arteroscuerosis).
planta_alivia(toronja, artritis).
planta_alivia(eucalipto, asma).
planta_alivia(marrubio, asma).
planta_alivia(toloache, asma).
planta_alivia(oregano, asma).
planta_alivia(salvia, asma).
planta_alivia(lupulo, 'atonia estomacal').
planta_alivia(eucalipto, 'atonia estomacal').
planta_alivia(cuasia, 'atonia estomacal').
planta_alivia(uva, bazo).
planta_alivia(cerezo, bazo).
planta_alivia(malva, 'boca inflamacion').
planta_alivia(rosal, 'boca inflamacion').
planta_alivia(limon, 'boca inflamacion').
planta_alivia(salvia, 'boca inflamacion').
planta_alivia(rosal, 'boca estomatitis').
planta_alivia(encina, 'boca estomatitis').
planta_alivia(salvia, 'boca estomatitis').
planta_alivia(zarzamora, 'boca estomatitis').
planta_alivia('chaparro amargoso', 'diarrea con sangre').
planta_alivia(muicle, 'diarrea con sangre').
planta_alivia(monacillo, 'diarrea con sangre').
planta_alivia(limon, difteria).
planta_alivia(naranja, difteria).
planta_alivia(tamarindo, disenteria).
planta_alivia('chaparro amargoso', disenteria).
planta_alivia(ipecacuana, disenteria).
planta_alivia(cedron, disenteria).
planta_alivia(anis, hipercdispepsia).
planta_alivia(menta, hipercdispepsia).
planta_alivia(yerbabuena, hipercdispepsia).
planta_alivia(diente, hipercdispepsia).
planta_alivia('te limon', hipercdispepsia).
planta_alivia(genciana, hipoccdispepsiaquina).
planta_alivia(tabaquillo, hipoccdispepsiaquina).
planta_alivia(ruibarbo, hipoccdispepsiaquina).
planta_alivia(alcanfor, 'dolores musculares').
planta_alivia(tamarindo, empacho).
planta_alivia(linaza, enteritis).
planta_alivia(cedron, enteritis).
planta_alivia(llanten, enteritis).
planta_alivia(valeriana, epilepsia).
planta_alivia('hierba de pollo', 'epistaxis').
planta_alivia(cebolla, 'epistaxis').
planta_alivia(perejil, 'epistaxis').
planta_alivia(sauco, erisipela).
planta_alivia(hiedra, erisipela).
planta_alivia(zanahoria, erisipela).
planta_alivia(borraja, escarlatina).
planta_alivia(sauco, escarlatina).
planta_alivia(cebolla, escarlatina).
planta_alivia(ajo, escorbuto).
planta_alivia(limon, escorbuto).
planta_alivia(berro, escorbuto).
planta_alivia(cebolla, escorbuto).
planta_alivia(geranio, escorbuto).
planta_alivia(ciruela, estrenimiento).
planta_alivia(linaza, estrenimiento).
planta_alivia(chia, estrenimiento).
planta_alivia(tamarindo, estrenimiento).
planta_alivia('agar-agar', estrenimiento).
planta_alivia(eucalipto, faringitis).
planta_alivia(lavanda, faringitis).
planta_alivia(anacahuite, faringitis).
planta_alivia(apio, flatulencias).
planta_alivia(tomillo, flatulencias).
planta_alivia(perejil, flatulencias).
planta_alivia('anis estrella', flatulencias).
planta_alivia(hinojo, flatulencias).
planta_alivia(toronjil, flatulencias).
planta_alivia(romero, flatulencias).
planta_alivia(ruibarbo, flatulencias).
planta_alivia(ruda, flatulencias).
planta_alivia(menta, flatulencias).
planta_alivia(arnica, flebitis).
planta_alivia(alfalfa, flebitis).
planta_alivia(lino, flebitis).
planta_alivia(malvavisco, flebitis).
planta_alivia(romero, flebitis).
planta_alivia(quina, flebitis).
planta_alivia(genciana, flemas).
planta_alivia(oregano, flemas).
planta_alivia(fenogreco, forunculos).
planta_alivia(malvavisco, forunculos).
planta_alivia(hiedra, forunculos).
planta_alivia(manzanilla, 'gastralgia (dolor de estomago)').
planta_alivia('anis estrella', 'gastralgia (dolor de estomago)').
planta_alivia('cola de caballo', gonorrea).
planta_alivia(doradilla, gonorrea).
planta_alivia(zarzaparrilla, gonorrea).
planta_alivia(apio, gota).
planta_alivia(cerezo, gota).
planta_alivia(limon, gota).
planta_alivia(pino, gota).
planta_alivia(alcanfor, gota).
planta_alivia(aconito, gota).
planta_alivia(belladona, gota).
planta_alivia(beleno, gota).
planta_alivia(colchico, gota).
planta_alivia(chicalote, gota).
planta_alivia(encina, 'grietas del ano').

planta_alivia(encina,'grietas del pezon').
planta_alivia(nogal,'grietas del pezon').
planta_alivia(milenrama,'grietas del pezon').
planta_alivia(eucalipto,gripe).
planta_alivia(limon,gripe).
planta_alivia(quina,gripe).
planta_alivia(zaxzaparrila,gripe).
planta_alivia(calendula,gripe).
planta_alivia(hinojo,galitosis).
planta_alivia(menta,halitosis).
planta_alivia(mastuerzo,'hemorragia interna').
planta_alivia(ortiga,'hemorragia interna').
planta_alivia(rosal,'Hemorragia interna').
planta_alivia(retama,hepatitis).
planta_alivia(boldo,hepatitis).
planta_alivia(alcachofa,hepatitis).
planta_alivia(prodigiosa,hepatitis).
planta_alivia('cascara sagrada',hepatitis).
planta_alivia(helecho,hernia).
planta_alivia(ricino,hernia).
planta_alivia(tabaco,hernia).
planta_alivia(linaza,herpes).
planta_alivia(llanton,herpes).
planta_alivia(arnica,heridas).
planta_alivia(hamamelis,heridas).
planta_alivia(alcachofa,hidropesia).
planta_alivia(cardo,hidropesia).
planta_alivia(perejil,hidropesia).
planta_alivia(sauco,hidropesia).
planta_alivia(benos,hidropesia).
planta_alivia(retama,hidropesia).
planta_alivia(marrubio,higado).
planta_alivia(boldo,higado).
planta_alivia(doradilla,higado).
planta_alivia(ruibarbo,higado).
planta_alivia(manzanilla,colicos).
planta_alivia(lechuga,bilis).
planta_alivia(tila,bilis).
planta_alivia(papaloquelite,lactancia).
planta_alivia(achicoria,lactancia).
planta_alivia(berros,lactancia).
planta_alivia(llanton,lactancia).
planta_alivia(retama,lactancia).
planta_alivia(tecomasuchil,lactancia).
planta_alivia(ajo,hipertension).
planta_alivia(esparrago,hipertension).
planta_alivia(alpiste,hipertension).
planta_alivia(muerdago,hipertension).
planta_alivia(miel,hipotension).
planta_alivia('nuez de kola',hipotension).
planta_alivia(crategus,hipotension).
planta_alivia(acedera,hipotension).
planta_alivia(anis,hipo).
planta_alivia(hinojo,hipo).
planta_alivia(tila,hipo).
planta_alivia(valeriana,hipo).
planta_alivia(azahax,hipoterismo).
planta_alivia(beleno,hipoterismo).
planta_alivia(gelsemio,hipoterismo).
planta_alivia(tila,hipoterismo).
planta_alivia(valeriana,hipoterismo).
planta_alivia(pasiflora,insomnio).
planta_alivia(azahax,insomnio).
planta_alivia(menta,insomnio).
planta_alivia(manzanilla,insomnio).
planta_alivia(lechuga,insomnio).
planta_alivia(tila,insomnio).
planta_alivia(genciana,intestino).
planta_alivia(melisa,intestino).
planta_alivia(yohimbo,'impotencia sexual').
planta_alivia(daniana,'impotencia sexual').
planta_alivia('nuez vomica','impotencia sexual').
planta_alivia(aguacate,'impotencia sexual').
planta_alivia(manzanilla,jaqueca).
planta_alivia(aconito,jaqueca).
planta_alivia(valeriana,jaqueca).
planta_alivia(tila,jaqueca).
planta_alivia(chicalote,jaqueca).
planta_alivia(hinojo,lactancia).
planta_alivia(anis,lactancia).
planta_alivia(alenta,lactancia).
planta_alivia(perejil,lactancia).
planta_alivia(zanahoria,lactancia).
planta_alivia(aconito,laringitis).
planta_alivia(borraja,laringitis).
planta_alivia(cebolla,laringitis).
planta_alivia(rosa,laringitis).
planta_alivia(benjui,laringitis).
planta_alivia(encino,laringitis).
planta_alivia(encina,leucorrea).
planta_alivia(zarzaparrilla,leucorrea).
planta_alivia(pino,leucorrea).
planta_alivia(enebro,leucorrea).
planta_alivia(genciana,leucorrea).
planta_alivia(ajenjo,lombrices).
planta_alivia(ajo,lombrices).
planta_alivia(cebolla,lombrices).
planta_alivia(brionia,lombrices).
planta_alivia(aguacate,lombrices).
planta_alivia(papaya,lombrices).
planta_alivia(avena,lumpago).
planta_alivia(cebada,lumpago).
planta_alivia(tomillo,lumpago).
planta_alivia(verbena,lumpago).
planta_alivia(fenogreco,llagas).
planta_alivia(eucalipto,llagas).
planta_alivia(llanton,llagas).
planta_alivia(sanguinaria,llagas).
planta_alivia(quina,malaria).
planta_alivia(girasol,malaria).
planta_alivia(eucalipto,malaria).
planta_alivia(caxdo,malaria).
planta_alivia(azahax,menopausia).
planta_alivia(hamamelis,menopausia).
planta_alivia(tila,menopausia).
planta_alivia('quina roja',menopausia).
planta_alivia(azafran,'menstruacion abundante').
planta_alivia(hamamelis,'menstruacion abundante').
planta_alivia(belladona,'menstruacion dolorosa').
planta_alivia('anis estrella','menstruacion dolorosa').
planta_alivia(ruda,'menstruacion escasa').
planta_alivia(ajenjo,'menstruacion escasa').
planta_alivia(manzanilla,'menstruacion escasa').
planta_alivia(apio,'menstruacion irregular').
planta_alivia(hisopo,'menstruacion irregular').
planta_alivia('quina amarilla','menstruacion irregular').
planta_alivia(sabina,'menstruacion irregular').
planta_alivia(artemisa,'menstruacion irregular').
planta_alivia(hamamelis,metorragia).
planta_alivia(zoapatle,metorragia).
planta_alivia(perejil,metorragia).
planta_alivia('cuexrecillo centeno',metorragia).
planta_alivia(clavo,muelas).
planta_alivia(hiedra,muelas).
planta_alivia(ortiga,'nariz hemorragia').
planta_alivia('cola de caballo','nariz hemorragia').
planta_alivia(ruda,'nariz hemorragia').
planta_alivia(eucalipto,'nariz hemorragia').
planta_alivia(anis,nauseas).
planta_alivia(salvia,nauseas).
planta_alivia(ajenjo,nauseas).
planta_alivia(menta,nauseas).
planta_alivia(manzanilla,neuralgias).
planta_alivia(menta,neuralgias).
planta_alivia(valeriana,neuralgias).
planta_alivia(boldo,neuralgias).
planta_alivia(pasiflora,neurastenia).
planta_alivia('te negro',neurastenia).
planta_alivia(mate,neurastenia).
planta_alivia(valeriana,neurastenia).
planta_alivia(linaza,nefritis).
planta_alivia(grama,nefritis).
planta_alivia(cebada,nefritis).
planta_alivia(llanton,nefritis).
planta_alivia(doradilla,nefritis).
planta_alivia(esparrago,nefritis).
planta_alivia(ruda,nefritis).
planta_alivia(toronjil,obesidad).
planta_alivia(marrubio,obesidad).
planta_alivia(limon,obesidad).
planta_alivia(malva,obesidad).
planta_alivia(esparrago,obesidad).
planta_alivia(boldo,oidos).
planta_alivia('aceite de oliva',oidos).
planta_alivia(llanton,oidos).
planta_alivia(hiedra,oidos).
planta_alivia(manzanilla,'ojos conjuntiritis').
planta_alivia(limon,'ojos conjuntiritis').
planta_alivia(hanteu,'ojos conjuntiritis').
planta_alivia(salvia,'ojos conjuntiritis').
planta_alivia(ruda,'ojos conjuntiritis').
planta_alivia(rosal,'ojos conjuntiritis').
planta_alivia(manzanilla,'ojos irritacion').
planta_alivia(limon,'ojos irritacion').
planta_alivia(hanteu,'ojos irritacion').
planta_alivia(salvia,'ojos irritacion').
planta_alivia(ruda,'ojos irritacion').
planta_alivia(rosal,'ojos irritacion').
planta_alivia(ajenjo,paludismo).
planta_alivia(quina,paludismo).
planta_alivia(berro,pecas).
planta_alivia(genciana,pecas).
planta_alivia(rabano,pecas).
planta_alivia(papaya,pecas).
planta_alivia(laurel,'pies olorosos').
planta_alivia(encina,'pies olorosos').
planta_alivia(miel,'piquetes de abeja').
planta_alivia(perejil,'piquetes de abeja').
planta_alivia(cebolla,'piquetes de abeja').
planta_alivia(puexco,'piquetes de abeja').
planta_alivia(fresno,'piquetes de arania').
planta_alivia(ipecacuana,'piquetes de arania').
planta_alivia(alcanfor,'piquetes de mosco').
planta_alivia(perejil,'piquetes de mosco').
planta_alivia(hamamelis,'piquetes de mosco').
planta_alivia(anagalida,'piquetes de vibora').
planta_alivia(jengibre,pleuresia).
planta_alivia(linaza,pleuresia).
planta_alivia(caxdo,pleuresia).
planta_alivia(girasol,pleuresia).
planta_alivia(ipecacuana,piorrea).
planta_alivia('cola de caballo',prostata).
planta_alivia(eucalipto,pulmonia).
planta_alivia(ocute,pulmonia).
planta_alivia(gordolobo,pulmonia).
planta_alivia(borraja,pulmonia).
planta_alivia(sauco,pulmonia).
planta_alivia(linaza,quemaduras).
planta_alivia(cebolla,quemaduras).
planta_alivia(hiedra,quemaduras).
planta_alivia(gordolobo,quemaduras).
planta_alivia(nogal,raquitismo).
planta_alivia(ajo,reumatismo).
planta_alivia(apio,reumatismo).
planta_alivia(borraja,reumatismo).
planta_alivia(gobexnadora,reumatismo).
planta_alivia(pino,reumatismo).
planta_alivia(romero,reumatismo).
planta_alivia(sanguinaria,reumatismo).
planta_alivia(marrubio,reumatismo).
planta_alivia(tabaco,reumatismo).
planta_alivia('cabellos de elote',rinones).
planta_alivia('cola de caballo',rinones).
planta_alivia(apio,rinones).
planta_alivia(eucalipto,ronquera).
planta_alivia(pino,ronquera).
planta_alivia(gordolobo,ronquera).
planta_alivia(ajo,sabanones).
planta_alivia(cebolla,sabanones).
planta_alivia(estafiate,'san vito').
planta_alivia(valeriana,'san vito').
planta_alivia(borraja,sarampion).
planta_alivia(ortiga,sarampion).
planta_alivia(sauco,sarampion).
planta_alivia(ajo,sarna).
planta_alivia(alcanfor,sarna).
planta_alivia(menta,sarna).
planta_alivia(tomillo,sarna).
planta_alivia(romero,sarna).
planta_alivia(encina,sarallido).
planta_alivia(salvia,sarallido).
planta_alivia(tila,sarallido).
planta_alivia(limon,sed).
planta_alivia(tamarindo,sed).
planta_alivia(pirul,sed).
planta_alivia('semilla de calabaza',solitaria).
planta_alivia(granado,solitaria).
planta_alivia('coquito de aceite',solitaria).
planta_alivia('helecho macho',solitaria).
planta_alivia(encina,'sudoracion excesiva').
planta_alivia(alcanfor,tifoidea).
planta_alivia(borraja,tifoidea).
planta_alivia(quina,tifoidea).
planta_alivia(canela,tifoidea).
planta_alivia(romero,tifoidea).
planta_alivia(salvia,tifoidea).
planta_alivia(berro,tinia).
planta_alivia(tila,tinia).
planta_alivia(tamarindo,tinia).
planta_alivia(salvia,tinia).
planta_alivia(eucalipto,tos).
planta_alivia(capulin,tos).
planta_alivia(cedron,tos).
planta_alivia(salvia,tos).
planta_alivia(malva,tos).
planta_alivia(marrubio,tos).
planta_alivia(gelsemio,'tos ferina').
planta_alivia(quina,'tos ferina').
planta_alivia(rabano,'tos ferina').
planta_alivia(violeta,'tos ferina').
planta_alivia('jugo de vastago de platano morado',tuberculosis).
planta_alivia(mastuerzo,tuberculosis).
planta_alivia(berro,tuberculosis).
planta_alivia(ajo,tuberculosis).
planta_alivia(eucalipto,tuberculosis).
planta_alivia(pirul,tuberculosis).
planta_alivia(pino,tuberculosis).
planta_alivia(roble,tuberculosis).
planta_alivia(cuachalalate,ulcera).
planta_alivia(sanguinaria,ulcera).
planta_alivia('cola de caballo',ulcera).
planta_alivia(girasol,ulcera).
planta_alivia(limon,ulticaria).
planta_alivia(ruibarbo,ulticaria).
planta_alivia(hamamelis,varices).
planta_alivia('castaño de indias',varices).
planta_alivia(llanton,varices).
planta_alivia(toronjil,varices).
planta_alivia(apio,vejiga).
planta_alivia(cipres,vejiga).
planta_alivia('cola de caballo',vejiga).
planta_alivia(ortiga,vejiga).
planta_alivia(malva,vejiga).
planta_alivia('leche de higuera',verrugas).
planta_alivia(cebolla,verrugas).
planta_alivia(nogal,verrugas).
planta_alivia(albahaca,vertigos).
planta_alivia(espino,vertigos).
planta_alivia(menta,vomitos).
planta_alivia(tila,vomitos).
planta_alivia(marrubio,vomitos).
planta_alivia(valeriana,vomitos).
planta_alivia(salvia,vomitos).
planta_alivia(cilantro,voz).
planta_alivia(ajo,voz).
planta_alivia(limon,voz).
planta_alivia(pino,voz).
planta_alivia(alfalfa,'Carencia de vitaminas del alfalfa').
planta_alivia(espinacas,'Carencia de vitaminas del espinacas').
planta_alivia(acelga,'Carencia de vitaminas del acelga').
planta_alivia(berro,'Carencia de vitaminas del berro').
planta_alivia(cebolla,'Carencia de vitaminas del cebolla').
planta_alivia(limon,'Carencia de vitaminas del limon').
planta_alivia(zanahoria,'Carencia de vitaminas del zanahoria').
planta_alivia('aceite de bacalao','Carencia de vitaminas del aceite de bacalao').



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
medicamento(barbasco,cortisona).
medicamento(barbasco,progesterona).
medicamento(digital,digitalina).
medicamento(digital,'tonico cardiaco').
medicamento(opio,morfina).
medicamento(opio,codeina).
medicamento(ipeca,emetina).
medicamento('nuez vomica',estricnina).
medicamento('eleboro blanco',veratrina).
medicamento(colchico,colchicina).
medicamento(belladona,atropina).
medicamento(quina,quinina).
medicamento(cacao,teobromina).
medicamento(retana,esparteina).
medicamento(coca,cocaina).
medicamento(peyote,mescalina).
medicamento(efedra,eferina).
medicamento('nenufar amarillo', lutenurina).
medicamento(name,diosponina).
medicamento(artemisa,tauremicina).
medicamento('semilla de yute', olitorisida).
medicamento(toloache,'acido licergico').
medicamento(eucalipto,eucaliptol).
medicamento(rosal,'vitamina c').
medicamento(rosal,quercitrina).


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
planta_botiquin(azahax).
planta_botiquin(malva).
planta_botiquin(marrubio).
planta_botiquin(rosal).
planta_botiquin(ajo).
planta_botiquin(ajenjo).
planta_botiquin(ahuehuete).


% waos
significado(afrodisiaca, 'que excita el apetito sexual').
significado(analgesica, 'que quita o modera el dolor').
significado(anestesica, 'que insensibiliza el cuerpo (o parte)').
significado(antidiarreica, 'que controla diarreas o deposiciones').
significado(antiespasmodica, 'que controla espasmos nerviosos').
significado(antiflogistica, 'que actua contra las inflamaciones').
significado(antipiretica, 'que quita o disminuye la fiebre').
significado(antiseptica, 'que mata los tejidos').
significado(aperitiva, 'que produce apetito (deseo de comer)').
significado(astringente, 'que hace contraer los tejidos').
significado(carminativa, 'que evita la formacion de gases o provoca su expulsion').
significado(colagoga, 'que ayuda a expulsar la bilos').
significado(depurativa, 'que limpia y purifica la sangre').
significado(diaforetica, 'que provoca sudar como cochino').
significado(digestiva, 'que favorece la digestion').
significado(diuretica, 'que provoca la orina').
significado(emetica, 'que provoca nauseas y vomitos').
significado(emenagoga, 'que activa la menstruacion').
significado(estupefaciente, 'que aquieta, duerme o atonta').
significado(expectorante, 'que expulsa flemas y mucosidades').
significado(hemostatica, 'que corta o detiene hemorragias').
significado(hepatica, 'que ayuda al higado').
significado(laxante, 'que purga sin provocar diarrea').
significado(pectoral, 'que ayuda al pecho').
significado(sedante, 'que calma dolores intestinales').
significado(tonica, 'que da fuerza al organismo').
significado(toxica, 'que es venenosa').



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
    send(Dialogo, size, size(400, 300)), % Ajusta el tamaño de la ventana de diálogo según sea necesario

    % Crear un widget browser para admitir el desplazamiento
    new(Browser, browser),
    send(Browser, size, size(380, 250)), % Ajusta el tamaño del browser según sea necesario
    send(Dialogo, append, Browser),

    % Añadir título y espacio en blanco
    send(Browser, append, 'Medicamentos de plantas:'),
    send(Browser, append, ' '), % Espacio en blanco

    % Añadir todos los medicamentos al browser
    forall(medicamento(_, Medicamento),
           (   send(Browser, append, string(Medicamento)),
               send(Browser, append, ' ') % Espacio en blanco para simular un salto de línea
           )),

    % Botón para cerrar el diálogo
    send(Dialogo, append, button('Cerrar', message(Dialogo, destroy))),

    % Mostrar el diálogo
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
    send(Dialogo, size, size(600, 400)), % Ajusta el tamaño de la ventana de diálogo según sea necesario

    % Crear un widget text para mostrar los datos con una fuente mas pequeña
    new(Texto, text),
    send(Texto, font, font(helvetica, roman, 10)), % Ajustar la fuente y el tamaño
    send(Dialogo, append, Texto),

    % Añadir título y espacio en blanco
    send(Texto, append, 'Acciones de Plantas:\n\n'),

    % Obtener todas las plantas y acciones en una lista
    findall((Planta, Accion), planta_alivia(Planta, Accion), PlantasAcciones),

    % Construir una cadena con los datos
    forall(member((Planta, Accion), PlantasAcciones),
           (
               format(atom(Linea), 'Planta: ~w, Accion: ~w\n', [Planta, Accion]),
               send(Texto, append, Linea)
           )),

    % Botón para cerrar el diálogo
    send(Dialogo, append, button('Cerrar', message(Dialogo, destroy))),

    % Mostrar el diálogo
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


consultar_listado_plantas_acciones :-
    new(Dialogo, dialog('Listado de Plantas y Acciones')),
    send(Dialogo, size, size(600, 400)), % Ajusta el tamaño de la ventana de diálogo según sea necesario

    % Crear un widget text para mostrar los datos con una fuente mas pequeña
    new(Texto, text),
    send(Texto, font, font(helvetica, roman, 10)), % Ajustar la fuente y el tamaño
    send(Dialogo, append, Texto),

    % Añadir título y espacio en blanco
    send(Texto, append, 'Listado de Plantas y Acciones:\n\n'),

    % Obtener todas las plantas y acciones en una lista
    forall((planta_alivia(Planta, Medicamento),
            forma_empleo(Medicamento, Accion)),
           (format(atom(Linea), 'Planta: ~w, Accion: ~w\n', [Planta, Accion]),
            send(Texto, append, Linea))),

    % Botón para cerrar el diálogo
    send(Dialogo, append, button('Cerrar', message(Dialogo, destroy))),

    % Mostrar el diálogo
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
    send(Dialogo, size, size(600, 400)), % Ajusta el tamaño de la ventana de diálogo según sea necesario

    % Crear un widget text para mostrar los datos con una fuente mas pequeña
    new(Texto, text),
    send(Texto, font, font(helvetica, roman, 10)), % Ajustar la fuente y el tamaño
    send(Dialogo, append, Texto),

    % Añadir título y espacio en blanco
    send(Texto, append, 'Enfermedades que curan las Plantas:\n\n'),

    % Mostrar todas las plantas y enfermedades que curan
    forall(planta_alivia(Planta, Enfermedad),
           (format(atom(Linea), 'Planta: ~w, Enfermedad: ~w\n', [Planta, Enfermedad]),
            send(Texto, append, Linea))),

    % Botón para cerrar el diálogo
    send(Dialogo, append, button('Cerrar', message(Dialogo, destroy))),

    % Mostrar el diálogo
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
    (   send(ResultadoDialogo?members?size, equal, 0) ->  % Si solo está el botón de cerrar, no se encontraron datos
        (   medicamento(_, Medicamento) ->
            send(ResultadoDialogo, append, text('No se encontraron acciones para este medicamento.'));
            send(ResultadoDialogo, append, text('El medicamento no existe en la base de datos.'))
        )
    ;   true
    ),
    send(ResultadoDialogo, append, button('Cerrar', message(ResultadoDialogo, destroy))),
    send(ResultadoDialogo, open).

% Define la ruta de la imagen (asegúrate de que esta ruta sea correcta en tu sistema)
% ruta_imagen('yerberito.jpg'). % Cambia esta ruta a la ubicación de tu imagen

% Crear la interfaz principal con una imagen de fondo
crear_interfaz :-
    % ruta_imagen(RutaImagen),

    % Crear la ventana principal
    new(MainWindow, dialog('Sistema Experto en Plantas Medicinales')),
    send(MainWindow, size, size(400, 600)),

    % Crear y mostrar la imagen de fondo
    % new(Fondo, picture),
    % send(Fondo, size, size(300, 500)),
    % send(MainWindow, display, Fondo, point(0, 0)),

    % new(Imagen, bitmap(RutaImagen)),
    % send(Imagen, scale, size(300, 500)),
    % send(Fondo, display, Imagen, point(0, 0)),

    % Crear el menú de barras
    new(MenuBar, menu_bar),
    send(MainWindow, append, MenuBar),

    % Crear el menú "Consultas"
    new(ConsultasMenu, popup('Consultas')),
    send(MenuBar, append, ConsultasMenu),

    % Opciones de consulta
    send(ConsultasMenu, append, menu_item('Plantas Medicinales', message(@prolog, consultar_plantas_medicinales))),
    send(ConsultasMenu, append, menu_item('Elementos en las Plantas', message(@prolog, consultar_elementos_planta))),
    send(ConsultasMenu, append, menu_item('Elementos en Planta Especifica', message(@prolog, consultar_elementos_planta_especifica))),
    send(ConsultasMenu, append, menu_item('Plantas que Producen Medicamentos', message(@prolog, consultar_plantas_medicinales_prod_med))),
    send(ConsultasMenu, append, menu_item('Medicamentos de Planta Especifica', message(@prolog, consultar_medicamentos_planta_especifica))),
    send(ConsultasMenu, append, menu_item('Medicamentos de Plantas', message(@prolog, consultar_medicamentos_plantas))),
    send(ConsultasMenu, append, menu_item('Acciones de Medicamentos de Plantas', message(@prolog, consultar_acciones_medicamentos_plantas))),
    send(ConsultasMenu, append, menu_item('Acciones de Medicamento Especifico', message(@prolog, consultar_acciones_medicamento_especifico))),
    send(ConsultasMenu, append, menu_item('Acciones de Plantas', message(@prolog, consultar_acciones_plantas))),
    send(ConsultasMenu, append, menu_item('Significado de Accion', message(@prolog, consultar_significado_palabra))),
    send(ConsultasMenu, append, menu_item('Listado de Plantas y Acciones', message(@prolog, consultar_listado_plantas_acciones))),
    send(ConsultasMenu, append, menu_item('Acciones de Planta Especifica', message(@prolog, consultar_acciones_planta_especifica))),
    send(ConsultasMenu, append, menu_item('Plantas Analgesicas', message(@prolog, consultar_plantas_analgesicas))),
    send(ConsultasMenu, append, menu_item('Plantas y su Nombre Cientifico', message(@prolog, consultar_plantas_nombre_cientifico))),
    send(ConsultasMenu, append, menu_item('Enfermedades que Curan las Plantas', message(@prolog, consultar_enfermedades_curan_plantas))),
    send(ConsultasMenu, append, menu_item('Enfermedades Curadas por Planta Especifica', message(@prolog, consultar_enfermedades_curadas_planta_especifica))),
    send(ConsultasMenu, append, menu_item('Plantas que Curan una Enfermedad', message(@prolog, consultar_plantas_curan_enfermedad))),
    send(ConsultasMenu, append, menu_item('Formas de Preparacion para Tratamiento', message(@prolog, consultar_forma_preparacion_tratamiento))),
    send(ConsultasMenu, append, menu_item('Modos de Preparacion de Planta Especifica', message(@prolog, consultar_modos_preparacion_planta))),
    send(ConsultasMenu, append, menu_item('Tratamiento y Preparacion para Enfermedad', message(@prolog, consultar_forma_preparacion_tratamiento))),
    send(ConsultasMenu, append, menu_item('Origenes de Plantas Medicinales', message(@prolog, mostrar_origen_todas))),
    send(ConsultasMenu, append, menu_item('Origen de Planta especifica', message(@prolog, consultar_origen_planta))),
    send(ConsultasMenu, append, menu_item('Botiquin de Plantas', message(@prolog, mostrar_plantas_botiquin))),

    % Botón para cerrar la aplicación
    new(CerrarBoton, button('Cerrar', message(@prolog, halt))),
    send(MainWindow, append, CerrarBoton),

    % Mostrar la ventana principal
    send(MainWindow, open).

% Ejemplo de cómo llamar a crear_interfaz
:- crear_interfaz.