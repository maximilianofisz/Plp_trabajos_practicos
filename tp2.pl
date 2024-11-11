%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicados básicos %%
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% proceso(+P)
proceso(computar).
proceso(escribir(_, _)).
proceso(leer(_)). 
proceso(secuencia(P, Q)) :- proceso(P), proceso(Q).
proceso(paralelo(P, Q)) :- proceso(P), proceso(Q).

%% Ejercicio 2
%% buffersUsados(+P,-BS)
buffersUsados(computar, []).
buffersUsados(escribir(BS, _), [BS]).
buffersUsados(leer(BS), [BS]).
buffersUsados(secuencia(P, Q), BS) :- buffersUsados(P, BP), buffersUsados(Q, BQ), union(BP, BQ, BS).
buffersUsados(paralelo(P, Q), BS) :- buffersUsados(P, BP), buffersUsados(Q, BQ), union(BP, BQ, BS). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Organización de procesos %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 3
%% intercalar(+XS,+YS,?ZS)
intercalar([], YS, YS).
intercalar(XS, [], XS).
intercalar([X|XS], [Y|YS], ZS) :- intercalar(XS, [Y|YS], Rec), append([X], Rec, ZS).
intercalar([X|XS], [Y|YS], ZS) :- intercalar([X|XS], YS, Rec), append([Y], Rec, ZS).

%% Ejercicio 4
%% serializar(+P,?XS)
serializar(computar, [computar]).
serializar(escribir(B, X), [escribir(B, X)]).
serializar(leer(B), [leer(B)]).
serializar(secuencia(P, Q), XS) :- serializar(P, PS), serializar(Q, QS), append(PS, QS, XS).
serializar(paralelo(P, Q), XS) :- serializar(P, PS), serializar(Q, QS), intercalar(PS, QS, XS). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Contenido de los buffers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% contenidoBuffer(+B,+ProcesoOLista,?Contenidos)
contenidoBuffer(B, Proceso, Contenidos) :- proceso(Proceso), serializar(Proceso, Lista), contenidoBuffer(B, Lista, Contenidos).
contenidoBuffer(B, Lista, Contenidos) :- not(proceso(Lista)), sacarOtrosBuffers(B, Lista, ListaB), pasarContenidos(ListaB, Contenidos).

%
sacarOtrosBuffers(_, [], []).
sacarOtrosBuffers(B, [X|XS], C) :- esAccionB(B, X), sacarOtrosBuffers(B, XS, CRec), append([X], CRec, C).
sacarOtrosBuffers(B, [X|XS], C) :- not(esAccionB(B,X)), sacarOtrosBuffers(B, XS, C). 

%
pasarContenidos([], []).
pasarContenidos([leer(_)|XS], YS) :- pasarContenidos(XS, Rec), append([1], YS, Rec).
pasarContenidos([escribir(_, X)|XS], YS) :- pasarContenidos(XS, Rec), append([X], Rec, YS).

%
sacarLeidos([], []).
sacarLeidos([escribir(B, _)|XS], YS) :- X is leido(B), member(X, XS), sacarPrimero(X, XS, ZS), escribir(ZS, YS).
sacarLeidos([escribir(B, _)|XS], YS) :- X is leido(B), not(member(X, XS)), escribir(ZS, YS),  

%
esAccionB(B, escribir(B, _)).
esAccionB(B, leer(B)).


%% Ejercicio 6
%% contenidoLeido(+ProcesoOLista,?Contenidos)
contenidoLeido(_,_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Contenido de los buffers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 7
%% esSeguro(+P)

%% Ejercicio 8
%% ejecucionSegura( XS,+BS,+CS) - COMPLETAR LA INSTANCIACIÓN DE XS
ejecucionSegura(_,_,_).

  %% 8.1. Analizar la reversibilidad de XS, justificando adecuadamente por qué el predicado se comporta como
  %% lo hace.



%%%%%%%%%%%
%% TESTS %%
%%%%%%%%%%%

% Se espera que completen con las subsecciones de tests que crean necesarias, más allá de las puestas en estos ejemplos

cantidadTestsBasicos(2). % Actualizar con la cantidad de tests que entreguen
testBasico(1) :- proceso(computar).
testBasico(2) :- proceso(secuencia(escribir(1,pepe),escribir(2,pipo))).
testBasico(3) :- buffersUsados(escribir(1, hola), [1]).
% Agregar más tests

cantidadTestsProcesos(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests

cantidadTestsBuffers(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests

cantidadTestsSeguros(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests


tests(basico) :- cantidadTestsBasicos(M), forall(between(1,M,N), testBasico(N)).
tests(procesos) :- cantidadTestsProcesos(M), forall(between(1,M,N), testProcesos(N)).
tests(buffers) :- cantidadTestsBuffers(M), forall(between(1,M,N), testBuffers(N)).
tests(seguros) :- cantidadTestsSeguros(M), forall(between(1,M,N), testSeguros(N)).

tests(todos) :-
  tests(basico),
  tests(procesos),
  tests(buffers),
  tests(seguros).

tests :- tests(todos).