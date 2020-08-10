:-[codigo_comum].
:-[puzzles_publicos].

obtem_letras_palavras([HPals|[]],  Letras) :-
    % Exit Condition
    atom_chars(HPals, Chrs),
    Letras = [Chrs].

obtem_letras_palavras([HPals|Rest], Letras) :-
    % Main obtem_letras_palavras
    %%%%%%%%%%
    %     [HPals|Rest] e uma lista de palavras
    %     Letras e uma lista ordenada cujos elementos sao listas com as letras
    % de cada palavra de [HPals|Rest]
    %%%%%%%%%%
    % Sort List
    sort([HPals|Rest], SortedPals),
    [HNewPals|NewRest] = SortedPals,
    % Get atoms from each word
    atom_chars(HNewPals,Chrs),
    obtem_letras_palavras(NewRest, Letras1),
    append([Chrs], Letras1, Letras).



espaco_fila(Fila, Esp) :-
    % Main espaco_fila
    %%%%%%%%%%
    %     Fila e uma fila (linha ou coluna) de uma grelha,
    % significa que Esp e um espaco de Fila 
    %%%%%%%%%%
    espaco_fila_aux(Fila,[],Esp).


espaco_fila_aux([], Result,Result) :-
    % Exit Condition
    % Reaches the end of the Fila
    length(Result, Cmp),
    Cmp >= 3;
    Result = [].

espaco_fila_aux([Element|_],Result, Result) :-
    % Exit Condition
    % Encounters a '#'
    Element == '#',
    length(Result, Cmp),
    Cmp >= 3.

espaco_fila_aux([Element|Rest], Comin,Esp) :-
    % Element is a space
    Element \== '#',
    append(Comin, [Element], Result),
    espaco_fila_aux(Rest, Result, Esp).

espaco_fila_aux([Element|Rest], _,Esp) :-
    % Element is '#'
    Element == '#',
    Rest \== [],
    espaco_fila_aux(Rest, [], Esp).



espacos_fila(Fila, Espacos) :-
    %%%%%%%%%%
    %     Fila e uma fila (linha ou coluna) de uma
    % grelha, significa que Espacos e a lista de todos os espacos de Fila, da esquerda para a
    % direita 
    %%%%%%%%%%
    bagof(Espaco, espaco_fila(Fila, Espaco), Espacos).
    


espacos_puzzle(Matriz, Espacos) :-
    % Main Puzzle
    %%%%%%%%%%
    %     Matrix e uma grelha, significa que
    % Espacos e a lista de espacos de Matriz 
    %%%%%%%%%%
    % Espacos das Filas
    espacos_puzzle(Matriz, [], EspFil),
    mat_transposta(Matriz, Trans),
    % Espacos das Colunas
    espacos_puzzle(Trans, [], EspCol),
    append(EspFil, EspCol, Espacos).

espacos_puzzle([], Acm, Acm).

espacos_puzzle([Fila|Rest], Acm, Espacos) :-
    (espacos_fila(Fila, Esp) -> 
    append(Acm, Esp, Result),
    espacos_puzzle(Rest, Result, Espacos);
    espacos_puzzle(Rest, Acm, Espacos)).
    


espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
    % Main espacos_com_posicoes_comuns
    %%%%%%%%%%
    %     Espacos e uma lista de espacos e Esp e um espaco, significa que Esps_com e a lista de espacos
    % com variaveis em comum com Esp, exceptuando Esp 
   
    espacos_com_posicoes_comuns(Espacos, Esp, [], Esps_com).

espacos_com_posicoes_comuns([], _, Esps_com, Esps_com).
    % Exit Condition

espacos_com_posicoes_comuns([Space|Rest], Esp, Acm, Esps_com) :-
    Space \== Esp,
    on(Esp, Space),
    append(Acm, [Space], Result),
    espacos_com_posicoes_comuns(Rest, Esp, Result, Esps_com).

espacos_com_posicoes_comuns([Space|Rest], Esp, Acm, Esps_com) :-
    Space \== Esp,
    \+ on(Esp, Space),
    espacos_com_posicoes_comuns(Rest, Esp, Acm, Esps_com).

espacos_com_posicoes_comuns([Space|Rest], Esp, Acm, Esps_com) :-
    Space == Esp,
    espacos_com_posicoes_comuns(Rest, Esp, Acm, Esps_com).

on([Element|Rest], Spaces) :-
    on1(Element, Spaces);
    on(Rest, Spaces).

on([[]], [_|_]):-
    false.

on1(Item, [Element|_]) :-
    Item == Element.

on1(Item, [Element|Rest]) :-
    Element \== Item,
    Rest \== [],
    on1(Item, Rest).

on1(Item, [Element|Rest]) :-
    Element \== Item,
    Rest == [],
    false.



palavra_possivel_esp(Pal, Esp, Espacos, Letras) :-
    % Main palavra_possivel_esp
    %%%%%%%%%%
    %     Pal e uma lista de letras de uma palavra, Esp e um espaco, Espacos e uma lista de espacos, e Letras e
    % uma lista de listas de letras de palavras, significa que Pal e uma palavra possivel para
    % o espaco Esp 
    %%%%%%%%%%
    member(Pal,Letras),
    espacos_com_posicoes_comuns(Espacos, Esp, Esps_com),
    Esp = Pal,
    % The other spaces need to still be solvable
    check_possible(Esps_com, Letras).

check_possible([Space|[]], Letras) :-
    % Exit Condition
    check_space(Space, Letras),!.

check_possible([Space|Rest], Letras) :-
    % Main check_possible
    check_space(Space, Letras),
    check_possible(Rest, Letras),!.

check_space(Space, [Letra|Rest]) :-
    % Tentativa de unificacao
    copy_term(Space, X),
    Space \== X,
    X = Letra;
    check_space(Space, Rest).



palavras_possiveis_esp(Letras, Espacos, Esp, Palavras) :-
    % Main palavras_possiveis_esp
    %%%%%%%%%%%%
        % Letras e uma lista de listas de letras de palavras
        % Espacos e uma lista de espacos
        % Esp e um espaco
        % Palavras e a lista ordenada de palavras
    %%%%%%%%%%%%
    findall(Palavra, palavra_possivel_esp(Palavra, Esp, Espacos, Letras), Palavras).
    
palavras_possiveis(Letras, Espacos, Pals_possiveis) :-
    % Main palavras_possiveis
    %%%%%%%%%%%%
        % Letras e uma lista de listas de letras de palavras
        % Espacos e uma lista de espacos
        % Pals_Possiveis e uam lista de palavras possiveis
    %%%%%%%%%%%%
    palavras_possiveis(Letras, Espacos, Espacos, Pals_possiveis).

palavras_possiveis(Letras, [Hesp|[]], Espacos, Pals_possiveis) :-
    palavras_possiveis_esp(Letras, Espacos,Hesp, Pals_possiveis1),
    append([Hesp], [Pals_possiveis1], Pals_almost),
    Pals_possiveis = [Pals_almost].


palavras_possiveis(Letras, [Hesp|Rest], Espacos, Pals_possiveis) :-
    palavras_possiveis_esp(Letras, Espacos,Hesp, Pals_possiveis1),
    append([Hesp], [Pals_possiveis1], Pals_poss),
    palavras_possiveis(Letras, Rest, Espacos, New_pals),
    append([Pals_poss], New_pals, Pals_possiveis).



letras_comuns([Pal|Rest], Letras_comuns) :-
    % Main letras_comuns
    %%%%%%%%%%%%
        % [Pal|Rest] e uma lista de listas de letras
        % Letras_comuns e uma lista de pares(pos, letra)
        % Todas as listas de [Pal|Rest] contem a letra letra na posicao pos
    %%%%%%%%%%%%
    letras_comuns(Pal,Rest, [], Letras_comuns, 1).

letras_comuns([],_, Acm, Acm, _).
    % Exit Condition

letras_comuns([Letter|Rest],Words, Acm, Letras_comuns, Nbr) :-
    (test_letter(Letter, Words, Nbr) -> 
        append(Acm, [(Nbr, Letter)], NewAcm),
        NewNbr is Nbr + 1,
        letras_comuns(Rest, Words, NewAcm, Letras_comuns, NewNbr);
        NewNbr is Nbr + 1,
        letras_comuns(Rest, Words, Acm, Letras_comuns, NewNbr)).

test_letter(_, [], _).

test_letter(Letter, [Hl|Tl],Nbr) :-
    test_word(Letter, Hl, Nbr, 1),
    test_letter(Letter, Tl, Nbr).


test_word(Letter, [Hl|Tl], Nbr_final, Nbr) :-
    (Nbr == Nbr_final -> 
        Letter == Hl;
        NewNbr is Nbr + 1,
        test_word(Letter, Tl, Nbr_final, NewNbr)).

test_word(_,[], _, _) :-
    false.


atribui_comuns([Hpls_esp|[]]) :-
    % Exit Condition
    modifiy_pals_poss(Hpls_esp).

atribui_comuns([Hpls_esp|Tpls_esp]) :-
    /* Atribui ao espaco as letras comuns entre as palavras possiveis */
    modifiy_pals_poss(Hpls_esp),
    atribui_comuns(Tpls_esp).


modifiy_pals_poss([Space|Pals]) :-
    Pals = [HPals|_],
    letras_comuns(HPals, Pairs),
    modify_space(Space, Pairs).

modify_space(_, []).

modify_space(Space,[(Nbr,Letter)|[]]) :-
    nth1(Nbr, Space, Letter).

modify_space(Space,[(Nbr,Letter)|Tpairs]) :-
    nth1(Nbr, Space, Letter),
    modify_space(Space, Tpairs).



retira_impossiveis([Hp|[]], New_pals) :-
    % Exit Condition
    check_pals(Hp, Pals),
    New_pals = [Pals].

retira_impossiveis([Hp|Tp], New_pals) :-
    /* Retira as palavras que deixaram de unificar */
    check_pals(Hp, To_add),
    retira_impossiveis(Tp, Upnew_pals),
    append([To_add], Upnew_pals, New_pals).


check_pals([Space|Words], Final) :-
    [HWord|_] = Words,
    length(HWord, Cmp),
    (Cmp == 1 ->
        Final = [Space|Words];
    check_space(Space, HWord, [], Right_Words),
    append([Space], [Right_Words], Final)).

check_space(_, [], Acm, Acm).

check_space(Space, [HWord|TWord],Acm, Final) :-
    copy_term(Space, X),
    (X = HWord ->
        append(Acm, [HWord], NewAcm),
        check_space(Space, TWord, NewAcm, Final);
        check_space(Space, TWord, Acm, Final)).



obtem_unicas(Pals_Possiveis, Unicas) :-
    % Main obtem_unicas
    %%%%%%%%%%%%
        % Pals_Possiveis e uma lista de palavras possiveis
        % Unicas e a lista de palavras unicas de Pals_Possiveis 
    %%%%%%%%%%%%
    obtem_unicas(Pals_Possiveis, [], Unicas).

obtem_unicas([], Unicas, Unicas).
    % Exit Condition

obtem_unicas([Hpal|Rest], Acm, Unicas) :-
    find_unique(Hpal, Words),
    length(Words, Cmp),
    (Cmp == 1 ->
        append(Acm, Words, NewAcm),
        obtem_unicas(Rest, NewAcm, Unicas);
        obtem_unicas(Rest, Acm, Unicas)).

find_unique([_|AlmostWord], Final) :-
    [Word|_] = AlmostWord,
    Final = Word.



retira_unicas(Pals_poss, NewPal) :-
    % Main retira_unicas
    %%%%%%%%%%%%
        % Pals_poss e uma lista de palavras possiveis
        % NewPal e o resultado de retirar de Pals_poss as palavras unicas
    %%%%%%%%%%%%
    obtem_unicas(Pals_poss, Unicas),
    retira_unicas(Pals_poss, Unicas, [], NewPal).

retira_unicas([], _, Final, Final).
    % Exit Condition

retira_unicas([To_aval|Rest], Unicas, Acm, Final) :-
    aval(To_aval, Unicas, To_add),
    append(Acm, [To_add], NewAcm),
    retira_unicas(Rest, Unicas, NewAcm, Final).

aval([Space|List], Unicas, Final) :-
    [Pals|[]] = List,
    length(Pals, Cmp),
    (Cmp == 1 ->
        Final = [Space|List];
    subtract(Pals, Unicas, PalsNotUnique),
    Final = [Space|[PalsNotUnique]]).



simplifica(Pals_poss, Pals_poss) :-
    % Exit Condition
    check_done(Pals_poss).

simplifica(Pals_poss, Novas_pals) :-
    % Main simplifica
    %%%%%%%%%%%%
        % Pals_poss e uma lista de palavras possiveis
        % Novas_pals e o resultado de simplificar Pals_poss
    %%%%%%%%%%%%
    \+ check_done(Pals_poss),
    % Unificar os espacos com a sua solucao
    atribui_comuns(Pals_poss),
    % Retira as palavras possiveis que ja nao unificam
    retira_impossiveis(Pals_poss, Pals_poss1),
    % Retira as palavras que ja sao uma solucao para outro espaco
    retira_unicas(Pals_poss1, Pals_poss2),
    (Pals_poss == Pals_poss2 ->
        Novas_pals = Pals_poss2;
    simplifica(Pals_poss2, Novas_pals)).

check_done([]).

check_done([To_check|Rest]) :-
    % Check if all spaces still have unsolvable variables
    [Space|_] = To_check,
    check_space(Space),
    check_done(Rest).

check_space([]).

check_space([Fsquare|Rest]) :-
    (var(Fsquare) ->
        false;
        check_space(Rest)).


inicializa([Palavras|Rest], Pals_poss) :-
    % Main inicializa
    %%%%%%%%%%%%
        % Puz e um puzzle
        % Pals_Poss e a lista de palavras possiveis simplificada de Puz
    %%%%%%%%%%%%
    [Matriz|_] = Rest,
    obtem_letras_palavras(Palavras,Letras),
    espacos_puzzle(Matriz, Espacos),
    palavras_possiveis(Letras,Espacos,Pals_not_simpl),
    simplifica(Pals_not_simpl, Pals_poss).



escolhe_menos_alternativas(Pals_poss, Escolha) :-
    % Main escolhe_menos_alternativas
    %%%%%%%%%%%%
        % Pals_poss e uma lista de palavras possives
        % Escolha e o elemento de Pals_poss escolhido
    %%%%%%%%%%%%
    escolhe_menos_alternativas(Pals_poss, [], Escolha).

escolhe_menos_alternativas([], Acm, Escolha) :-
    % Exit Condition
    length(Acm, Cmp),
    (Cmp \== 0 ->
        Escolha = Acm;
        false).

escolhe_menos_alternativas([To_aval|Rest], Acm, Escolha) :-
    [_|List] = To_aval,
    [Words|[]] = List,
    length(Words, Cmp),
    (Cmp \== 1 -> 
        length(Acm, Cmp_Acm),
        (Cmp_Acm == 0 ->
            escolhe_menos_alternativas(Rest, To_aval, Escolha),!;
            [_|List_Words] = Acm,
            [Acm_Words|[]] = List_Words,
            length(Acm_Words, Cmp_Compare),
            (Cmp < Cmp_Compare ->
                escolhe_menos_alternativas(Rest, To_aval, Escolha),!;
                escolhe_menos_alternativas(Rest, Acm, Escolha),!));
        escolhe_menos_alternativas(Rest, Acm, Escolha),!).



experimenta_pal(Escolha, Pals_Poss, Novas_Pals_Poss) :-
    % Main experienta_pal
    %%%%%%%%%%%%
        % Pals_Poss e um lista de palavras possiveis
        % Escolha e um dos elementos escolhidos
        % Novas_Pals_Poss e Pals_Poss simplificada usando Escolha
    %%%%%%%%%%%%
    [Space|Rest] = Escolha,
    [Words|[]] = Rest,
    member(Pal, Words),
    Space = Pal,
    To_change = [Space|[[Pal]]],
    change(Escolha,To_change, Pals_Poss, [],Novas_Pals_Poss).

change(Escolha, To_change, [To_aval|Rest], Acm, Novas_pals) :-
    To_aval == Escolha,
    append(Acm, [To_change], NewAcm),
    change(Escolha, To_change, Rest, NewAcm, Novas_pals),!.

change(Escolha, To_change, [To_aval|Rest], Acm, Novas_pals) :-
    % \+ To_aval == Escolha, 
    append(Acm, [To_aval], NewAcm),
    change(Escolha, To_change, Rest, NewAcm, Novas_pals),!.

change(_,_,[], Novas_pals, Novas_pals).



resolve_aux(Pals_Poss, Pals_Poss) :-
    % Exit Condition
    \+ escolhe_menos_alternativas(Pals_Poss, _).

resolve_aux(Pals_Poss, Novas_pals) :-
    % Main resolve_aux
    %%%%%%%%%%%%
        % Pals_Poss e uma lista de palavras possiveis
        % Novas_pals e o resultado de aplicar um algoritmo de resulucao a Pals_Poss
    %%%%%%%%%%%%
    escolhe_menos_alternativas(Pals_Poss, Escolha),
    experimenta_pal(Escolha, Pals_Poss, Pals_poss1),
    simplifica(Pals_poss1, Pals_poss2),
    resolve_aux(Pals_poss2, Novas_pals),!.



resolve(Puz) :-
    inicializa(Puz, Pals_poss),
    resolve_aux(Pals_poss, _).
