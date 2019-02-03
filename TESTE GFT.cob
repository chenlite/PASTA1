
Restaurant Order App (technical test)  
LINGUAGEM : COBOL 
 
================================================================================== 
IDENTIFICATION DIVISION. 
PROGRAM-ID.       TESTE1. 
AUTHOR.           CHEN LI TE. 
DATE-WRITTEN.     03/02/2019. 
 
DATA DIVISION. 
WORKING-STORAGE SECTION. 
 
77 WS-PRI-EGGS             PIC X(01) VALUE ‘S’. 
77 WS-PRI-TOAST            PIC X(01) VALUE ‘S’. 
77 WS-PRI-COFFEE           PIC X(01) VALUE ‘S’. 
77 WS-PRI-STEAK            PIC X(01) VALUE ‘S’. 
77 WS-PRI-POTATO           PIC X(01) VALUE ‘S’. 
77 WS-PRI-WINE             PIC X(01) VALUE ‘S’. 
77 WS-PRI-CAKE             PIC X(01) VALUE ‘S’. 
77 WS-ERRO                 PIC X(01) VALUE ‘N’. 
77 WS-INFORMOU-PRATO       PIC X(01) VALUE ‘N’.       
77 WS-PRATO-TEMP           PIC X(01) VALUE SPACES. 
77 WS-QTD-COFFEE           PIC 9(01) VALUE ZEROS. 
77 WS-QTD-POTATO           PIC 9(01) VALUE ZEROS. 
77 WS-IND-COFFEE           PIC 9(01) VALUE ZEROS. 
77 WS-IND-POTATO           PIC 9(01) VALUE ZEROS. 
77 WS-I                    PIC 9(01) VALUE ZEROS. 
77 WS-J                    PIC 9(01) VALUE ZEROS. 
77 WS-K                    PIC 9(01) VALUE ZEROS. 
77 IND                     PIC 9(01) VALUE ZEROS. 
77 IND1                    PIC 9(01) VALUE ZEROS. 
77 IND2                    PIC 9(01) VALUE ZEROS. 
77 IND-SAI                 PIC 9(01) VALUE ZEROS. 
 
01 TABELA-PRATOS. 
     05 TAB-PERIODO      OCCURS 2 TIMES. 
          10 TAB-DISH    OCCURS 4 TIMES. 
             15 TAB-PRATO  PIC X(06). 
 
01 SAIDA. 
     05 SAI-DISH OCCURS 8 TIMES. 
          10 SAI-PRATO     PIC X(10). 
          10 SAI-VIRGULAS  PIC X(01). 
 
01 FILLER. 
     05 WS-CAMPO. 
          10 WS-PRATO      PIC X(06). 
          10 FILLER        PIC X(02) VALUE ‘(x‘. 
          10 WS-QTD        PIC 9(01). 
          10 FILLER        PIC X(01) VALUE ‘)‘. 
 
01 CARTAO-CONTROLE. 
     05 CC-PERIODO         PIC X(07). 
     05 CC-VIRGULA1        PIC X(01). 
     05 CC-DISH OCCURS 8 TIMES. 
          10 CC-PRATO      PIC X(01). 
          10 CC-VIRGULAS   PIC X(01). 
 
PROCEDURE DIVISION. 
 
       INITIALIZE SAIDA. 
       PERFORM 100-TRATA-CARTAO-CONTROLE. 
.      PERFORM 200-CLASSIFICA-PRATOS 
                   VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 8. 
 
       PERFORM 300-CARREGA-TABELA-PRATOS. 
       PERFORM 400-TRATA-SAIDA. 
       STOP RUN. 

*******  RECEBE E TRATA DADOS DA ENTRADA INFORMADOS VIA CARTÃO CONTROLE JCL
      
100-TRATA-CARTAO-CONTROLE  SECTION. 
 
         ACCEPT CARTAO-CONTROLE. 
         IF CC-PERIODO NOT EQUAL ‘MORNING’ AND ‘NIGHT’ 
             DISPLAY ‘PERIODO INVALIDO’ 
             DISPLAY ‘ERRO CARTAO CONTROLE ’  CARTAO-CONTROLE 
             STOP RUN. 
         END-IF. 
 
         IF CC-VIRGULA1  NOT EQUAL ‘,’ 
             DISPLAY ‘VIRGULA INVALIDA’ 
             DISPLAY ‘ERRO CARTAO CONTROLE ’  CARTAO-CONTROLE 
             STOP RUN. 
        END-IF. 
 
        MOVE 1 TO IND.  
        PERFORM 150-CONSISTE-PRATOS UNTIL IND > 8. 
      
        IF WS-INFORMOU-PRATO EQUAL 'N'
            DISPLAY ‘NAO INFORMOU PRATO’ 
            DISPLAY ‘ERRO CARTAO CONTROLE ’  CARTAO-CONTROLE 
            STOP RUN. 
        END-IF.       
    
100-EXIT. 

*******  CONSISTE DADOS DA ENTRADA INFORMADOS VIA CARTÃO CONTROLE JCL
      
150-CONSISTE-PRATOS  SECTION. 
 
        IF   CC-PRATO (IND)  NOT NUMERIC AND 
             CC-PRATO (IND)  NOT EQUAL ' ‘ 
             DISPLAY ‘PRATO INVALIDO’ 
             DISPLAY ‘ERRO CARTAO CONTROLE ’  CARTAO-CONTROLE 
             STOP RUN. 
        END-IF. 
 
        IF  CC-VIRGULAS (IND)  NOT EQUAL ‘,’ AND ‘ ‘ 
            DISPLAY ‘VIRGULA INVALIDA’   
            DISPLAY ‘ERRO CARTAO CONTROLE ’  CARTAO-CONTROLE 
            STOP RUN. 
        END-IF. 
      
        IF CC-PERIODO EQUAL ‘MORNING’ AND
           CC-PRATO (IND) EQUAL '1' OR '2' OR '3' 
           MOVE 'S' TO WS-INFORMOU-PRATO
        END-IF.
      
        IF CC-PERIODO EQUAL ‘NIGHT’ AND
           CC-PRATO (IND) EQUAL '1' OR '2' OR '3' OR '4'
           MOVE 'S' TO WS-INFORMOU-PRATO
        END-IF.
               
        ADD 1 TO IND. 
 
150-EXIT. 
      
*******  CLASSIFICA ORDEM DOS PRATOS ASCENDENTE      
 
200-CLASSIFICA-PRATOS  SECTION. 
 
        COMPUTE WS-J = WS-I + 1.  
        PERFORM 250-COMPARA-PRATOS 
                    VARYING WS-K FROM WS-J BY 1 UNTIL WS-K >= 8. 
 
200-EXIT. 

*******  COMPARA ORDEM DOS PRATOS        
      
250-COMPARA-PRATOS SECTION. 
 
        IF  CC-PRATO (WS-I)  IS NUMERIC AND 
            CC-PRATO (WS-K)  IS NUMERIC 
              IF CC-PRATO (WS-I) > CC-PRATO (WS-K) 
                   MOVE CC-PRATO (WS-I)  TO WS-PRATO-TEMP 
                   MOVE CC-PRATO (WS-K)  TO CC-PRATO (WS-I) 
                   MOVE WS-PRATO-TEMP    TO CC-PRATO (WS-K) 
              END-IF 
       END-IF. 

250-EXIT. 

*******  CARREGA DESCRIÇÃO DOS PRATOS       
      
300-CARREGA-TABELA-PRATOS  SECTION. 
 
       MOVE ‘EGGS’     TO  TAB-PRATO (1,1). 
       MOVE ‘TOAST’    TO  TAB-PRATO (1,2). 
       MOVE ‘COFFEE’   TO  TAB-PRATO (1,3). 
       MOVE ‘ERROR’    TO  TAB-PRATO (1,4). 
       MOVE ‘STEAK’    TO  TAB-PRATO (2,1). 
       MOVE ‘POTATO’   TO  TAB-PRATO (2,2). 
       MOVE ‘WINE’     TO  TAB-PRATO (2,3). 
       MOVE ‘CAKE’     TO  TAB-PRATO (2,4). 
 
300-EXIT. 

*******  PREPARA O RESULTADO DA SAIDA       
      
400-TRATA-SAIDA  SECTION. 
      
       IF CC-PERIODO EQUAL ‘MORNING’  
            MOVE 1  TO IND1 
       ELSE 
            MOVE 2  TO IND1 
       END-IF. 
 
       MOVE 1 TO IND.  
       PERFORM 450-MONTA-PRATOS UNTIL IND > 8 OR WS-ERRO = ‘S’.
      ****** MONTA CAMPO DE SAIDA PARA MAIS DE UM CAFÉ- COFFEE(xN)
       IF WS-QTD-COFFEE > 1 
            MOVE WS-QTD-COFFEE TO WS-QTD  
            MOVE WS-CAMPO      TO SAI-PRATO (WS-IND-COFFEE) 
       END-IF.
      ****** MONTA CAMPO DE SAIDA PARA MAIS DE UM PRATO DE POTATO - POTATO(xN)
       IF WS-QTD-POTATO > 1 
            MOVE WS-QTD-POTATO TO WS-QTD  
            MOVE WS-CAMPO      TO SAI-PRATO (WS-IND-POTATO) 
       END-IF. 
 
      ****** MOSTRA RESULTADO DE SAIDA
       DISPLAY ‘SAIDA = ’  SAIDA.  
 
400-EXIT. 

*******  MONTA DADOS DA SAIDA / VERIFICA ERROS        
      
450-MONTA-PRATOS  SECTION. 
 
       MOVE CC-PRATO (IND)   TO IND2. 
 
****** TRATA PERIODO MORNING 
       IF IND1  EQUAL 1  
           IF CC-PRATO (IND) EQUAL ‘1’ 
                ADD    1                                              TO IND-SAI  
                IF  WS-PRI-EGGS EQUAL ‘S’ 
                      MOVE ‘N’                                        TO WS-PRI-EGGS 
                      MOVE TAB-PRATO (IND1, IND2)                     TO SAI-PRATO (IND-SAI) 
                      MOVE ‘,’                                        TO SAI-VIRGULAS (IND-SAI) 
                ELSE  
                     MOVE 'ERROR’                                     TO SAI-PRATO (IND-SAI)  
                     MOVE ‘S’                                         TO WS-ERRO  
                END-IF           
           END-IF 
           IF CC-PRATO (IND) EQUAL ‘2’ 
                ADD    1                                              TO IND-SAI  
                IF  WS-PRI-TOAST EQUAL ‘S’ 
                      MOVE ‘N’                                        TO WS-PRI-TOAST 
                      MOVE TAB-PRATO (IND1, IND2)                     TO SAI-PRATO (IND-SAI) 
                      MOVE ‘,’                                        TO SAI-VIRGULAS (IND-SAI) 
                ELSE  
                     MOVE 'ERROR’                                     TO SAI-PRATO (IND-SAI)  
                     MOVE ‘S’                                         TO WS-ERRO  
                END-IF           
           END-IF 
           IF CC-PRATO (IND) EQUAL ‘3’ 
                IF  WS-PRI-COFFEE EQUAL ‘S’ 
                      MOVE ‘N’                                        TO WS-PRI-COFFEE 
                      ADD    1                                        TO IND-SAI  
                      MOVE TAB-PRATO (IND1, IND2)                     TO SAI-PRATO (IND-SAI) 
                      MOVE TAB-PRATO (IND1, IND2)                     TO WS-PRATO 
                      MOVE ‘,’                                        TO SAI-VIRGULAS (IND-SAI) 
                      MOVE 1                                          TO WS-QTD-COFFEE  
                      MOVE IND-SAI                                    TO WS-IND-COFFEE           
                ELSE  
                     ADD     1                                        TO  WS-QTD-COFFEE      
                END-IF           
            END-IF 
            IF  CC-PRATO (IND)  IS NUMERIC AND 
                  CC-PRATO (IND) > ‘3’ 
                  ADD    1                                            TO IND-SAI  
                  MOVE 'ERROR’                                        TO SAI-PRATO (IND-SAI)  
                  MOVE ‘S’                                            TO WS-ERRO  
            END-IF 
       END-IF.   
 
****** TRATA PERIODO NIGHT 
       IF IND1  EQUAL 2  
           IF CC-PRATO (IND) EQUAL ‘1’ 
                ADD    1                                              TO IND-SAI  
                IF  WS-PRI-STEAK EQUAL ‘S’ 
                      MOVE ‘N’                                        TO WS-PRI-STEAK 
                      MOVE TAB-PRATO (IND1, IND2)                     TO SAI-PRATO (IND-SAI) 
                      MOVE ‘,’                                        TO SAI-VIRGULAS (IND-SAI) 
                ELSE  
                     MOVE 'ERROR’                                     TO SAI-PRATO (IND-SAI)  
                     MOVE ‘S’                                         TO WS-ERRO  
                END-IF           
           END-IF 
           IF CC-PRATO (IND) EQUAL ‘2’ 
                IF  WS-PRI-POTATO EQUAL ‘S’ 
                      MOVE ‘N’                                        TO WS-PRI-POTATO 
                      ADD    1                                        TO IND-SAI  
                      MOVE TAB-PRATO (IND1, IND2)                     TO SAI-PRATO (IND-SAI) 
                      MOVE TAB-PRATO (IND1, IND2)                     TO WS-PRATO 
                      MOVE ‘,’                                        TO SAI-VIRGULAS (IND-SAI) 
                      MOVE 1                                          TO WS-QTD-POTATO  
                      MOVE IND-SAI                                    TO WS-IND-POTATO           
                ELSE  
                     ADD     1                                        TO  WS-QTD-POTATO      
                END-IF           
           END-IF 
           IF CC-PRATO (IND) EQUAL ‘3’ 
                ADD    1                                              TO IND-SAI  
                IF  WS-PRI-WINE EQUAL ‘S’ 
                      MOVE ‘N’                                        TO WS-PRI-WINE 
                      MOVE TAB-PRATO (IND1, IND2)                     TO SAI-PRATO (IND-SAI) 
                      MOVE ‘,’                                        TO SAI-VIRGULAS (IND-SAI) 
                ELSE  
                     MOVE 'ERROR’                                     TO SAI-PRATO (IND-SAI)  
                     MOVE ‘S’                                         TO WS-ERRO  
                END-IF           
           END-IF 
           IF CC-PRATO (IND) EQUAL ‘4’ 
                ADD    1                                              TO IND-SAI  
                IF  WS-PRI-CAKE EQUAL ‘S’ 
                      MOVE ‘N’                                        TO WS-PRI-CAKE 
                      MOVE TAB-PRATO (IND1, IND2)                     TO SAI-PRATO (IND-SAI) 
                      MOVE ‘,’                                        TO SAI-VIRGULAS (IND-SAI) 
                ELSE  
                     MOVE 'ERROR’                                     TO SAI-PRATO (IND-SAI)  
                     MOVE ‘S’                                         TO WS-ERRO  
                END-IF           
           END-IF 
           IF CC-PRATO (IND)  IS NUMERIC AND 
                  CC-PRATO (IND) > ‘4’ 
                  ADD    1                                            TO IND-SAI  
                  MOVE 'ERROR’                                        TO SAI-PRATO (IND-SAI)  
                  MOVE ‘S’                                            TO WS-ERRO  
           END-IF 
       END-IF.      
 
       ADD 1 TO IND. 
 
450-EXIT. 
 
************* FIM **************** 
 
================================================================================== 
      
************* TESTES *************** 
JCL 
 
. TESTE 1 
//SYSIN DD *          
MORNING,1,2,3 
 
. TESTE 2 
//SYSIN DD *          
MORNING,2,1,3 
 
. TESTE 3 
//SYSIN DD *          
MORNING,1,2,3,4 
 
. TESTE 4 
//SYSIN DD *          
MORNING,1,2,3,3,3 
 
. TESTE 5 
//SYSIN DD *          
NIGHT  ,1,2,3,4 
 
. TESTE 6 
//SYSIN DD *          
NIGHT  ,1,2,2,4 
 
. TESTE 7 
//SYSIN DD *          
NIGHT  ,1,2,3,5 
 
. TESTE 8 
//SYSIN DD *          
NIGHT  ,1,1,2,3,5
      
. TESTE 9 - INFORMANDO PERIODO INVÁLIDO 
//SYSIN DD *          
XXXXX  ,1,2,3
      
. TESTE 10 - INFORMANDO PRATO INVÁLIDO
//SYSIN DD *          
NIGHT  ,1,@,3      
