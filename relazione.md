# microc

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

<div style="text-align: center">
<h3>Andrea Simone Costa - 597287</h3>
</div>

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Scanner

Lo scanner non presenta particolari differenze rispetto a quanto visto a lezione. Lo scanner dichiara una serie di espressioni regolari di varia utilità, ad esempio per riconoscere numeri in base decimale e binaria, e una hashmap per il riconoscimento delle keyword. Sono definite tre regole: una per riconoscere il token successivo, una per gestire i commenti su linea singola e la terza per gestire i commenti multilinea.

## Parser

Il parser utilizza le API monolitiche di menhir, dichiara tutti i token necessari e fa estensivo uso della precedenza assegnabile per risolvere la maggior parte dei conflitti tra regole, come ad esempio il classico problema degli if-then. Per risolvere altre situazioni conflittuali è stato fatto uso del modificatore `%inline` oppure di una appropriata riscrittura delle produzioni, non sempre intuitiva, per evitare le ambiguità.

Tra le definizioni ausiliarie troviamo:

* `vardesc_typ`, un tipo che dichiara dei costruttori base per tenere traccia delle varie star, parentesi tonde e quadre incontrate durante il parsing di una `vardesc`
* `from_vardesc_to_ast_type`, una funzione in grado di trasformare una lista di `vardesc_typ` in un tipo ben formato inseribile nell'AST
* `|@|`, una funzione per aggiungere al volo la location a un nodo creando un record

Poiché vi è il supporto a la dichiarazione di variabili con inizializzazione opzionale e dichiarazioni multiple è stata creata una produzione apposita, `vardecl_init_list`, che si cura di generare l'AST corrispondente dopo aver costruito il tipo di ogni dichiarazione con `from_vardesc_to_ast_type`.

Il supporto al comma operator rasenta quello del linguaggio C ma è leggermente meno potente. È stata inserita una produzione apposita, `expr_comma`, che lo supporta, a differenza della classica `expr`. Il problema che si è voluto risolvere creando questa distinzione riguardava i conflitti sorti con l'invocazione di funzioni e la lista di inizializzazione degli array, luoghi nei quali compaiono espressioni intervallate da virgole. In queste situazioni è stata imposta la produzione `expr`, che non supporta il comma operator tra le alternative immediate, mentre in tutti gli altri luoghi viene fatto uso della produzione `expr_comma`.

Il parser riconosce i cicli `for` e il ciclo `do-while`, ma effettua una operazione di riscrittura di questi ultimi nel ciclo `while`. Altre operazioni di riscrittura sono state eseguite per gli operatori di pre- e post-decremento, tramite il comma operator, e per le abbreviazioni degli assignment operator, che vengono espanse.

Poiché il linguaggio non ammette funzioni che restituiscono array o puntatori, tale invariante è stata imposta nella produzione per la dichiarazione di funzione.

## AST

L'AST fornitoci ha subito le seguenti modifiche:

1. è stato aggiunto il costruttore `Null`, un `expr_node` apposito per il `NULL`
2. è stato aggiunto il costruttore `Comma`, un `expr_node` dedicato al comma operator che contiene una lista di espressioni
3. sono stati modificati i costruttori `topdecl_node.Vardec` e `stmtordec_node.Dec` per supportare la dichiarazione di variabili multiple con corrispondente inizializzazione

È stata definita anche una funzione ricorsiva per trasformare un AST in una stringa utilizzando una "notazione" minimale, per facilitare la lettura e la comprensione di un AST stampato a video.

## Symbol Table

La symbol table è una lista immutabile di hashmap mutabili, una per ogni scope incontrato. Questa configurazione si è rivelata il compromesso ideale: all'inizio di ogni blocco o di ogni funzione è possibile agilmente aggiungere una entry alla lista senza intaccare gli scope esistenti fino a quel momento, le modifiche ad ogni hashmap rimangono visibili per tutta l'esistenza dello scope e all'uscita in automatico esso viene deallocato, in quanto viene meno il riferimento alla lista che lo contiene.

È stata definita anche la funzione `print_keys` per stampare gli identificatori presenti in ogni livello della lista.

## Semantic analysis

Nel file di analisi semantica troviamo l'implementazione del type system. Vengono definiti i tipi delle entità manipolate dal linguaggio e una utility di conversione dalle annotazioni di tipo presenti sull'AST. È interessante notare che il tipo delle funzioni è soggetto a currying. Ad esempio un tipo funzione come `int f(int a, bool b)` diventa `int -> bool -> int`, mentre un tipo funzione come `int f()` diventa `void -> int`. Poter trattare un parametro alla volta si rivela molto agevole in fase di type checking delle chiamate a funzione.

La relazione di equivalenza tra tipi, `check_type_equality`, è un controllo ricorsivo di assegnabilità in realtà che fa perno sulla stretta uguaglianza tra i tipi in gioco, poiché non è prevista una nozione di sottotipo. Per quanto riguarda gli array, si ha che due array non sono mai l'uno assegnabile all'altro, tranne nel caso di chiamata a funzione: l'assegnabilità è consentita solo se entrambi dichiarano la medesima size oppure se almeno uno dei due non la dichiara. In entrambi i casi, come avviene nel linguaggio C, è buona pratica passare un secondo parametro contenente la size.

