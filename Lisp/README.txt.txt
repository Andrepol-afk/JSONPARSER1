Introduzione.
Lo sviluppo di applicazioni web su Internet, ma non solo, richiede di scambiare dati fra applicazioni eterogenee,
ad esempio tra un client web scritto in Javascript e un server, e viceversa.
Uno standard per lo scambio di dati molto diffuso è lo standard JavaScript Object Notation,o JSON.
Lo scopo di questo progetto è di realizzare due librerie, una in Prolog e l’altra in Common Lisp,
che costruiscano delle strutture dati che rappresentino degli oggetti JSON a partire dalla loro rappresentazione come stringhe.

La sintassi delle stringhe JSON.
La sintassi JSON è definita nel sito https://www.json.org.
Dalla grammatica data, un oggetto JSON può essere scomposto ricorsivamente nelle seguenti parti:
  1. Object
  2. Array
  3. Value
  4. String
  5. Number
Potete evitare di riconoscere caratteri Unicode.

Esempi:
  L’oggetto vuoto:
  {}
  L’array vuoto:
  []
  Un oggetto con due “items”:
  {
  "nome" : "Arthur",
  "cognome" : "Dent"
  }

Un oggetto complesso, contenente un sotto-oggetto, che a sua volta contiene un array di numeri
(notare che, in generale, gli array non devono necessariamente avere tutti gli elementi dello stesso tipo)
{
"modello" : "SuperBook 1234",
"anno di produzione" : 2014,
"processore" : {
 "produttore" : "EsseTi",
"velocità di funzionamento (GHz)" : [1, 2, 4, 8]
 }
}

Un esempio tratto da Wikipedia (una possibile voce di menu)
{
"type": "menu",
"value": "File",
"items": [
 {"value": "New", "action": "CreateNewDoc"},
 {"value": "Open", "action": "OpenDoc"},
 {"value": "Close", "action": "CloseDoc"}
 ]
}

Indicazioni e requisiti.
Dovete costruire un parser per le stringhe JSON che abbiamo descritto. La stringa in input va
analizzata ricorsivamente per comporre una struttura adeguata a memorizzarne le componenti. 
Si cerchi di costruire un parser guidato dalla struttura ricorsiva del testo in input. 
Ad esempio, un eventuale array (e la sua composizione interna in elements) va individuato dopo l’individuazione 
del member del quale fa parte, e il meccanismo di ricerca non deve ripartire dalla stringa iniziale 
ma bensì dal risultato della ricerca del member stesso.

Errori di sintassi.
Se la sintassi che incontrate non è corretta dovete fallire in Prolog o segnalare un errore in 
Common Lisp chiamando la funzione error.

Realizzazione Common Lisp

La realizzazione Common Lisp deve fornire due funzioni. (1) una funzione
jsonparse che accetta in ingresso una stringa e produce una struttura simile a quella illustrata
per la realizzazione Prolog. (2) una funzione jsonaccess che accetta un oggetto JSON
(rappresentato in Common Lisp, così come prodotto dalla funzione jsonparse) e una serie di
“campi”, recupera l’oggetto corrispondente. Un campo rappresentato da N (con N un numero
maggiore o uguale a 0) rappresenta un indice di un array JSON.

La sintassi degli oggetti JSON in Common Lisp è:
Object = ’(’ jsonobj members ’)’
Object = ’(’ jsonarray elements ’)’

e ricorsivamente:
members = pair*
pair = ’(’ attribute value ’)’
attribute = <stringa Common Lisp>
number = <numero Common Lisp>
value = string | number | Object
elements = value*

Esempio

CL-prompt> (defparameter x (jsonparse "{\"nome\" : \"Arthur\",
 \"cognome\" : \"Dent\"}"))
X
;; Attenzione al newline!
CL-prompt> x
(JSONOBJ ("nome" "Arthur") ("cognome" "Dent"))
CL-prompt> (jsonaccess x "cognome")
"Dent"
CL-prompt> (jsonaccess (jsonparse
 "{\"name\" : \"Zaphod\",
 \"heads\" : [[\"Head1\"], [\"Head2\"]]}")
 "heads" 1 0)
"Head2"
CL-prompt> (jsonparse "[1, 2, 3]")
(JSONARRAY 1 2 3)
CL-prompt> (jsonparse "{}")
(JSONOBJ)
CL-prompt> (jsonparse "[]")
(JSONARRAY)
CL-prompt> (jsonparse "{]")
ERROR: syntax error
CL-prompt> (jsonaccess (jsonparse " [1, 2, 3] ") 3) ; Arrays are 0-based.
ERROR: …

Input/Output da e su file
La vostra libreria dovrà anche fornire due funzioni per la lettura da file e la scrittura su file.
(jsonread filename)  JSON
(jsondump JSON filename)  filename
La funzione jsonread apre il file filename ritorna un oggetto JSON (o genera un errore). Se
filename non la funzione genera un errore. Il suggerimento è di leggere l’intero file in una
stringa e poi di richiamare jsonparse.
La funzione jsondump scrive l’oggetto JSON sul file filename in sintassi JSON. Se filename
non esiste, viene creato e se esiste viene sovrascritto. Naturalmente ci si aspetta che
CL-PROMPT> (jsonread (jsondump ’(jsonobj #| stuff |#) ”foo.json”))
(JSONOBJ #| stuff |#)


Per la realizzazione del progetto in linguaggio lisp sono stati utilizzati le seguenti funzioni:

string-to-list: riceve una stringa e la converte in una lista di caratteri;
clean-charlist: riceve una lista di caratteri e rimuove da questa whitespaces, newline, 
                           tab e return;
json-parse-charlist: riceve una lista di caratteri, confronta il primo carattere della lista con 
                     '{' e '[' per verificare se la lista contega un object o un array, invocando
                      rispettivamente il metodo opportuno al parsing;
remove-braces: rimuove primo e ultimo elemento della lista in input;
parse-members: riceve una lista di caratteri e restituisce una lista creata concatenando le
               coppie chiave-valore;
parse-array: riceve una lista di caratteri e restituisce una lista creata concatenando i diversi
             elementi dell'array json;
parse-pair: riceve una lista di caratteri e restituisce la prima coppia chiave-valore ottenuta 
            dalla concatenazione di un attributo e il rispettivo valore;
parse-element: riceve una lista di caratteri e effettua il parsing degli elementi dell'array json,
               (creando una lista) distinguendo numeri, stringhe, oggetti json o array json;
parse-attribute: riceve una lista di caratteri e restituisce la chiave o 'attribute' della coppia
                 chiave-valore;
parse-value: riceve una lista di caratteri e restituisce il value della coppia chiave-valore, 
             distinguendo numeir, stringhe, oggetti json o array json;
parse-string: riceve una lista di caratteri e restituisce due valori, una nuova lista contenente 
              i gli elementi antecedenti al carattere ':' della lista iniziale, e la lista originale;
parse-number: riceve una lista di numeri formato carattere e la converte in una stringa;
parse-atom: riceve una lista di caratteri e verifica che il suo contenuto sia true, false o null;
convert-to-string: riceve una lista di caratteri e la converte in stringa;
remove-parsed-chars: riceve due liste, una prima lista di caratteri che rappresenta l'insieme dei
                     caratteri gia' sottoposti al parsing, che verranno rimossi dal metodo dalla
                     seconda lista ricevuta in input;
create-value-list: riceve una lista di caratteri e restituisce una porzione della lista iniziale
                   (una lista) usata per effettuare il parsing del value;
create-array-list: riceve una lista di caratteri e restituisce una lista rappresentante un array json;
create-obj-list: riceve una lista di caratteri e restituisce una lista rappresentate un oggetto json;
json-access-result: riceve una lista in formato json e una lista contenente dei fields e recupera 
                    l'oggetto corrispondente.
json-access-obj: riceve un oggetto json e un campo e restituisce la coppia chiave-valore avente 
                 per chiave il campo inserito in input.
get-index: riceve un array json e un intero, restituisce l'elemento dell'array all'indice passato
           in input.
json-read: legge il file "filename" in input carattere per carattere, se non esiste restituisce errore.
load-char: legge "inputstream" carattere per carattere.
clean-obj: aggiunge gli elementi necessari all'interno dell'oggetto per scriverlo in maniera corretta.
fix-pairs: aggiunge " : " tra il campo field e il campo attribute.
clean-array: aggiunge ", " e gli spazi tra gli elementi dell'array per formattare la scrittura.
jsondump: scrive l'oggetto parsato in un file JSON, se il file non esiste lo crea,
          se esiste lo sovrascrive.
createjsonobj: converte l'elemento parsato da jsonparse alla corretta sintassi JSON.
jsonreverse: aggiunge le parentesi e riconosce se un elemento e' un oggetto, una lista, 
             un valore booleano, un numero, una stringa o il valore null.
convert-to-json: usato per creare un json object parsato con gli apici doppi in posizione esatta