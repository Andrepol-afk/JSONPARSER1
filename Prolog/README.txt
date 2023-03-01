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

La realizzazione in Prolog del parser richiede la definizione di due predicati: jsonparse/2 e 
jsonaccess/3.
Il predicato jsonparse/2 è definibile come:
jsonparse(JSONString, Object).
Che risulta vero se JSONString (una stringa SWI Prolog o un atomo Prolog) può venire 
scorporata come stringa, numero o nei termini composti:
Object = jsonobj(Members)
Object = jsonarray(Elements)
e ricorsivamente:
Members = [] or
Members = [Pair | MoreMembers]
Pair = (Attribute, Value)
Attribute = <string SWI Prolog>
Number = <numero Prolog>
Value = <string SWI Prolog> | Number | Object
Elements = [] or
Elements = [Value | MoreElements]

Il predicato jsonaccess/3 è definibile come:
jsonaccess(Jsonobj, Fields, Result).
che risulta vero quando Result è recuperabile seguendo la catena di campi presenti in Fields
(una lista) a partire da Jsonobj. Un campo rappresentato da N (con N un numero maggiore o 
uguale a 0) corrisponde a un indice di un array JSON.
Come caso speciale dovete anche gestire il caso
jsonaccess(Jsonobj, Field, Result).
Dove Field è una stringa SWI Prolog.

Esempi:

?- jsonparse('{"nome" : "Arthur", "cognome" : "Dent"}', O),
 jsonaccess(O, ["nome"], R).
O = jsonobj([(”nome”, ”Arthur”), (”cognome”, ”Dent”)])
R = ”Arthur”

?- jsonparse(’[]’, X).
X = jsonarray([]).
?- jsonparse(’{}’, X).

La vostra libreria dovrà anche fornire due predicati per la lettura da file e la scrittura su file.
jsonread(FileName, JSON).
jsondump(JSON, FileName).
Il predicato jsonread/2 apre il file FileName e ha successo se riesce a costruire un oggetto JSON.
Se FileName non esiste il predicato fallisce. Il suggerimento è di leggere l’intero file in una stringa e poi di richiamare jsonparse/2.
Il predicato jsondump/2 scrive l’oggetto JSON sul file FileName in sintassi JSON. 
Se FileName non esiste, viene creato e se esiste viene sovrascritto. 
Naturalmente ci si aspetta che
?- jsondump(jsonobj([/* stuff */]), ’foo.json’),
 jsonread(’foo.json’, JSON).
JSON = jsonobj([/* stuff */])
Attenzione! Il contenuto del file foo.json scritto da jsondump/2 dovrà essere JSON standard. 
Ciò significa che gli attributi dovranno essere scritti come stringhe e non come atomi.

Per il parser l'oggetto JSON viene diviso ricorsivamente in sottoparti valide, fino ad arrivare ad analizzare gli elementi interni,
ottenendo quindi un termine json_obj contenente una lista di sottotermini contenenti le coppie, o un json_array 
contenente una lista di sottovalues da restituire come conversione del JSON passato in origine.

I principali predicati utilizzati sono:
- json_primitive/3, che riconosce il tipo di parametro oggetto del parsing, distinguendo tra oggetti
                    json, array json, stringhe, numeri e boolean, ignorando ':', ' ', '"'.
- parseString/3, crea una lista di caratteri che rappresenta una stringa.
- parseNumber/3, crea una lista di caratteri che rappresenta un numero.
- parseBoolean/3, crea una lista che rappresenta un boolean o null.
- jsonpair/3, crea un compound rappresentante una coppia chiave-valore.
- jsonparse/2, trasforma l'atomo in una stringa e chiama successivamente parseObject o parseArray.
- jsonack/2, chiamato in jsonparse/2 che riconosce se il parametro da parsare è oggetto o array.
- parseArray/3, effettua il parsing dell'array json.
- parseObject/3, effettua il parsing dell'oggetto json.
- jsonaccess/3, che recupera il campo "Result" seguendo ricorsivamente i campi Fields a partire da Jsonobj.
- get_pair/2, recupera la lista di pair di un oggetto json.
- get_elements/2, recupera gli elementi di un array json.
- get_string/3, trova il pair avente per chiave la stringa passata come argomento.
- get_index/3, trova l'elemento all'indice passato come agomento.
- jsonread/2, per la lettura da file, sia con path relativo che assoluto.
- jsondump/2, per la scrittura su file.
- writeOnJson/2, che scorpora il termine json dai suoi elementi.
- createJsonObj/2 e createJsonArray/2, che tramite la concatenazione di liste trasforma la sintassi Prolog in sintassi JSON
                                       per scrivere nel formato corretto oggetti o array json sul file.