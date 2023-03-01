%%%% -*- Mode: Prolog -*-
%%%% json-parsing.pl

parseString([Char, '"'| Xs], [Char], Xs).
parseString([Char | MoreChars], [Char | StringList], Rest):-
    parseString(MoreChars, StringList, Rest).

parseNumber([Number, ',' | Xs], [Number], [',' | Xs]):-
    number_chars(Num, [Number]),
    number(Num), !.
parseNumber([Number, '}' | Xs], [Number], ['}' | Xs]):-
    number_chars(Num, [Number]),
    number(Num), !.
parseNumber([Number, ']' | Xs], [Number], [']' | Xs]):-
    number_chars(Num, [Number]),
    number(Num), !.
parseNumber([Number | Numbers], [Number | NumberList], Rest):-
    parseNumber(Numbers, NumberList, Rest).

parseBoolean([Char, ','| Xs], [Char], Xs).
parseBoolean([Char, '}' | Xs], [Char], ['}' | Xs]).
parseBoolean([Char, ']' | Xs], [Char], [']' | Xs]).
parseBoolean([Char | MoreChars], [Char | BoolList], Rest):-
    parseBoolean(MoreChars, BoolList, Rest).

json_primitive(['{' | RemainingObject], Rest, Object):-
    parseObject(RemainingObject, Object, Rest), !.
json_primitive(['[' | RemainingArray], Rest, Array):-
    parseArray(RemainingArray, Array, Rest), !.
json_primitive([':' | MoreMembers], Rest, X):-
    json_primitive(MoreMembers, Rest, X), !.
json_primitive([' ' | MoreMembers], Rest, X):-
    json_primitive(MoreMembers, Rest, X), !.
json_primitive(['"' | MoreMembers], Rest, X):-
    parseString(MoreMembers, String, Rest),
    string_chars(X, String),
    string(X), !.
json_primitive(Number, Rest, N):-
    parseNumber(Number, NumberList, Rest),
    number_chars(N, NumberList),
    number(N), !.

json_primitive(Chars, Rest, X):-
    parseBoolean(Chars, Bool, Rest),
    atom_chars(X, Bool),
    atom(X), !.

jsonpair(Key, Value, P):-
    P =.. [',', Key, Value].

jsonparse(JSONAtom, Object):-
    atom(JSONAtom),
    atom_string(JSONAtom, JSONString),
    jsonparse(JSONString, Object), !.

jsonparse(JSONString, Object):-
    string_chars(JSONString, List),
    jsonAck(List, Object), !.

jsonAck(['{' | Xs], Object):-
    parseObject(Xs, Object), !.
jsonAck(['[' | Xs], Object):-
    parseArray(Xs, Object), !.

parseArray(Array, ParsedArray):-
    parseArray(Array, ParsedArray, _), !.

parseArray([']'], jsonarray([]), _):- !.
parseArray([']' | Rest], jsonarray([]), Rest):- !.
parseArray([',' | MoreElements], jsonarray(Ys), Remaining):-
    parseArray(MoreElements, jsonarray(Ys), Remaining), !.
parseArray(['"' | MoreElements], jsonarray([ParsedValue | Ys]), Remaining):-
    json_primitive(['"' | MoreElements], Rest, ParsedValue),
    parseArray(Rest, jsonarray(Ys), Remaining), !.
parseArray(MoreElements, jsonarray([ParsedValue | Ys]), Remaining):-
    json_primitive(MoreElements, Rest, ParsedValue),
    parseArray(Rest, jsonarray(Ys), Remaining), !.
parseArray(['{' | MoreElements], jsonarray([ParsedObject | Ys]), Remaining):-
    parseObject(MoreElements, ParsedObject, Rest),
    parseArray(Rest, jsonarray(Ys), Remaining), !.

parseObject(Object, ParsedObject):-
    parseObject(Object, ParsedObject, _), !.

parseObject(['}'], jsonobj([]), _):- !.
parseObject(['}' | Rest], jsonobj([]), Rest):- !.
parseObject([',' | MoreMembers], jsonobj(Ys), Rest):-
            parseObject(MoreMembers, jsonobj(Ys), Rest), !.
parseObject([' ' | MoreMembers], jsonobj(Ys), Rest):-
            parseObject(MoreMembers, jsonobj(Ys), Rest), !.
parseObject(['"' | MoreMembers], jsonobj([(P) | Ys]), Rest):-
    json_primitive(['"' | MoreMembers], MemberYetNotParsed, Key),
    json_primitive(MemberYetNotParsed, RemainingMembers, Value),
    jsonpair(Key, Value, P),
    parseObject(RemainingMembers, jsonobj(Ys), Rest), !.
parseObject(['[' | MoreMembers], jsonobj([ParsedArray | Ys]), Rest):-
    parseArray(MoreMembers, ParsedArray, Remaining),
    parseObject(Remaining, jsonobj(Ys), Rest), !.

/* jsonaccess(Jsonobj, Fields, Result).
* che risulta vero quando Result e' recuperabile seguendo la catena
* di campi presenti in Fields (una lista) a partire da Jsonobj 
*/

% caso base
jsonaccess(PartialResult, [], PartialResult).

% oggetto
jsonaccess(JsonObject, [Field | Fields], Result) :-
    get_pair(JsonObject, Pairs),
    get_string(Pairs, Field, PartialResult),
    jsonaccess(PartialResult, Fields, Result), !.

jsonaccess(JsonObject, [Field], Result):-
    get_pair(JsonObject, Pair),
    get_string(Pair, Field, PartialResult),
    jsonaccess(PartialResult, Field, Result), !.

% fields stringa SWI

jsonaccess(JsonObject, String, Result) :-
    string(String),
    get_pair(JsonObject, Pair),
    get_string(Pair, String, Result), !.

% array

jsonaccess(jsonarray(_), [], _).

jsonaccess(JsonArray, [Field | Fields], Result) :-
    jsonaccess(JsonArray, Field, PartialResult),
    jsonaccess(PartialResult, Fields, Result), !.

% fields numero

jsonaccess(JsonArray, Field, Result) :-
    number(Field),
    get_elements(JsonArray, Elements),
    get_index(Elements, Field, Result), !.

% get pairs of an object

get_pair(JsonObject, Pairs):-
    JsonObject =.. [ _ , List],
    List =.. [_, Pairs].

get_pair(JsonObject, Pairs):-
    JsonObject =.. [_, Pairs].

get_pair(JsonObject, Pairs):-
    JsonObject =.. [ _ , Object],
    get_pair(Object, Pairs).

% get_elements of the array

get_elements(JsonArray, Elements):-
    JsonArray =.. [ _, Elements].

% ricerca attributo

% fallisce sempre
get_string(_, "", _) :-
    fail.

get_string([Pair| _], String, Result) :-
   jsonpair(String, Result, Pair), !.

get_string([_ | Pairs], String, Result) :-
   get_string(Pairs, String, Result), !.

% ricerca indice

% fallisce se lista vuota
get_index([], _, _) :-
    fail.

% in testa trova subito
get_index([Item | _], 0, Item).

% ricerca ricorsiva
get_index([_ | Items], N, Result) :-
    N > 0,
    M is N-1,
    get_index(Items, M, Result).

% jsonread(FileName, JSON)
% ha successo se riesce a leggere un file tramite
% la creazione di oggetto JSON

jsonread(FileName, JSON) :-
% absolute_file_name conversione bilaterale absolute e relative path
    absolute_file_name(FileName, AbsFileName),
    catch(open(AbsFileName, read, Stream), _, false),
    read_line_to_codes(Stream, O),
    jsonparse(O, JSON),
    close(Stream).

% jsondump(JSON, FileName)
% scrive l'oggetto JSON sul file FileName, JSON � la stringa SWI

jsondump(JSON, FileName):-
    open(FileName, write, Out),
    writeOnJson(JSON, Object),
    write(Out,Object),
    close(Out).

% il predicato writeOnJson scorpora il termine json dai suoi
% elementi

writeOnJson(jsonobj(Obj), Object):-
    catch(createJsonObject(Obj, ObjectString), _, false),
    atomic_list_concat(['{', ObjectString], Object), !.

writeOnJson(jsonarray(Array), Object):-
    catch(createJsonArray(Array, ObjectString), _, false),
    atomic_list_concat(['[', ObjectString], Object), !.


% createJson costruisce un compound {} da scrivere sul file json

createJsonObject([], '}').

createJsonObject([(Key, jsonobj(Value))], Object):-
    createJsonObject(Value, FinalValue),
    atomic_list_concat(['{', FinalValue], Final),
    atomic_list_concat(['"',Key,'"', ':', Final, '}'], Object), !.

createJsonObject([(Key, jsonarray(Value))], Object):-
    createJsonArray(Value, FinalValue),
    atomic_list_concat(['[', FinalValue], Final),
    atomic_list_concat(['"',Key,'"', ':', Final, '}'], Object), !.

createJsonObject([(Key, Value)], Object ):-
    atomic_list_concat(['"',Key,'"', ':', Value, '}'], Object), !.

createJsonObject([(Key, jsonobj(Value)) | Pairs], Object):-
    createJsonObject(Value, FinalValue),
    atomic_list_concat(['{', FinalValue], Final),
    atomic_list_concat(['"',Key,'"', ':', Final, ','], PartialObject),
    createJsonObject(Pairs, JsonObject),
    atomic_list_concat([PartialObject, JsonObject], Object), !.

createJsonObject([(Key, jsonarray(Value)) | Pairs], Object):-
    createJsonArray(Value, FinalValue),
    atomic_list_concat(['[', FinalValue], Final),
    atomic_list_concat(['"',Key,'"', ':', Final, ','], PartialObject),
    createJsonObject(Pairs, JsonObject),
    atomic_list_concat([PartialObject, JsonObject], Object), !.


createJsonObject([(Key, Value) | Pairs], Object):-
    atomic_list_concat(['"',Key,'"', ':', Value, ','], PartialObject),
    createJsonObject(Pairs, JsonObject),
    atomic_list_concat([PartialObject, JsonObject], Object), !.

createJsonObject([[List] | Pairs], Object):-
    createJsonArray(List, PartialResult),
    createJsonObject(Pairs, JsonObject),
    atomic_list_concat([PartialResult, JsonObject], Object), !.


createJsonArray([], ']').

createJsonArray([Element], Array):-
    string(Element),
    atomic_list_concat(['"',Element,'"', ']'], Array), !.

createJsonArray([Element], Array):-
    atomic_list_concat([Element, ']'], Array), !.

createJsonArray([Element | Elements], Array):-
    createJsonArray(Elements, JsonArray),
    string(Element),
    atomic_list_concat(['"',Element,'"',',', JsonArray], Array), !.

createJsonArray([Element | Elements], Array):-
    createJsonArray(Elements, JsonArray),
    atomic_list_concat([Element,',', JsonArray], Array), !.

%%%% end of file -- json-parsing.pl
