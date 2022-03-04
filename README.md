# DynamicSQL
An interpretation of the semantic, syntactic, and typing rules for a dynamically typed, delcarative database language and its implementation in OCaml

Specifically, this is meant to be more of a shallow dive into these rules. This was made as a project for a programming language design course, and is not really a 
"complete" language. For it to be "complete", bare minimum it would need to have a parser for the statements typed into the DynamicSQL console as well as I/O for the
database. We were instructed to not implement these features, as it was out of scope for the course and would eat up time for actually implementing the rules for the 
language. 

For now, the main functions implemented within the code (and their semantics) are:

 - CREATE TABLE tblnm (column_names_list);
 - DROP TABLE tblnm; (deletes like Windows "deletes" a file)
 - INSERT INTO tblnm VALUES (values_list);
 - SELECT * FROM tblnm WHERE condition = wanted_value;
 
In regards to the SELECT statement, this is a very, VERY narrow dive into what it would do in a SQL dialect like SQLite. 

Feel free to add any features or improvements to the code. 

CURRENT TO DO LIST:
    * Implement a stronger "typing" system; i.e. The first value of a column decides the type
        - First name in name col -> name col type is string
        - Whole number in age col -> age col type is int

    * Implement a parser with C
        - Takes input from console and translates statements into code workable with OCaml functions