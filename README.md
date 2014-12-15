XQYSP
===

Yet another parser?
---

Recently I needed to parse a fairly sophisticated search syntax for a project.
I needed a pure XQuery solution, and MarkLogic's built-in search API
wouldn't handle some of the syntax: nested groups, for example.
So I wrote another one.

Here is a rough cut at the BNF

    L ::= expr*
    expr ::= group | infixExpr | term
    group ::= prefixOp? '(' expr* ')'
    prefixOp ::= "+" | "-" | "~" | "NOT"
    infixExpr ::= (term | group) " " infixOp " " (term | group)
    infixOp ::= "*" | "OR" | "|" | "AND" | "ANDNOT" | "NOTIN"
    term ::= prefixOp? (field fieldOp)? (group | literal)
    field ::= (letter | "_")+
    fieldOp ::= [":" | "=" | ">" | ">=" | "<" | "<=" | "!" | "!="]
    literal ::= (word | quoted_words) weight?
    quoted_words ::= '"' word (" " word)* '"'
    word ::= (letter | digit | "_")+
    weight ::= ^digit+
    number ::= digit+
    letter ::= [A-Za-z]
    digit ::= [0-9]

XQYSP takes a slightly different approach than the Search API
or the older lib-parser.xqy, both of which returned cts:query items.
Instead, XQYSP returns an abstract syntax tree (AST) as XML.
It is up to you, the caller, to transform that AST into a cts:query.
That is a little more work for you,
but adds a lot of flexibility at the same time.
Most of the tasks that used to go into lib-parser-custom.xqy
can now be implemented without changing the parser itself.
To make it easier to get started, though,
there is some sample XQuery to generate a query from an AST in
[query-eval.xqy](https://github.com/mblakele/xqysp/blob/master/src/query-eval.xqy)
and there is a test case in
[xqysp.xml](https://github.com/mblakele/xqysp/blob/master/test/xqysp.xml).
The `query-eval.xqy` code is provided as a simple example:
copy and modify it as needed for your application.

If the syntax isn't exactly what you need,
this could make a nice template for your own search parser.
Patches are also welcome.

The test cases use [XQUT](https://github.com/mblakele/xqut).
If you find problems, please provide a test case.

License
---
Copyright (c) 2011-2014 Michael Blakeley. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

The use of the Apache License does not indicate that this project is
affiliated with the Apache Software Foundation.
