xquery version "1.0-ml";
(:
 : xqysp.xqy
 :
 : Copyright (c) 2011-2014 Michael Blakeley. All Rights Reserved.
 :
 : Licensed under the Apache License, Version 2.0 (the "License");
 : you may not use this file except in compliance with the License.
 : You may obtain a copy of the License at
 :
 : http://www.apache.org/licenses/LICENSE-2.0
 :
 : Unless required by applicable law or agreed to in writing, software
 : distributed under the License is distributed on an "AS IS" BASIS,
 : WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 : See the License for the specific language governing permissions and
 : limitations under the License.
 :
 : The use of the Apache License does not indicate that this project is
 : affiliated with the Apache Software Foundation.
 :
 :)
module namespace p = "com.blakeley.xqysp";

declare default function namespace "http://www.w3.org/2005/xpath-functions";

declare default element namespace "com.blakeley.xqysp";

(:
 : BNF - rough guide
 :
 : L ::= expr*
 : expr ::= group | infixExpr | term
 : group ::= prefixOp? '(' expr* ')'
 : prefixOp ::= "+" | "-" | "~" | "NOT"
 : infixExpr ::= (term | group) " " infixOp " " (term | group)
 : infixOp ::= "*" | "OR" | "|" | "AND"
 : term ::= prefixOp? (field fieldOp)? (group | literal)
 : field ::= (letter | "_")+
 : fieldOp ::= [":" | "=" | ">" | ">=" | "<" | "<=" | "!" | "!="]
 : literal ::= (word | quoted_words) weight?
 : quoted_words ::= '"' word (" " word)* '"'
 : word ::= (letter | digit | "_")+
 : weight ::= ^digit+
 : number ::= digit+
 : letter ::= [A-Za-z]
 : digit ::= [0-9]
 :)

(:
 : XML output
 :
 : root - root element, equivalent to L in BNF
 : group
 : expression
 : operator
 : term, with attributes @bool
 :)

declare private variable $DEBUG as xs:boolean := false();

declare private variable $TOKS as cts:token* := ();

declare private variable $TOKS-COUNT as xs:integer := count($TOKS);

declare private variable $X as xs:integer := -1;

declare variable $TOK-AND := cts:word('AND');
declare variable $TOK-APOS := cts:punctuation("'");
declare variable $TOK-CARAT := cts:punctuation('^');
declare variable $TOK-GROUP-START := cts:punctuation('(');
declare variable $TOK-GROUP-END := cts:punctuation(')');
declare variable $TOK-HYPHEN := cts:punctuation('-');
declare variable $TOK-NEAR := cts:word('NEAR');
declare variable $TOK-NOT := cts:word('NOT');
declare variable $TOK-ONEAR := cts:word('ONEAR');
declare variable $TOK-OR := cts:word('OR');
declare variable $TOK-QUOTE := cts:punctuation('"');
declare variable $TOK-SPACE := cts:space(' ');
declare variable $TOK-UNDERSCORE := cts:punctuation('_');

declare variable $TOKS-FIELD := (
  cts:punctuation(':'), cts:punctuation('='),
  $TOKS-INEQ);
(: for range query terms - inequality :)
declare variable $TOKS-INEQ := (
   cts:punctuation('!'), cts:punctuation('>'), cts:punctuation('<'));
declare variable $TOKS-INEQ-VALID := (
  $TOKS-INEQ,
  cts:punctuation('!='), cts:punctuation('>='), cts:punctuation('<=')) ;
declare variable $TOKS-INFIX := ($TOK-AND, $TOK-NEAR, $TOK-ONEAR, $TOKS-OR);
declare variable $TOKS-OP-JOIN := (cts:punctuation('/'));
declare variable $TOKS-OR := (cts:punctuation('|'), cts:word('OR'));
declare variable $TOKS-PREFIX := (
  $TOK-NOT, $TOK-HYPHEN, cts:punctuation('+'), cts:punctuation('~'));
declare variable $TOKS-WEIGHT := ($TOK-CARAT) ;
declare variable $TOKS-WILDCARD := (
  cts:punctuation('*'), cts:punctuation('?'));
declare variable $TOKS-NOT-WORD := (
  $TOK-GROUP-START, $TOK-GROUP-END, $TOK-QUOTE,
  $TOKS-OR, $TOKS-WEIGHT) ;

declare function p:debug-set($debug as xs:boolean)
as empty-sequence() {
  xdmp:set($DEBUG, $debug)
};

declare function p:debug($s as item()*)
as empty-sequence()
{
  if (not($DEBUG)) then () else xdmp:log(
    text {
      "DEBUG:",
      if (empty($s)) then "()" else
      for $i in $s return typeswitch($i)
      case node() return xdmp:quote($i)
      case cts:token return xdmp:describe($i)
      default return $i})
};

declare function p:debug-state($label as xs:anyAtomicType+)
as empty-sequence()
{
  if (not($DEBUG)) then () else p:debug(
    ($label,
      '[debug-state]', concat($X, '/', $TOKS-COUNT),
      let $count := 1 + $TOKS-COUNT - $X
      return xdmp:describe(
        subsequence($TOKS, $X, $count), $TOKS-COUNT)))
};

declare function p:error($code as xs:string, $s as item()*)
 as empty-sequence()
{
  error(
    (), $code, text {
      if (empty($s)) then '()' else
      for $i in $s return typeswitch ($i)
      case node() return xdmp:quote($i)
      case cts:token return xdmp:quote($i)
      default return $i,
      $X, $TOKS-COUNT, xdmp:describe($TOKS, $TOKS-COUNT)} )
};

declare function p:error($code as xs:string)
 as empty-sequence()
{
  p:error($code, ())
};

declare function p:warning($message as xs:string)
 as attribute(warning)
{
  attribute warning { $message }
};

declare private function p:empty() as xs:boolean
{
  $X gt $TOKS-COUNT
};

declare private function p:has-next() as xs:boolean
{
  $X le $TOKS-COUNT
};

(: This unrolls code from skip and peek. :)
declare private function p:next($n as xs:integer) as cts:token*
{
  subsequence($TOKS, $X, $n),
  xdmp:set($X, $n + $X)
};

declare private function p:next() as cts:token? { p:next(1) };

declare private function p:peek($n as xs:integer) as cts:token*
{
  subsequence($TOKS, $X, $n)
};

declare private function p:peek() as cts:token? { p:peek(1) };

declare private function p:rewind($n as xs:integer) as empty-sequence()
{
  xdmp:set($X, $X - $n)
};

declare private function p:rewind() as empty-sequence() { p:rewind(1) };

declare private function p:skip($n as xs:integer) as empty-sequence()
{
  xdmp:set($X, $n + $X)
};

declare private function p:skip() as cts:token? { p:skip(1) };

declare private function p:next-until(
  $tok as cts:token?,
  $halt as cts:token+)
as cts:token*
{
  if (not($DEBUG)) then () else p:debug-state(
    ('next-until: tok', $tok, 'halt', $halt)),
  if (empty($tok)) then ()
  else if ($tok = $halt) then p:rewind(1)
  else ($tok, p:next-until(p:next(), $halt))
};

(: list may contain strings or nodes :)
declare private function p:maybe-wrap(
  $name as xs:string,
  $min as xs:integer,
  $list as item()*,
  $prepend as node()*)
as element()*
{
  if (not($DEBUG)) then () else p:debug-state(
    ('maybe-wrap:', $name, 'min', $min,
      'list', xdmp:describe($list), 'prepend', xdmp:describe($prepend))),
  if (count($list) lt $min
    and not($list instance of xs:string)) then (
    (: Preserve the prepend nodes, some of which may be attributes. :)
    if (empty($prepend)) then () else element meta { $prepend },
    $list)
  else element { $name } { $prepend, $list }
};

declare private function p:maybe-wrap(
  $name as xs:string,
  $list as item()*)
as element()*
{
  p:maybe-wrap($name, 2, $list, ())
};

declare private function p:maybe-wrap-expr(
  $type as xs:string,
  $op as xs:string,
  $list as element()*)
as element()? {
  p:maybe-wrap(
    'expression',
    if ($type eq 'prefix') then 1 else 2,
    $list,
    (attribute type { $type },
      attribute op { $op }))
};

(: caller does not want single group :)
declare private function p:ungroup-single(
  $list as element()*)
as element()*
{
  if ($list instance of element(group)) then $list/*
  else $list
};

(: caller does not want groups of one expr :)
declare private function p:maybe-ungroup(
  $expr as element()?)
as element()*
{
  typeswitch ($expr)
  case element(group) return (
    if (count($expr/*, 2) gt 1) then $expr
    else $expr/*)
  default return $expr
};

declare private function p:weight($tok as cts:token)
as attribute(weight)?
{
  if (not($tok castable as xs:integer)) then ()
  else attribute weight { xs:integer($tok) }
};

declare private function p:weight()
as attribute(weight)?
{
  if (not(p:peek() = $TOKS-WEIGHT)) then ()
  else p:weight(p:next(2)[2])
};

declare private function p:word(
  $tok as cts:token?,
  $next as cts:token?,
  $join as xs:boolean)
as xs:string?
{
  if (not($DEBUG)) then () else p:debug-state(
    ('word:', 'tok', $tok, 'next', $next,
      'join', $join)),
  if ($join) then string-join(
    ($tok,
      if (not($DEBUG)) then () else p:debug-state('word: next-until'),
      p:next-until(
        $next,
        ($TOKS-FIELD, $TOK-GROUP-START, $TOK-GROUP-END,
          $TOK-QUOTE, $TOK-SPACE))),
    '')
  else (
    $tok,
    p:rewind(),
    if (not($DEBUG)) then () else p:debug-state(
      ('word: rewind', $next)))
};

declare private function p:word(
  $tok as cts:token?,
  $next as cts:token?)
as xs:string?
{
  if (not($DEBUG)) then () else p:debug-state(
    ('word:', 'tok', $tok, 'next', $next)),
  if (empty($tok)) then ()
  else if (not($tok instance of cts:word
      or $tok = $TOKS-WILDCARD)) then (
    if (not($DEBUG)) then () else p:debug-state(('word: skip', $tok)),
    p:word($next, p:next()))
  else if (empty($next)) then $tok
  (: Join if there is a leading wildcard, following word,
   : or following join token.
   : Sequences of cts:word without whitespace
   : can happen if the tokenization class changes mid-word.
   :)
  else p:word(
    $tok, $next,
    $next instance of cts:word
    or not($next = $TOKS-NOT-WORD))
};

declare private function p:quoted-word(
  $tok as cts:token,
  $next as cts:token?)
as xs:string?
{
  if (not($DEBUG)) then () else p:debug-state(
    ('quoted-word:', $tok, 'next', xdmp:describe($next))),
  (: TODO optimize out this assertion? :)
  if (not($tok = $TOK-QUOTE)) then p:error(
    'UNEXPECTED', ('not a quote', $tok))
  else if (empty($next)) then ()
  (: Surface the end quote so the caller can verify it. :)
  else if ($next eq $TOK-QUOTE) then p:rewind()
  else string-join(
    ($next, p:next-until(p:next(), $TOK-QUOTE)),
    '')
};

declare private function p:literal(
  $tok as cts:token?,
  $next as cts:token?)
as element()?
{
  if (not($DEBUG)) then () else p:debug-state(
    ('literal:', 'tok', $tok, 'next', $next)),
  if (empty($tok)) then ()
  else if ($tok eq $TOK-SPACE) then p:literal($next, p:next())
  else if ($tok instance of cts:punctuation
    and not($tok = ($TOK-QUOTE, $TOKS-WILDCARD))) then p:literal(
    $next, p:next())
  else p:maybe-wrap(
    'literal',
    1,
    if ($tok eq $TOK-QUOTE) then p:quoted-word($tok, $next)
    else p:word($tok, $next),
    (p:weight(),
      (: Generate any warnings.
       : As a side-effect of handling TOK-QUOTE, skip any end-quote.
       :)
      (if ($tok eq $TOK-QUOTE and empty(p:next())) then 'unterminated quote'
        else ())
      ! p:warning(.)))
};

declare private function p:field-op(
  $op as cts:token?,
  $next as cts:token?)
as xs:string
{
  (: join complex ops, eg '<=' :)
  if ($op = $TOKS-INEQ
    and concat($op, $next) = $TOKS-INEQ-VALID) then concat($op, $next)
  else $op
};

declare private function p:field2(
  $literal as element(),
  $op as xs:string,
  $next as cts:token?)
as element()?
{
  element field {
    attribute name { $literal },
    attribute op { $op },
    p:maybe-ungroup(
      if ($op eq '') then p:error('UNEXPECTED')
      else if (string-length($op) eq 1) then p:group($next, p:next())
      else p:group(p:next(), p:next())) }
};

declare private function p:field(
  $literal as element()?,
  $tok as cts:token?,
  $next as cts:token?)
as element()?
{
  (: literal is the field name, tok is the operator, and next is the value :)
  if (not($DEBUG)) then () else p:debug-state(
    ('field#3:', 'literal', $literal, 'tok', $tok, 'next', $next)),
  if (empty($tok)) then $literal
  (: empty $next is ok :)
  else if ($tok eq $TOK-SPACE) then (
    (: maybe there is an extra space before the operator? :)
    if ($next = $TOKS-FIELD) then p:field($literal, $next, p:next())
    else ($literal, p:rewind()))
  (: whitespace $next is ok :)
  (: detect and encode inequality ops :)
  else if ($tok = $TOKS-FIELD) then p:field2(
    $literal, p:field-op($tok, $next), $next)
  (: rewind at group end :)
  else if ($tok eq $TOK-GROUP-END) then ($literal, p:rewind(2))
  else ($literal, p:rewind(2))
};

declare private function p:field(
  $tok as cts:token?,
  $next as cts:token?)
as element()?
{
  if (not($DEBUG)) then () else p:debug-state(
    ('field#2:', 'tok', $tok, 'next', $next)),
  p:field(
    p:literal($tok, $next),
    p:next(),
    p:next())
};

(: term ::= prefixOp? (field fieldOp)? (group | literal) :)
declare private function p:term(
  $tok as cts:token?,
  $next as cts:token?)
as element()?
{
  if (not($DEBUG)) then () else p:debug-state(
    ('term:', $tok, 'next', $next)),
  if (empty($tok)) then ()
  else if (empty($next)) then p:literal($tok, p:next())
  else if ($tok eq $TOK-SPACE) then p:term($next, p:next())
  (: defer any check for whitespace next :)
  (: handle prefix operator - but accept groups of 1 :)
  else if ($tok = $TOKS-PREFIX) then p:maybe-wrap-expr(
    'prefix', $tok, p:term($next, p:next()))
  (: handle field if present :)
  else p:field($tok, $next)
};

(: group ::= prefixOp? '(' expr* ')' :)
declare private function p:group(
  $tok as cts:token?,
  $next as cts:token?)
as element()*
{
  if (not($DEBUG)) then () else p:debug-state(
    ('group:', $tok, 'next', $next)),
  if (empty($tok)) then ()
  else if (empty($next)) then p:literal($tok, ())
  else if ($tok eq $TOK-SPACE) then p:group($next, p:next())
  (: delay any check for whitespace next :)
  (: handle prefix operator :)
  else if ($tok = $TOKS-PREFIX) then p:maybe-wrap-expr(
    'prefix', $tok, p:group($next, p:next()))
  (: start of group :)
  else if ($tok eq $TOK-GROUP-START) then p:maybe-wrap(
    'group', 2, p:expr($next, p:next(), $TOK-GROUP-END),
    (if (not($DEBUG) ) then () else p:debug-state(
        ('group: ok', 'next', xdmp:describe(p:peek()),
          p:peek() eq $TOK-GROUP-END)),
      if (p:next() eq $TOK-GROUP-END) then ()
      else p:warning('unterminated group')))
  (: just a term after all :)
  else p:term($tok, $next)
};

declare private function p:infix-op(
  $op as cts:token,
  $next as cts:token?)
as cts:token
{
  if (not($DEBUG)) then () else p:debug-state(
    ('infix-op:', $op, 'next', $next))
  ,
  (: check for operator joins, eg NEAR/5 :)
  if ($next = $TOKS-OP-JOIN) then cts:token(concat($op, $next, p:next()))
  else ($op, p:rewind())
};

declare private function p:infix-expr(
  $op as cts:token?,
  $stack as element()*)
as element()?
{
  if (not($DEBUG)) then () else p:debug(
    ('infix-expr:', $op, count($stack), xdmp:describe($stack)))
  ,
  element expression {
    attribute op { $op },
    attribute type { 'infix' },
    $stack}
};

declare private function p:infix-empty-op(
  $stack as element()*,
  $tok as cts:token?,
  $next as cts:token?)
as element()*
{
  if (not($DEBUG)) then () else p:debug-state(
    ('infix-empty-op:',
      'stack', count($stack),
      'tok', xdmp:describe($tok), 'next', xdmp:describe($next)))
  ,
  if ($tok = $TOKS-INFIX) then p:infix(
    p:infix-op($tok, $next),
    ($stack,
      if (not($DEBUG)) then () else p:debug(
        ('infix: empty op, found', $tok)),
      (: append the next group or term :)
      p:group(p:next(), p:next())),
    p:next(), p:next())
  (: not an infix expression, and stack size should be 1 :)
  else ($stack, p:rewind(2))
};

declare private function p:infix(
  $op as cts:token?,
  $stack as element()*,
  $tok as cts:token?,
  $next as cts:token?)
as element()*
{
  if (not($DEBUG)) then () else p:debug-state(
    ('infix:',
      'op', xdmp:describe($op), 'stack', count($stack),
      'tok', xdmp:describe($tok), 'next', xdmp:describe($next))),
  (:
   : prefixOp ::= "+" | "-" | "~" | "NOT"
   : infixExpr ::= (term | group) " " infixOp " " (term | group)
   : infixOp ::= "*" | "OR" | "|" | "AND"
   : term ::= prefixOp? (field fieldOp)? (group | literal)
   :)
  (: TODO check for group end? propagate halt tokens? :)
  if (empty(($tok, $stack))) then ()
  (: not infix - just a literal :)
  else if (empty(($next, $stack))) then p:literal($tok, ())
  else if ($tok eq $TOK-SPACE) then p:infix($op, $stack, $next, p:next())
  (: delay any check for whitespace next :)
  (: if this is the first group or term, gather it :)
  else if (empty($stack)) then p:infix(
    if (empty($op)) then $op else p:error('UNEXPECTED'),
    (if (not($DEBUG)) then () else p:debug('infix: empty stack'),
      p:group($tok, $next)),
    p:next(), p:next())
  (: now we have at least one group or term - do we have an operator yet? :)
  else if (empty($op)) then p:infix-empty-op($stack, $tok, $next)
  (: with $op, gather next group or term to continue infix expression :)
  else if ($tok = $TOKS-INFIX) then (
    if ($tok eq $op) then p:infix(
      $op,
      ($stack,
        if (not($DEBUG)) then () else p:debug('infix: recurse'),
        (: append the next group or term :)
        p:group($next, p:next())),
      p:next(), p:next())
    (: new infix operator - finalize this one and hand it off :)
    else p:infix(
      p:infix-op($tok, $next),
      (p:infix-expr($op, $stack), p:group($next, p:next())),
      p:next(), p:next()))
  (: no following op, so end it - stack size may be gt 1 :)
  else (
    if (not($DEBUG)) then () else p:debug(('infix:', 'final')),
    p:infix-expr($op, $stack),
    p:rewind(2))
};

declare private function p:expr(
  $tok as cts:token?,
  $next as cts:token?,
  $halt as cts:token*)
as element()*
{
  if (not($DEBUG)) then () else p:debug-state(
    ('expr:', 'tok', xdmp:describe($tok),
      'next', xdmp:describe($next),
      'halt', xdmp:describe($halt))),
  (: halt recursion? :)
  if (empty($tok)) then (
    if (not($DEBUG)) then () else p:debug-state(('expr empty')))
  else if ($tok = $halt) then (
    if (not($DEBUG)) then () else p:debug-state(('expr found halt', $tok)),
    p:rewind(2))
  (: expr ::= group | infixExpr | term :)
  else if ($tok eq $TOK-SPACE) then p:expr($next, p:next(), $halt)
  else if (empty($next)) then (
    p:literal($tok, $next),
    if (empty($halt)) then () else p:rewind(2))
  else (
    p:infix((), (), $tok, $next)
    ,
    (: recurse? :)
    if (p:empty()) then (
      if (not($DEBUG)) then () else p:debug-state(('expr empty')))
    else p:expr(p:next(), p:next(), $halt))
};

(: public entry point :)
declare function p:parse($str as xs:string)
as element()?
{
  if (not($DEBUG)) then () else p:debug(('parse', $str)),
  xdmp:set(
    $TOKS,
    (: Handle "smart quotes" and other annoyances. :)
    cts:tokenize(
      translate(normalize-space($str), '&#x201c;&#x201d;', '""'))),
  xdmp:set($TOKS-COUNT, count($TOKS)),
  xdmp:set($X, 1),
  (: return an empty sequence if the output tree is empty :)
  element root {
    p:ungroup-single(
      p:expr(p:next(), p:next(), ())) }[*]
};

(: xqysp.xqy :)
