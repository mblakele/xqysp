xquery version "1.0-ml";
(:
 : query-eval.xqy
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
module namespace qe="com.blakeley.xqysp.query-eval";

import module namespace p = "com.blakeley.xqysp" at "xqysp.xqy";

declare default function namespace "http://www.w3.org/2005/xpath-functions";

declare private function qe:expr(
  $op as xs:string,
  $list as cts:query*)
as cts:query?
{
  (: To implement a new operator, simply add it to this code :)
  if (empty($list)) then ()
  (: simple boolean :)
  else if (empty($op) or $op eq 'AND') then cts:and-query($list)
  else if ($op = ('NOT', '-')) then cts:not-query($list)
  else if ($op = ('OR','|')) then cts:or-query($list)
  (: near and variations :)
  else if ($op eq 'NEAR') then cts:near-query($list)
  else if (starts-with($op, 'NEAR/')) then cts:near-query(
    $list, xs:double(substring-after($op, 'NEAR/')))
  else if ($op eq 'ONEAR') then cts:near-query($list, (), 'ordered')
  else if (starts-with($op, 'ONEAR/')) then cts:near-query(
    $list, xs:double(substring-after($op, 'ONEAR/')), 'ordered')
  else error((), 'UNEXPECTED')
};

declare private function qe:field(
  $qnames as xs:QName+,
  $op as xs:string?,
  $list as element()*)
as cts:query?
{
  (: This function leaves many problems unresolved.
   : What if $list contains sub-expressions from nested groups?
   : What if $list contains non-string values for range queries?
   : What if a range-query needs a special collation?
   : Handle these corner-cases if you need them.
   :)
  if (empty($list)) then cts:element-query($qnames, cts:and-query(()))
  else if ($op = ('>', '>=', '<', '<=')) then cts:element-range-query(
    $qnames, $op, $list, (), ($list/@weight)[1])
  else if ($op eq '=') then cts:element-value-query(
    $qnames, $list, (), ($list/@weight)[1])
  else cts:element-word-query(
    $qnames, $list, (), ($list/@weight)[1])
};

declare function qe:eval($n as element())
as cts:query?
{
  (: walk the AST, transforming AST XML into cts:query items :)
  typeswitch($n)
  case element(p:expression) return qe:expr($n/@op, qe:eval($n/*))
  (: NB - no eval, since we may need to handle literals in qe:field too :)
  (: This code works as long as your field names match the QNames.
   : If they do not, replace xs:QName with a lookup function.
   :)
  case element(p:field) return qe:field(
    xs:QName($n/@name/string()), $n/@op, $n/*)
  case element(p:group) return (
    if (count($n/*, 2) lt 2) then qe:eval($n/*)
    else cts:and-query(qe:eval($n/*)))
  (: NB - interesting literals should be handled by the cases above  :)
  case element(p:literal) return cts:word-query($n, (), $n/@weight)
  case element(p:root) return (
    if (count($n/*, 2) lt 2) then qe:eval($n/*)
    else cts:and-query(qe:eval($n/*)))
  default return error((), 'UNEXPECTED')
};

(: public entry point :)
declare function qe:parse($qs as xs:string?) as cts:query?
{
  qe:eval(p:parse($qs))
};

(: query-eval.xqy :)
