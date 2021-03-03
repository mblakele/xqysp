xquery version '1.0-ml';

import module namespace test = 'http://marklogic.com/test' at '/test/test-helper.xqy';

test:assert-true(fn:true()),
test:log("sample-tests COMPLETE....")