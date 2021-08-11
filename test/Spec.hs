module Spec(
    runtest
    )where

import Main hiding (main) 
import Test.HUnit

testAttr = TestCase (assertEqual "name" (["has-knife"]) (attributes $ Animal "rosko" ["has-knife"] ))

testName = TestCase (assertEqual "name" ("rosko") (name $ Animal "rosko" ["has-knife"]))

testHasAttrT = TestCase (assertEqual "hasAttr" (True) (hasAttr "has-knife" $ Animal "rosko" ["has-knife"]))
testHasAttrF = TestCase (assertEqual "hasAttr" (False) (hasAttr "has-beard" $ Animal "rosko" ["has-knife"]))

t = [Animal "rosko" ["has-knife"],Animal "rosko2" ["has-beard"]]

testAttrLeft = TestCase (assertEqual "attrLeft" (["has-beard"]) (attrLeft t ["has-knife"] ))

testHas = TestCase (assertEqual "attrLeft" ([Animal "rosko" ["has-knife"]]) ( t `has` "has-knife" ))

runtest = TestList [testAttr,testName,testHasAttrT,testHasAttrF,testAttrLeft,testHas]
