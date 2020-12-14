{-# LANGUAGE PatternSynonyms #-}

module StringSymbolsTests(
  stringSymbolsTests
  ) where

import Test.HUnit
import StringSymbols (nTupleSectionDelimiters)

x :: Maybe String
x = Just "x"
n :: Maybe String
n = Nothing


stringSymbolsTests :: Test
stringSymbolsTests = TestList[
    TestCase (assertEqual "(x)"       ["(",")" ]              (nTupleSectionDelimiters [x]))      
  -- , TestCase (assertEqual "(,)"       ["(",",",")"]         (nTupleSectionDelimiters [n,n]))      
  -- , TestCase (assertEqual "(x,)"      ["(•,",")"]           (nTupleSectionDelimiters [x,n]))      
  -- , TestCase (assertEqual "(,x)"      ["(",",•)"]           (nTupleSectionDelimiters [n,x]))      
  -- , TestCase (assertEqual "(,,)"      ["(",",",",",")"]     (nTupleSectionDelimiters [n,n,n]))      
  -- , TestCase (assertEqual "(,,x)"     ["(",",",",•)"]       (nTupleSectionDelimiters [n,n,x]))      
  -- , TestCase (assertEqual "(,x,)"     ["(",",•,",")"]       (nTupleSectionDelimiters [n,x,n]))      
  -- , TestCase (assertEqual "(x,,)"     ["(•,",",",")"]       (nTupleSectionDelimiters [x,n,n]))      
  -- , TestCase (assertEqual "(x,x,)"    ["(•,•,",")"]         (nTupleSectionDelimiters [x,x,n]))      
  -- , TestCase (assertEqual "(x,,x)"    ["(•,",",•)"]         (nTupleSectionDelimiters [x,n,x]))      
  -- , TestCase (assertEqual "(,x,x)"    ["(",",•,•)"]         (nTupleSectionDelimiters [n,x,x]))      
  -- , TestCase (assertEqual "(x,x,x)"   ["(•,•,•)"]           (nTupleSectionDelimiters [x,x,x]))      
  , TestCase (assertEqual "(,,,)"     ["(",",",",",",",")"] (nTupleSectionDelimiters [x,x,x,x])) 
  , TestCase (assertEqual "(x,,,)"    ["(•,",",",",",")"  ] (nTupleSectionDelimiters [n,x,x,x])) 
  , TestCase (assertEqual "(,x,,)"    ["(",",•,",",",")"  ] (nTupleSectionDelimiters [x,n,x,x])) 
  , TestCase (assertEqual "(,,x,)"    ["(",",",",•,",")"  ] (nTupleSectionDelimiters [x,x,n,x])) 
  -- , TestCase (assertEqual "(,,,x)"    ["(",",",",",",•)"  ] (nTupleSectionDelimiters [x,x,x,n])) 
  , TestCase (assertEqual "(x,x,,)"   ["(•,•,",",",")"    ] (nTupleSectionDelimiters [n,n,x,x]))
  , TestCase (assertEqual "(x,,x,)"   ["(•,",",•,",")"    ] (nTupleSectionDelimiters [n,x,n,x]))
  -- , TestCase (assertEqual "(x,,,x)"   ["(•,",",",",•)"    ] (nTupleSectionDelimiters [n,x,x,n]))
  , TestCase (assertEqual "(x,x,x,)"  ["(•,•,•,",")"      ] (nTupleSectionDelimiters [n,n,n,x]))
  -- , TestCase (assertEqual "(x,x,,x)"  ["(•,•,",",•)"      ] (nTupleSectionDelimiters [n,n,x,n]))
  , TestCase (assertEqual "(x,x,x,x)" ["(•,•,•,•)"        ] (nTupleSectionDelimiters [n,n,n,n])) 
  ]


  
  
  
  
  
  










