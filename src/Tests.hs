module Tests where

import LispLanguage

import Control.Applicative
import Test.QuickCheck

instance Arbitrary LispValue where
      arbitrary = oneof [ Atom <$> arbitrary -- TODO: Arbitrary strings are a bit too arbitrary. Write a proper generator!
                        , Bool <$> arbitrary
                        , List <$> arbitrary
                        , List' <$> arbitrary <*> arbitrary
                        , Number <$> arbitrary
                        , String <$> arbitrary
                        ]