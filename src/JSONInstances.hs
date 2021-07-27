{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module JSONInstances where

import Data.Aeson (ToJSON (..), object, (.=))
import Types

instance ToJSON Type

instance ToJSON Value where
  toJSON (Character c) = object ["type" .= ("character" :: String), "value" .= c]
  toJSON (String s) = object ["type" .= ("string" :: String), "value" .= s]
  toJSON (Atom _ a) = object ["type" .= ("atom" :: String), "value" .= a]
  toJSON (Integer i) = object ["type" .= ("integer" :: String), "value" .= i]
  toJSON (Float f) = object ["type" .= ("float" :: String), "value" .= f]
  toJSON (Bool b) = object ["type" .= ("bool" :: String), "value" .= b]
  toJSON (List _ xs) = object ["type" .= ("list" :: String), "value" .= xs]
  toJSON (Map xs) = object ["type" .= ("map" :: String), "value" .= xs]
  toJSON (Call xs) = object ["type" .= ("call" :: String), "value" .= xs]
  toJSON (DottedList _ head tail) = object ["type" .= ("dotted-list" :: String), "head" .= head, "tail" .= tail]
  toJSON (Func params body closure) = object ["type" .= ("func" :: String), "params" .= params, "body" .= body, "closure" .= closure]
  toJSON (PrimitiveFunc name) = object ["type" .= ("primitive-func" :: String), "value" .= name]
  toJSON (Macro body closure) = object ["type" .= ("macro" :: String), "body" .= body, "closure" .= closure]
  toJSON Unit = object ["type" .= ("unit" :: String)]
  toJSON (Type t) = object ["type" .= ("type" :: String), "value" .= t]

instance ToJSON Error where
  toJSON err = object ["error" .= show err]

instance ToJSON ValueCrumb where
  toJSON (ValueCrumb env ls rs) = object ["type" .= ("crumb" :: String), "env" .= env, "ls" .= ls, "rs" .= rs]
