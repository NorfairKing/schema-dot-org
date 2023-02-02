{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Generate the code for a gives schemas.json file
--
-- We generate the following code in `schema-dot-org/src/SchemaDotOrg/Generated.hs`:
--
-- * For a class `FooBar`:
--
-- > data FooBar
--
-- @FooBar@ is called 'classTypeName'.
--
-- > classFooBar :: Class FooBar [FooBarSuperClass, FooBarSuperClassSuperClass]
-- > classFooBar = Class "FooBar"
--
-- @classFooBar@ is called 'classValueName'.
-- @"FooBar"@ is called 'classNameValue'.
--
-- * For a property `fooBar` of a class `Quux`:
--
-- > propertyQuuxFooBar :: Property Quux '[QuuxSuperClass, QuuxSuperClassSuperClass]
-- > propertyQuuxFooBar = Property "fooBar"
--
-- @propertyQuuxFooBar@ is called 'propertyValueName'.
-- @"fooBar"@ is called 'propertyNameValue'.
--
-- * For an enumeration `FooBar`:
--
-- > data FooBarEnumeration
-- >   = FooBarEnumerationValue1
-- >   | FooBarEnumerationValue2
--
-- @FooBarEnumeration@ is called 'enumerationTypeName'.
-- @FooBarEnumerationValue1@ is called 'enumerationConstructorName'.
--
-- > instance FromJSON FooBarEnumeration where
-- >    parseJSON = withText "FooBarEnumeration"
-- >      ( \case
-- >        "https://schema.org/FooBarEnumerationValue1" -> pure FooBarEnumerationValue1
-- >        "https://schema.org/FooBarEnumerationValue2" -> pure FooBarEnumerationValue2
-- >        t -> fail ("Failed to parse FooBarEnumeration: " <> show t
-- >      )
--
-- @"FooBarEnumeration"@ is called `enumerationTypeNameValue` and is also used in the error.
-- @"https://schema.org/FooBarEnumerationValue1"@ is called `enumConstructorSerialisation`.
--
-- > instance ToJSON FooBarEnumeration where
-- >    toJSON = (toJSON :: Text -> Value)
-- >      . ( \case
-- >          FooBarEnumerationValue1 -> "https://schema.org/FooBarEnumerationValue1"
-- >          FooBarEnumerationValue2 -> "https://schema.org/FooBarEnumerationValue2"
-- >        )
module SchemaDotOrg.Generator.Code (generateCodeFor) where

import qualified Data.ByteString as SB
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC (runGhc)
import GHC.Driver.Session (getDynFlags)
import qualified GHC.Paths as GHC (libdir)
import GHC.SourceGen
import SchemaDotOrg.Generator.Schema
import System.Process.Typed

generateCodeFor :: Map Text Schema -> IO ()
generateCodeFor schemaMap = do
  dynFlags <- runGhc (Just GHC.libdir) getDynFlags
  let code =
        showPpr dynFlags $
          module'
            (Just "SchemaDotOrg.Generated")
            Nothing
            [ import' "GHC.Generics" `exposing` [var "Generic"],
              import' "Data.Text" `exposing` [var "Text"],
              import' "Data.Aeson" `exposing` [thingAll "FromJSON", thingAll "ToJSON", bvar "withText", bvar "Value"],
              import' "SchemaDotOrg.Schema"
            ]
            $ concatMap (declsFor schemaMap) (M.elems schemaMap)

  let moduleHeader =
        unlines
          [ "{-# LANGUAGE DeriveGeneric #-}",
            "{-# LANGUAGE EmptyDataDeriving #-}",
            "{-# LANGUAGE OverloadedStrings #-}",
            "{-# LANGUAGE DataKinds #-}",
            "{-# LANGUAGE LambdaCase #-}"
          ]
  let moduleCodec = unlines [moduleHeader, code]

  let moduleFile = "schema-dot-org/src/SchemaDotOrg/Generated.hs"
  SB.writeFile moduleFile $ TE.encodeUtf8 $ T.pack moduleCodec
  runProcess_ $ proc "ormolu" ["-i", moduleFile]

declsFor :: Map Text Schema -> Schema -> [HsDecl']
declsFor schemaMap s@Schema {..}
  | subclassOfEnumeration schemaMap s = declsForEnumeration schemaMap s
  | "rdfs:Class" `elem` schemaType = declsForClass schemaMap s
  | "rdf:Property" `elem` schemaType = declsForProperty s
  | otherwise = []

subclassOfEnumeration :: Map Text Schema -> Schema -> Bool
subclassOfEnumeration schemaMap s = "schema:Enumeration" `elem` transitiveSuperclasses schemaMap s

transitiveSuperclasses :: Map Text Schema -> Schema -> [Text]
transitiveSuperclasses schemaMap schema = go (map unSchemaRef (schemaSubclassOf schema))
  where
    go :: [Text] -> [Text]
    go superClasses =
      superClasses
        ++ let superSuperClasses t = case M.lookup t schemaMap of
                 Nothing -> []
                 Just s -> go (map unSchemaRef (schemaSubclassOf s))
            in concatMap superSuperClasses superClasses

toPascalCase :: String -> String
toPascalCase = \case
  [] -> []
  (c : cs) -> toUpper c : cs

declsForClass :: Map Text Schema -> Schema -> [HsDecl']
declsForClass schemaMap schema =
  let classTypeNameString = schemaTypeNameString schema
      classTypeName = fromString classTypeNameString
      valueTypeName = fromString $ "class" <> classTypeNameString
      superClasses = transitiveSuperclasses schemaMap schema
      superClassTypeNames = mapMaybe (fmap (toTypeName . T.unpack) . T.stripPrefix "schema:") superClasses
      primitiveTypes = ["Text"]
   in case find (\primitiveType -> schemaSubclassOf schema == [SchemaRef ("schema:" <> primitiveType)]) primitiveTypes of
        Just primitiveType -> [type' classTypeName [] (var (fromString (T.unpack primitiveType)))]
        Nothing ->
          if "schema:DataType" `elem` schemaType schema
            || commentText (schemaLabel schema) `elem` ["Class", "Property", "Integer", "Float"]
            then []
            else
              [ data' classTypeName [] [] [],
                typeSig valueTypeName (bvar "Class" @@ bvar classTypeName @@ listPromotedTy (map (bvar . fromString) superClassTypeNames)),
                funBind valueTypeName (match [] (var "Class" @@ string (T.unpack (commentText (schemaLabel schema)))))
              ]

declsForProperty :: Schema -> [HsDecl']
declsForProperty schema =
  let propertyLabel = T.unpack (commentText (schemaLabel schema))
      valueTypeName className = fromString $ "property" <> toPascalCase className <> toPascalCase propertyLabel
   in if SchemaRef "https://meta.schema.org" `elem` schemaIsPartOf schema
        then []
        else
          concatMap
            ( \(SchemaRef ref) -> case T.unpack <$> T.stripPrefix "schema:" ref of
                Nothing -> []
                Just className ->
                  let typeName = valueTypeName className
                      rangeTypeNames = mapMaybe (fmap (toTypeName . T.unpack) . T.stripPrefix "schema:" . unSchemaRef) (schemaRangeIncludes schema)
                   in [ typeSig typeName (bvar "Property" @@ bvar (fromString (toTypeName className)) @@ listPromotedTy (map (bvar . fromString) rangeTypeNames)),
                        funBind typeName (match [] (bvar "Property" @@ string propertyLabel))
                      ]
            )
            (schemaDomainIncludes schema)

declsForEnumeration :: Map Text Schema -> Schema -> [HsDecl']
declsForEnumeration schemaMap schema =
  let enumerationElems = M.elems $ M.filter ((schemaId schema `elem`) . schemaType) schemaMap

      elemConstructorName s = schemaTypeNameString schema <> schemaTypeNameString s
      elemConstructor s = prefixCon (fromString (elemConstructorName s)) []

      enumLiteral s = string ("https://schema.org/" <> schemaTypeNameString s)

      enumTypeName = schemaTypeName schema
   in if null enumerationElems
        then [type' enumTypeName [] (var "Text")]
        else
          [ data'
              enumTypeName
              []
              (map elemConstructor enumerationElems)
              [ deriving'
                  [ var "Show",
                    var "Eq",
                    var "Ord",
                    var "Generic"
                  ]
              ],
            instance'
              (var "FromJSON" @@ bvar enumTypeName)
              [ funBinds
                  "parseJSON"
                  [ match [] $
                      (var "withText" @@ string (schemaTypeNameString schema))
                        @@ lambdaCase
                          ( map
                              ( \s ->
                                  match
                                    [enumLiteral s]
                                    (var "pure" @@ bvar (fromString (elemConstructorName s)))
                              )
                              enumerationElems
                              ++ [ match
                                     [bvar "t"]
                                     ( var "fail"
                                         @@ op
                                           ( string
                                               ( "Failed to parse "
                                                   ++ schemaTypeNameString schema
                                                   ++ ": "
                                               )
                                           )
                                           "<>"
                                           (var "show" @@ bvar "t")
                                     )
                                 ]
                          )
                  ]
              ],
            instance'
              (var "ToJSON" @@ bvar enumTypeName)
              [ funBinds
                  "toJSON"
                  [ match [] $
                      op (var "toJSON" @::@ (var "Text" GHC.SourceGen.--> var "Value")) "." $
                        lambdaCase $
                          map
                            (\s -> match [bvar (fromString (elemConstructorName s))] (enumLiteral s))
                            enumerationElems
                  ]
              ]
          ]

schemaTypeName :: Schema -> OccNameStr
schemaTypeName = fromString . schemaTypeNameString

schemaTypeNameString :: Schema -> String
schemaTypeNameString = toTypeName . T.unpack . commentText . schemaLabel

commentText :: Comment -> Text
commentText = \case
  CommentText t -> t
  CommentTextInLang _ t -> t

toTypeName :: String -> String
toTypeName = dropWhile isDigit
