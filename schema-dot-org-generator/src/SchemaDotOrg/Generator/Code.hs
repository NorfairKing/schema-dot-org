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
-- @"https://schema.org/FooBarEnumerationValue1"@ is called `enumerationConstructorSerialisation`.
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
  | isMetaType s = []
  | isPrimitiveType s = []
  | isPrimitiveTypeValueSchema schemaMap s = []
  | isTextSynonym s = declsForTextSynonym s
  | subclassOfEnumeration schemaMap s = declsForEnumeration schemaMap s
  | isEnumerationValueSchema schemaMap s = []
  | "rdfs:Class" `elem` schemaType = declsForClass schemaMap s
  | "rdf:Property" `elem` schemaType = declsForProperty schemaMap s
  | otherwise =
    error $
      unlines
        [ "Found a schema that we don't know how to deal with:",
          show s
        ]

subclassOfEnumeration :: Map Text Schema -> Schema -> Bool
subclassOfEnumeration schemaMap s = any ((== "schema:Enumeration") . schemaId) (transitiveSuperclasses schemaMap s)

isEnumerationValueSchema :: Map Text Schema -> Schema -> Bool
isEnumerationValueSchema schemaMap s =
  any
    (subclassOfEnumeration schemaMap)
    (mapMaybe (`M.lookup` schemaMap) (schemaType s))

transitiveSuperclasses :: Map Text Schema -> Schema -> [Schema]
transitiveSuperclasses schemaMap schema = go (schemaSubclassOf schema)
  where
    go :: [SchemaRef] -> [Schema]
    go superClassRefs =
      let superClasses = mapMaybe ((`M.lookup` schemaMap) . unSchemaRef) superClassRefs
       in superClasses
            ++ let superSuperClasses superClass = go (schemaSubclassOf superClass)
                in concatMap superSuperClasses superClasses

toPascalCase :: String -> String
toPascalCase = \case
  [] -> []
  (c : cs) -> toUpper c : cs

isMetaType :: Schema -> Bool
isMetaType s = schemaLabelString s `elem` ["Property", "Class"]

isPrimitiveType :: Schema -> Bool
isPrimitiveType s =
  let types = schemaType s
   in or
        [ "schema:DataType" `elem` types,
          schemaLabelString s `elem` ["Integer", "Float"]
        ]

isPrimitiveTypeValueSchema :: Map Text Schema -> Schema -> Bool
isPrimitiveTypeValueSchema schemaMap schema =
  any isPrimitiveType (mapMaybe (`M.lookup` schemaMap) (schemaType schema))

isTextSynonym :: Schema -> Bool
isTextSynonym = ("schema:Text" `elem`) . schemaSubclassOf

declsForTextSynonym :: Schema -> [HsDecl']
declsForTextSynonym schema = [type' (classTypeName schema) [] (var "Text")]

declsForClass :: Map Text Schema -> Schema -> [HsDecl']
declsForClass schemaMap schema =
  let typeName = classTypeName schema
      valueName = classValueName schema
      superClasses = transitiveSuperclasses schemaMap schema
      superClassTypeNames = map classTypeName superClasses
   in [ data' typeName [] [] [],
        typeSig valueName (bvar "Class" @@ bvar typeName @@ listPromotedTy (map bvar superClassTypeNames)),
        funBind valueName (match [] (var "Class" @@ classNameValue schema))
      ]

declsForProperty :: Map Text Schema -> Schema -> [HsDecl']
declsForProperty schemaMap schema =
  let valueTypeName clazz = propertyValueName clazz schema
      rangeClasses = mapMaybe ((`M.lookup` schemaMap) . unSchemaRef) (schemaRangeIncludes schema)
      domainClasses = mapMaybe ((`M.lookup` schemaMap) . unSchemaRef) (schemaDomainIncludes schema)
   in if SchemaRef "https://meta.schema.org" `elem` schemaIsPartOf schema
        then []
        else
          concatMap
            ( \clazz ->
                let typeName = valueTypeName clazz
                    rangeTypeNames = map classTypeName rangeClasses
                 in [ typeSig
                        typeName
                        ( bvar "Property"
                            @@ bvar (classTypeName clazz)
                            @@ listPromotedTy (map bvar rangeTypeNames)
                        ),
                      funBind
                        typeName
                        ( match
                            []
                            (bvar "Property" @@ propertyNameValue schema)
                        )
                    ]
            )
            domainClasses

declsForEnumeration :: Map Text Schema -> Schema -> [HsDecl']
declsForEnumeration schemaMap schema =
  let enumerationElems = M.elems $ M.filter ((schemaId schema `elem`) . schemaType) schemaMap

      enumTypeName = enumerationTypeName schema
   in if null enumerationElems
        then [type' enumTypeName [] (var "Text")]
        else
          [ data'
              enumTypeName
              []
              (map (enumerationConstructorDecl schema) enumerationElems)
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
                      (var "withText" @@ enumerationTypeNameValue schema)
                        @@ lambdaCase
                          ( map
                              ( \s ->
                                  match
                                    [enumerationConstructorSerialisationPattern s]
                                    (var "pure" @@ enumerationConstructorExpression schema s)
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
                            ( \s ->
                                match
                                  [enumerationConstructorPattern schema s]
                                  (enumerationConstructorSerialisationExpression s)
                            )
                            enumerationElems
                  ]
              ]
          ]

-- | The @FooBar@ in
--
-- > data FooBar
classTypeName :: Schema -> OccNameStr
classTypeName = fromString . classTypeNameString

-- | The @FooBar@ 'String' in
--
-- > data FooBar
classTypeNameString :: Schema -> String
classTypeNameString = schemaTypeNameString

-- | The @classFooBar@ in
--
-- > classFooBar = Class "FooBar"
classValueName :: Schema -> OccNameStr
classValueName schema = fromString $ "class" <> classTypeNameString schema

-- | The @"FooBar"@ in
--
-- > classFooBar = Class "FooBar"
classNameValue :: Schema -> HsExpr'
classNameValue = string . classTypeNameString

-- | The @propertyQuuxFooBar@ in
--
-- > propertyQuuxFooBar = Property "fooBar"
propertyValueName :: Schema -> Schema -> OccNameStr
propertyValueName super sub =
  fromString $
    mconcat
      [ "property",
        toPascalCase (classTypeNameString super),
        toPascalCase (schemaLabelString sub)
      ]

-- | The @"fooBar"@ in
--
-- > propertyQuuxFooBar = Property "fooBar"
propertyNameValue :: Schema -> HsExpr'
propertyNameValue = string . schemaLabelString

-- | The @"FooBarEnumeration"@ in
--
-- > instance FromJSON FooBarEnumeration where
-- >    parseJSON = withText "FooBarEnumeration"
enumerationTypeNameValue :: Schema -> HsExpr'
enumerationTypeNameValue = string . schemaTypeNameString

-- | The @FooBarEnumeration@ in
--
-- > data FooBarEnumeration
-- >   = FooBarEnumerationValue1
-- >   | FooBarEnumerationValue2
enumerationTypeName :: Schema -> OccNameStr
enumerationTypeName = fromString . schemaTypeNameString

-- | The @FooBarEnumerationValue1@ in
--
-- >  "https://schema.org/FooBarEnumerationValue1" -> pure FooBarEnumerationValue1
enumerationConstructorExpression :: Schema -> Schema -> HsExpr'
enumerationConstructorExpression super sub = bvar $ fromString $ enumerationConstructorName super sub

-- | The @FooBarEnumerationValue1@ in
--
-- >  FooBarEnumerationValue1 -> "https://schema.org/FooBarEnumerationValue1"
enumerationConstructorPattern :: Schema -> Schema -> Pat'
enumerationConstructorPattern super sub = bvar $ fromString $ enumerationConstructorName super sub

-- | The @FooBarEnumerationValue1@ in
--
-- > data FooBarEnumeration
-- >   = FooBarEnumerationValue1
-- >   | FooBarEnumerationValue2
enumerationConstructorDecl :: Schema -> Schema -> ConDecl'
enumerationConstructorDecl super sub = prefixCon (fromString (enumerationConstructorName super sub)) []

-- | The @FooBarEnumerationValue1@ in
--
-- > data FooBarEnumeration
-- >   = FooBarEnumerationValue1
-- >   | FooBarEnumerationValue2
enumerationConstructorName :: Schema -> Schema -> String
enumerationConstructorName super sub = schemaTypeNameString super <> schemaTypeNameString sub

-- | The @"https://schema.org/FooBarEnumerationValue1"@ in
--
-- >  FooBarEnumerationValue1 -> "https://schema.org/FooBarEnumerationValue1"
enumerationConstructorSerialisationExpression :: Schema -> HsExpr'
enumerationConstructorSerialisationExpression = string . enumerationConstructorSerialisation

-- | The @"https://schema.org/FooBarEnumerationValue1"@ in
--
-- >  "https://schema.org/FooBarEnumerationValue1" -> pure FooBarEnumerationValue1
enumerationConstructorSerialisationPattern :: Schema -> Pat'
enumerationConstructorSerialisationPattern = string . enumerationConstructorSerialisation

-- | The @https://schema.org/FooBarEnumerationValue1@ in
--
-- > "https://schema.org/FooBarEnumerationValue1"
enumerationConstructorSerialisation :: Schema -> String
enumerationConstructorSerialisation s =
  "https://schema.org/" <> schemaLabelString s

schemaTypeNameString :: Schema -> String
schemaTypeNameString = toTypeName . schemaLabelString

toTypeName :: String -> String
toTypeName = dropWhile isDigit

schemaLabelString :: Schema -> String
schemaLabelString = T.unpack . commentText . schemaLabel

commentText :: Comment -> Text
commentText = \case
  CommentText t -> t
  CommentTextInLang _ t -> t
