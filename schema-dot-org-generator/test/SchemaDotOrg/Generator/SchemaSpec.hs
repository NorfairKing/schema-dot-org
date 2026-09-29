{-# LANGUAGE OverloadedStrings #-}

module SchemaDotOrg.Generator.SchemaSpec (spec) where

import Data.Aeson (eitherDecode', object)
import SchemaDotOrg.Generator.Schema
import Test.Syd

spec :: Spec
spec = do
  describe "AllSchemas" $ do
    it "reads a term that schema.org defines" $
      eitherDecode'
        "{\"@context\":{},\"@graph\":[{\"@id\":\"schema:Quantity\",\"@type\":[\"rdfs:Class\",\"schema:DataType\"],\"rdfs:comment\":\"Quantities such as distance.\",\"rdfs:label\":\"Quantity\"}]}"
        `shouldBe` Right
          ( AllSchemas
              { allSchemasContext = object [],
                allSchemasGraph =
                  [ GraphEntrySchema
                      Schema
                        { schemaId = "schema:Quantity",
                          schemaType = ["rdfs:Class", "schema:DataType"],
                          schemaComment = CommentText "Quantities such as distance.",
                          schemaLabel = CommentText "Quantity",
                          schemaSubclassOf = [],
                          schemaSubPropertyOf = [],
                          schemaDomainIncludes = [],
                          schemaRangeIncludes = [],
                          schemaIsPartOf = [],
                          schemaSupersededBy = []
                        }
                  ]
              }
          )

    it "reads a term of another vocabulary, which carries no label or comment" $
      eitherDecode'
        "{\"@context\":{},\"@graph\":[{\"@id\":\"bibo:Issue\",\"@type\":\"rdfs:Class\"}]}"
        `shouldBe` Right
          ( AllSchemas
              { allSchemasContext = object [],
                allSchemasGraph = [GraphEntryForeignTerm "bibo:Issue"]
              }
          )

    it "fails on a schema.org term it cannot read, rather than taking it for a foreign one" $
      case eitherDecode'
        "{\"@context\":{},\"@graph\":[{\"@id\":\"schema:Thing\",\"@type\":\"rdfs:Class\"}]}" of
        Left _ -> pure ()
        Right allSchemas ->
          expectationFailure $
            unwords
              [ "Read it anyway, as:",
                show (allSchemas :: AllSchemas)
              ]

  describe "graphEntrySchemas" $
    it "drops the terms of other vocabularies" $
      graphEntrySchemas
        [ GraphEntryForeignTerm "bibo:Issue",
          GraphEntrySchema
            Schema
              { schemaId = "schema:Thing",
                schemaType = ["rdfs:Class"],
                schemaComment = CommentText "The most generic type of item.",
                schemaLabel = CommentText "Thing",
                schemaSubclassOf = [],
                schemaSubPropertyOf = [],
                schemaDomainIncludes = [],
                schemaRangeIncludes = [],
                schemaIsPartOf = [],
                schemaSupersededBy = []
              }
        ]
        `shouldBe` [ Schema
                       { schemaId = "schema:Thing",
                         schemaType = ["rdfs:Class"],
                         schemaComment = CommentText "The most generic type of item.",
                         schemaLabel = CommentText "Thing",
                         schemaSubclassOf = [],
                         schemaSubPropertyOf = [],
                         schemaDomainIncludes = [],
                         schemaRangeIncludes = [],
                         schemaIsPartOf = [],
                         schemaSupersededBy = []
                       }
                   ]
