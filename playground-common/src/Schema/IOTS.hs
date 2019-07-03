{-# LANGUAGE OverloadedStrings #-}

module Schema.IOTS
    ( export
    ) where

import           Data.Text                    (Text)
import           Schema                       (Constructor (Constructor, Record), ConstructorName (ConstructorName),
                                               DataType (DataType), Reference (Reference), ToSchema, ToTypeName,
                                               TypeName (TypeName), toSchema)
import           Text.PrettyPrint.Leijen.Text (Doc, braces, indent, linebreak, parens, punctuate, textStrict, vsep,
                                               (<+>))

export :: ToSchema a => a -> Doc
export x = vsep [preamble, linebreak, exportDataType (toSchema x)]

preamble :: Doc
preamble = "import * as t from 'io-ts'"

exportDataType :: DataType -> Doc
exportDataType (DataType (TypeName moduleName typeName) parameters [Record constructorName fields]) =
    vsep
        [ "//" <+> textStrict moduleName
        , "const" <+>
          textStrict typeName <+>
          "=" <+>
          "t.type" <>
          parens
              (braces
                   (linebreak <>
                    indent 2 (vsep (punctuate "," (foo <$> fields))) <>
                    linebreak))
        ]
  where
    foo :: (Text, Reference) -> Doc
    foo (fieldName, Reference ref) =
        textStrict fieldName <> ":" <+> exportTypeName ref

exportTypeName :: TypeName -> Doc
exportTypeName (TypeName "" "Int")    = "t.number"
exportTypeName (TypeName "" "String") = "t.string"
