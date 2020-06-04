module UmuHalogen.Capability.Generation.Route
  ( updateRouteModule
  , updateRouteCodec
  ) where

import           Import
-- text
import qualified Data.Text                     as T
import           Text.Casing                   (kebab, pascal)
-- lens
import           Control.Lens.Operators
import           Control.Lens.Prism
import           Control.Lens.Tuple
-- purescript
import           Language.PureScript.CST.Types
import           Language.PureScript.Names     hiding (Ident)
import           Language.PureScript.PSString  (mkString)
-- umu-halogen
import           UmuHalogen.Optics

updateRouteModule :: Module () -> Text -> Module ()
updateRouteModule module_ newRoute =
  module_
    { modDecls =
      ( updateDeclaration
        <$> ( getRoute module_ )
        <*> ( pure $ sepTailUpdate module_ ( isNewLine dataSrcLineHead dataSrcLineTail ) )
      ) <> ( getNotRoute module_ )
    }
  where
    routeDataType :: Text
    routeDataType = "Route"

    getNotRoute :: Module () -> [ Declaration () ]
    getNotRoute = filter (( routeDataType /= ) . matchDataRoute ) . modDecls

    getRoute :: Module () -> [ Declaration () ]
    getRoute = filter (( routeDataType == ) . matchDataRoute ) . modDecls

    sepTailUpdate :: Module () -> LineState -> [( SourceToken, DataCtor () )]
    sepTailUpdate m newLine =
      ( sepTail . snd =<< ( catMaybes $ matchSrcToken <$> modDecls m ) )
      <> ( pure $ mkDataCtor newRoute newLine )

    allTheSame :: Eq a => NonEmpty a -> Bool
    allTheSame xs = and $ fmap ( == head xs ) ( tail xs )

    dataSrcLineTail :: SrcLineTail
    dataSrcLineTail = SrcLineTail $ concat $ matchSrcLineFromDataCtor <$> modDecls module_

    dataSrcLineHead :: SrcLineHead
    dataSrcLineHead = SrcLineHead $ matchSrcLineFromDataHdName <$> modDecls module_

    isNewLine :: SrcLineHead -> SrcLineTail -> LineState
    isNewLine srcLineHead srcLineTail = maybe SameLine boolToLineState
      $ allTheSame
      <$> ( nonEmpty $ removeZeroes $ fromSrcLineHead srcLineHead <> fromSrcLineTail srcLineTail )

    removeZeroes :: [ Int ] -> [ Int ]
    removeZeroes = filter ( 0 /= )

-- | Represents the file line location of the head data constructor
-- e.g data Route = Home | About
--                  ^ head  ^ tail
newtype SrcLineHead = SrcLineHead [ Int ]
  deriving ( Eq, Show )

fromSrcLineHead :: SrcLineHead -> [ Int ]
fromSrcLineHead = coerce

-- | Represents the file line location of the tail data constructors
-- e.g data Route = Home | About
--                  ^ head  ^ tail
newtype SrcLineTail = SrcLineTail [ Int ]
  deriving ( Eq, Show )

fromSrcLineTail :: SrcLineTail -> [ Int ]
fromSrcLineTail = coerce

data LineState
  = NewLine
  | SameLine
  deriving ( Eq, Show )

-- | True will return SameLine, NewLine otherwise.
boolToLineState :: Bool -> LineState
boolToLineState b = if b then SameLine else NewLine

mkDataCtor :: Text -> LineState -> ( SourceToken, DataCtor () )
mkDataCtor newRoute lineState =
  ( SourceToken
    { tokAnn = TokenAnn
      { tokRange = SourceRange
        { srcStart = SourcePos { srcLine = 0, srcColumn = 0 }
        , srcEnd = SourcePos { srcLine = 0, srcColumn = 0 }
        }
        , tokLeadingComments = case lineState of
            SameLine -> [ Space 1 ]
            NewLine  -> [ Line LF, Space 2 ]
        , tokTrailingComments = [ Space 1 ]
      }
    , tokValue  = TokPipe
    }
  , DataCtor
    { dataCtorAnn = ()
    , dataCtorName = Name
      { nameTok = SourceToken
        { tokAnn = TokenAnn
          { tokRange = SourceRange
            { srcStart = SourcePos { srcLine = 0, srcColumn = 0 }
            , srcEnd = SourcePos { srcLine = 0, srcColumn = 0 }
            }
          , tokLeadingComments = []
          , tokTrailingComments = []
          }
        , tokValue = TokUpperName [] newRoute
        }
      , nameValue = ProperName { runProperName = newRoute }
      }
    , dataCtorFields = []
    }
  )

updateDeclaration
  :: Declaration ()
  -> [( SourceToken, DataCtor () )]
  -> Declaration ()
updateDeclaration ( DeclData a b c ) sepTailUpdate =
  DeclData a b ( (\(t, srcToken) -> (t, srcToken { sepTail = sepTailUpdate })) <$> c  )
updateDeclaration dec _ =  dec

matchDataRoute :: Declaration a -> Text
matchDataRoute ( DeclData _ b _ ) = runProperName . nameValue $ dataHdName b
matchDataRoute _                  = mempty

matchSrcToken :: Declaration a -> Maybe ( SourceToken, Separated ( DataCtor a ) )
matchSrcToken ( DeclData _ _ c ) = c
matchSrcToken _                  = Nothing

matchSrcLineFromDataCtor :: Declaration a -> [Int]
matchSrcLineFromDataCtor ( DeclData _ _ c ) = fromMaybe []
  $ (\s -> fmap (\t -> srcLine . srcStart . tokRange . tokAnn . fst $ t) . sepTail . snd $ s )
  <$> c
matchSrcLineFromDataCtor _ = []

matchSrcLineFromDataHdName :: Declaration a -> Int
matchSrcLineFromDataHdName ( DeclData _ b _ ) =
  srcLine . srcEnd . tokRange. tokAnn . nameTok . dataHdName $ b
matchSrcLineFromDataHdName _ = 0

updateRouteCodec :: Text -> Module () -> Module ()
updateRouteCodec routeName module_ =
  module_ { modDecls = ( getNotRouteCodecSignature notRouteCodec ) <> updatedRouteCodec  }
  where
    updatedRouteCodec :: [ Declaration () ]
    updatedRouteCodec = ( getRouteCodecSignature ( modDecls module_ ) ) <> ( mkNewModDecls $ getRouteCodec ( modDecls module_ ) )

    notRouteCodec :: [ Declaration () ]
    notRouteCodec = getNotRouteCodec ( modDecls module_ )

    getRouteCodec :: [ Declaration () ] -> [ Declaration () ]
    getRouteCodec declarations =
      filter (\dec -> dec ^? _DeclValue . _2 . valNameL . nameValueL . getIdentL == Just "routeCodec" ) declarations

    getRouteCodecSignature :: [ Declaration () ] -> [ Declaration () ]
    getRouteCodecSignature = filter (\decl -> Just "routeCodec" == decl ^? _DeclSignature . _2 . lblLabelL . nameValueL . getIdentL )

    getNotRouteCodecSignature :: [ Declaration () ] -> [ Declaration () ]
    getNotRouteCodecSignature = filter (\decl -> Just "routeCodec" /= decl ^? _DeclSignature . _2 . lblLabelL . nameValueL . getIdentL )

    getNotRouteCodec :: [ Declaration () ] -> [ Declaration () ]
    getNotRouteCodec declarations =
      filter (\dec -> dec ^? _DeclValue . _2 . valNameL . nameValueL . getIdentL /= Just "routeCodec" ) declarations

    mkNewModDecls :: [ Declaration () ] -> [ Declaration () ]
    mkNewModDecls declarations = declarations
      <&> (\modul -> modul
          & _DeclValue
          . _2
          . valGuardedL
          . _Unconditional
          . _2
          . whereExprL
          . _ExprOp
          . _4
          . _ExprApp
          . _3
          . _ExprRecord
          . _2
          . wrpValueL
          . _Just
          . sepTailL
          %~ (<> [ mkRouteItem routeName ])
        )

mkRouteItem :: Text -> ( SourceToken, RecordLabeled ( Expr () ))
mkRouteItem newRoute =
  ( SourceToken
    { tokAnn = TokenAnn
      { tokRange = SourceRange
        { srcStart = SourcePos
          { srcLine = 0
          , srcColumn = 0
          }
        , srcEnd = SourcePos
          { srcLine = 0
          , srcColumn = 0
          }
        }
      , tokLeadingComments = [ Line LF , Space 2 ]
      , tokTrailingComments = [ Space 1 ]
      }
    , tokValue = TokComma
    }
  , RecordField
    ( Label
      { lblTok = SourceToken
        { tokAnn = TokenAnn
          { tokRange = SourceRange
            { srcStart = SourcePos
              { srcLine = 0
              , srcColumn = 0
              }
            , srcEnd = SourcePos
              { srcLine = 0
              , srcColumn = 0
              }
            }
          , tokLeadingComments = []
          , tokTrailingComments = []
          }
        , tokValue = TokString
            ( mkNewRoute newRoute )
            ( mkString $ T.pack $ pascal $ T.unpack  newRoute )
        }
      , lblName = mkString $ T.pack $ pascal $ T.unpack newRoute
      }
    )
    ( SourceToken
      { tokAnn = TokenAnn
        { tokRange = SourceRange
          { srcStart = SourcePos
            { srcLine = 0
            , srcColumn = 0
            }
          , srcEnd = SourcePos
            { srcLine = 0
            , srcColumn = 0
            }
          }
        , tokLeadingComments = []
        , tokTrailingComments = [ Space 1 ]
        }
      , tokValue = TokOperator [] ":"
      }
    )
    ( ExprOp ()
      ( ExprString ()
        ( SourceToken
          { tokAnn = TokenAnn
            { tokRange = SourceRange
              { srcStart = SourcePos
                { srcLine = 0
                , srcColumn = 0
                }
              , srcEnd = SourcePos
                { srcLine = 0
                , srcColumn = 0
                }
              }
            , tokLeadingComments = []
            , tokTrailingComments = [ Space 1 ]
            }
          , tokValue = TokString
            ( T.pack $ kebab $ T.unpack $ T.toLower newRoute )
            ( mkString $ T.toLower newRoute )
          }
        ) (  mkString $ T.toLower newRoute )
      )
      ( QualifiedName
        { qualTok = SourceToken
          { tokAnn = TokenAnn
            { tokRange = SourceRange
              { srcStart = SourcePos
                { srcLine = 0
                , srcColumn = 0
                }
              , srcEnd = SourcePos
                { srcLine = 0
                , srcColumn = 0
                }
              }
            , tokLeadingComments = []
            , tokTrailingComments = [ Space 1 ]
            }
          , tokValue = TokOperator [] "/"
          }
        , qualModule = Nothing
        , qualName = OpName { runOpName = "/" }
        }
      )
      ( ExprIdent ()
        ( QualifiedName
          { qualTok = SourceToken
            { tokAnn = TokenAnn
              { tokRange = SourceRange
                { srcStart = SourcePos
                  { srcLine = 0
                  , srcColumn = 0
                  }
                , srcEnd = SourcePos
                  { srcLine = 0
                  , srcColumn = 0
                  }
                }
              , tokLeadingComments = []
              , tokTrailingComments =
                  [ Space 1
                  ]
              }
            , tokValue = TokLowerName [] "noArgs"
            }
          , qualModule = Nothing
          , qualName = Ident { getIdent = "noArgs" }
          }
        )
      )
    )
  )
  where
    mkNewRoute :: Text -> Text
    mkNewRoute = T.pack . pascal . T.unpack
