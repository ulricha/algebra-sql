-- This file determines the semantics of the 'Query' data structure and all of
-- its sub structures.
module Database.Algebra.SQL.Render.Query
    ( renderQuery
    , renderSelectStmt
    ) where

import Text.PrettyPrint.ANSI.Leijen ( (<$>)
                                    , (<+>)
                                    , (</>)
                                    , (<>)
                                    , Doc
                                    , align
                                    , bold
                                    , char
                                    , comma
                                    , double
                                    , empty
                                    , fillSep
                                    , float
                                    , hang
                                    , hsep
                                    , indent
                                    , int
                                    , integer
                                    , linebreak
                                    , lparen
                                    , ondullblue
                                    , parens
                                    , punctuate
                                    , red
                                    , rparen
                                    , sep
                                    , squotes
                                    , text
                                    )

import Database.Algebra.SQL.Query
import Database.Algebra.SQL.Compatibility

enlist :: [Doc] -> Doc
enlist = fillSep . punctuate comma

-- Does the same as enlist but does not break the line.
enlistOnLine :: [Doc] -> Doc
enlistOnLine = hsep . punctuate comma

-- | A keyword.
kw :: String -> Doc
kw = red . text

-- | A single character keyword.
op :: Char -> Doc
op = red . char

-- | Terminate a SQL query.
terminate :: Doc -> Doc
terminate = (<> op ';')

renderQuery :: CompatMode -> Query -> Doc
renderQuery c query = terminate $ case query of
    QValueQuery q      -> renderValueQuery c q
    QDefinitionQuery q -> renderDefinitionQuery c q

renderDefinitionQuery :: CompatMode -> DefinitionQuery -> Doc
renderDefinitionQuery compat (DQMatView query name)        =
    kw "CREATE MATERIALIZED VIEW"
    <+> text name
    <+> kw "AS"
    </> renderValueQuery compat query

renderDefinitionQuery compat (DQTemporaryTable query name) =
    createStmt
    <+>
    case compat of
        SQL99      ->
            as
            <$> indentedQuery
            -- Create the table with the result of the given value query.
            <$> kw "WITH DATA ON COMMIT DROP"
        MonetDB -> 
            as
            <$> indentedQuery
            -- Create the table with the result of the given value query.
            <$> kw "WITH DATA ON COMMIT DROP"
        PostgreSQL ->
            -- PostgreSQL does not accept the default syntax. In order to
            -- achieve the same behaviour, the SQL code is rendered differently.
            kw "ON COMMIT DROP"
            <+> as
            <$> indentedQuery
  where
    createStmt    = kw "CREATE LOCAL TEMPORARY TABLE" <+> text name
    as            = kw "AS"
    indentedQuery = indent 4 $ renderValueQuery compat query

renderValueQuery :: CompatMode -> ValueQuery -> Doc
renderValueQuery compat (VQSelect stmt)    = renderSelectStmt compat stmt
renderValueQuery compat (VQLiteral vals)   =
    kw "VALUES" <+> align (sep . punctuate comma $ map renderRow vals)
  where renderRow row = parens . enlistOnLine $ map (renderColumnExpr compat) row

renderValueQuery compat (VQWith bindings body)                 =
    hang 4 (kw "WITH" </> enlist (map renderBinding bindings))
    <$> renderValueQuery compat body
  where renderBinding :: (String, Maybe [String], ValueQuery) -> Doc
        renderBinding (name, optCols, query) =
            text name
            <> renderOptColDefs optCols
            <+> kw "AS"
            <+> lparen
            <$> indent 4 (renderValueQuery compat query)
            <$> rparen

renderValueQuery compat (VQBinarySetOperation left right o)    =
    renderValueQuery compat left
    <> linebreak
    <$> renderSetOperation o
    <> linebreak
    <$> renderValueQuery compat right

renderSetOperation :: SetOperation -> Doc
renderSetOperation SOUnionAll  = kw "UNION ALL"
renderSetOperation SOExceptAll = kw "EXCEPT ALL"

renderSelectStmt :: CompatMode -> SelectStmt -> Doc
renderSelectStmt compat stmt =
    kw "SELECT"
    <+> align ( let sC = enlist $ map (renderSelectColumn compat) $ selectClause stmt
                in if distinct stmt
                   then kw "DISTINCT" <+> sC
                   else sC
              )
               
    <> case fromClause stmt of
           []        -> empty
           fromParts ->
               linebreak
               <> kw "FROM"
               <+> align (enlist $ map (renderFromPart compat) fromParts)
    <> case whereClause stmt of
           Just valExpr -> linebreak
                           <> kw "WHERE"
                           <+> align ((renderColumnExpr compat) valExpr)
           Nothing      -> empty
    <> case groupByClause stmt of
           []      -> empty
           valExpr -> linebreak
                      <> kw "GROUP BY"
                      <+> align (enlist $ map (renderColumnExpr compat) valExpr)
    <> case orderByClause stmt of
           []    -> empty
           order -> linebreak
                    <> kw "ORDER BY"
                    <+> align (renderOrderExprList compat order)


renderOrderExpr :: CompatMode -> OrderExpr -> Doc
renderOrderExpr compat (OE expr dir) = renderAdvancedExpr compat expr <+> renderSortDirection dir

-- | Render a list of order expressions.
renderOrderExprList :: CompatMode -> [OrderExpr] -> Doc
renderOrderExprList compat = enlistOnLine . map (renderOrderExpr compat)

renderSortDirection :: SortDirection -> Doc
renderSortDirection Ascending  = kw "ASC"
renderSortDirection Descending = kw "DESC"

-- | Render a list of columns as definition within a from clause or within
-- a common table expression.
renderOptColDefs :: Maybe [String] -> Doc
renderOptColDefs = maybe empty colDoc
  where colDoc = parens . enlistOnLine . map text

renderFromPart :: CompatMode -> FromPart -> Doc
renderFromPart _ (FPAlias (FETableReference n) alias _) =
    -- Don't use positional mapping on table references, since they are mapped
    -- by their name.
    text n
    <+> kw "AS"
    <+> text alias

renderFromPart compat (FPAlias expr alias optCols)           =
    renderFromExpr compat expr
    <+> kw "AS"
    <+> text alias
    <> renderOptColDefs optCols

renderSubQuery :: CompatMode -> ValueQuery -> Doc
renderSubQuery compat q = lparen <+> align (renderValueQuery compat q) <$> rparen

renderFromExpr :: CompatMode -> FromExpr -> Doc
renderFromExpr compat (FESubQuery q)       = renderSubQuery compat q
renderFromExpr _      (FEVariable v)       = ondullblue $ int v
renderFromExpr _      (FETableReference n) = text n

-- | Renders an optional prefix.
renderOptPrefix :: Maybe String -> Doc
renderOptPrefix = maybe empty $ (<> char '.') . text


renderSelectColumn :: CompatMode -> SelectColumn -> Doc
renderSelectColumn compat (SCAlias expr name) = renderAdvancedExpr compat expr
                                                <+> kw "AS"
                                                <+> text name

renderAdvancedExpr :: CompatMode -> AdvancedExpr -> Doc
renderAdvancedExpr compat (AEBase v)                  = renderAdvancedExprBase compat v
renderAdvancedExpr compat (AERowNum partColumn order) =
    kw "ROW_NUMBER() OVER"
    <+> parens (partitionByDoc 
                <>
                case order of
                    [] -> empty
                    _  -> kw "ORDER BY" <+> renderOrderExprList compat order)
  where partitionByDoc = maybe empty
                               (\c -> kw "PARTITION BY"
                                      </> renderAdvancedExpr compat c
                                      <> linebreak)
                               partColumn

renderAdvancedExpr compat (AEDenseRank order)         = renderRank compat "DENSE_RANK() OVER"
                                                          order
renderAdvancedExpr compat (AERank order)              = renderRank compat "RANK() OVER" order

renderAdvancedExpr compat (AEAggregate optVE aggr)    =
    renderAggregateFunction compat aggr
    <> parens (maybe (char '*') (renderColumnExpr compat) optVE)

-- | Generic VEBaseTemplate renderer.
renderValueExprTemplate :: (CompatMode -> a -> Doc)
                        -> CompatMode
                        -> ValueExprTemplate a
                        -> Doc
renderValueExprTemplate renderRec compat e = case e of
    VEValue v            -> renderValue v
    VEColumn n optPrefix -> renderOptPrefix optPrefix
                            <> text n
    VECast v ty          ->
        kw "CAST" <> parens castDoc
      where castDoc = renderRec compat v
                      <+> kw "AS" <+> renderDataType ty

    VEBinApp f a b       -> parens $ renderRec compat a
                            <+> renderBinaryFunction f
                            <+> renderRec compat b

    VEUnApp f a          ->
        parens $ renderUnaryFunction f <> parens (renderRec compat a)

    VENot a              -> parens $ kw "NOT" <+> renderRec compat a
    VEExists q           -> kw "EXISTS" <+> renderSubQuery compat q

    VEIn v q             -> parens $ renderRec compat v
                            <+> kw "IN"
                            <+> renderSubQuery compat q

-- | Render a VEBaseTemplate specialized to a AdvancedExpr.
renderAdvancedExprBase :: CompatMode -> AdvancedExprBase -> Doc
renderAdvancedExprBase = renderValueExprTemplate renderAdvancedExpr
    

-- | Render a VEBaseTemplate specialized to a AdvancedExpr.
renderColumnExprBase :: CompatMode -> ColumnExprBase -> Doc
renderColumnExprBase = renderValueExprTemplate renderColumnExpr


-- | Render the postfix part of a rank operator.
renderRank :: CompatMode -> String -> [OrderExpr] -> Doc
renderRank compat prefix order =
    kw prefix <+> parens (kw "ORDER BY" <+> renderOrderExprList compat order)

renderAggregateFunction :: CompatMode -> AggregateFunction -> Doc
renderAggregateFunction _          AFAvg   = kw "AVG"
renderAggregateFunction _          AFMax   = kw "MAX"
renderAggregateFunction _          AFMin   = kw "MIN"
renderAggregateFunction _          AFSum   = kw "SUM"
renderAggregateFunction _          AFCount = kw "COUNT"
renderAggregateFunction PostgreSQL AFAll   = kw "BOOL_AND"
renderAggregateFunction SQL99      AFAll   = kw "EVERY"
renderAggregateFunction MonetDB    AFAll   = kw "MIN"
renderAggregateFunction PostgreSQL AFAny   = kw "BOOL_OR"
renderAggregateFunction SQL99      AFAny   = kw "SOME"
renderAggregateFunction MonetDB    AFAny   = kw "MAX"

renderColumnExpr :: CompatMode -> ColumnExpr -> Doc
renderColumnExpr compat (CEBase e) = renderColumnExprBase compat e


renderBinaryFunction :: BinaryFunction -> Doc
renderBinaryFunction BFPlus         = op '+'
renderBinaryFunction BFMinus        = op '-'
renderBinaryFunction BFTimes        = op '*'
renderBinaryFunction BFDiv          = op '/'
renderBinaryFunction BFModulo       = op '%'
renderBinaryFunction BFContains     = op '~'
renderBinaryFunction BFSimilarTo    = kw "SIMILAR TO"
renderBinaryFunction BFLike         = kw "LIKE"
renderBinaryFunction BFConcat       = kw "||"
renderBinaryFunction BFGreaterThan  = op '>'
renderBinaryFunction BFGreaterEqual = kw ">="
renderBinaryFunction BFLowerThan    = op '<'
renderBinaryFunction BFLowerEqual   = kw "<="
renderBinaryFunction BFEqual        = op '='
renderBinaryFunction BFNotEqual     = kw "<>"
renderBinaryFunction BFAnd          = kw "AND"
renderBinaryFunction BFOr           = kw "OR"

renderUnaryFunction :: UnaryFunction -> Doc
renderUnaryFunction UFSin  = kw "sin"
renderUnaryFunction UFCos  = kw "cos"
renderUnaryFunction UFTan  = kw "tan"
renderUnaryFunction UFLog  = kw "log"
renderUnaryFunction UFSqrt = kw "sqrt"
renderUnaryFunction UFExp  = kw "exp"
renderUnaryFunction UFASin = kw "asin"
renderUnaryFunction UFACos = kw "acos"
renderUnaryFunction UFATan = kw "atan"

renderDataType :: DataType -> Doc
renderDataType DTInteger         = kw "INTEGER"
renderDataType DTDecimal         = kw "DECIMAL"
renderDataType DTDoublePrecision = kw "DOUBLE PRECISION"
renderDataType DTText            = kw "TEXT"
renderDataType DTBoolean         = kw "BOOLEAN" 

literal :: Doc -> Doc
literal = bold

renderValue :: Value -> Doc
renderValue v = case v of
    VInteger i         -> literal $ integer i
    VDecimal d         -> literal $ float d
    VDoublePrecision d -> literal $ double d
    VText str          -> literal $ squotes $ text str
    VBoolean b         -> kw $ if b then "TRUE" else "FALSE"
    VNull              -> literal $ text "null"

