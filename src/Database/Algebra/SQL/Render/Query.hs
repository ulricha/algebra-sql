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
                                    , vcat
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
        PostgreSQL ->
            -- PostgreSQL does not accept the default syntax. In order to
            -- achieve the same behaviour, the SQL code is rendered differently.
            kw "ON COMMIT DROP"
            <+> as
            <$> indentedQuery

        -- Default implementation for SQL:1999 compliant DBMS. 
        _          ->
            as
            <$> indentedQuery
            -- Create the table with the result of the given value query.
            <$> kw "WITH DATA ON COMMIT DROP"
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

-- | Render a conjunction list, renders the neutral element, when given the
-- empty list.
renderAndList :: CompatMode -> [ColumnExpr] -> Doc
renderAndList compat l = case l of
    [] -> kw "TRUE"
    _  -> align $ hsep $ punctuate (linebreak <> kw "AND")
                         $ map (renderColumnExpr compat) l

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
               <+> align ( vcat . punctuate comma
                                $ map (renderFromPart compat) fromParts
                         )
    <> case whereClause stmt of
           []             -> empty
           l              -> linebreak
                             <> kw "WHERE"
                             <+> renderAndList compat l
    <> case groupByClause stmt of
           []      -> empty
           valExpr -> linebreak
                      <> kw "GROUP BY"
                      <+> align (enlist $ map (renderColumnExpr compat) valExpr)
    <> case orderByClause stmt of
           []    -> empty
           order -> linebreak
                    <> kw "ORDER BY"
                    <+> align ( renderGenericOrderByList renderOrderExpr
                                                         compat
                                                         order
                              ) 


renderOrderExpr :: CompatMode -> OrderExpr -> Doc
renderOrderExpr compat (OE ee dir) =
    renderExtendedExpr compat ee
    <+> renderSortDirection dir

renderWindowOrderExpr :: CompatMode -> WindowOrderExpr -> Doc
renderWindowOrderExpr compat (WOE ae dir) =
    renderAggrExpr compat ae
    <+> renderSortDirection dir

-- | Render a list of generic order expressions.
renderGenericOrderByList :: (CompatMode -> o -> Doc) -> CompatMode -> [o] -> Doc
renderGenericOrderByList renderGenericOrderExpr compat =
    enlistOnLine . map (renderGenericOrderExpr compat)

renderWindowOrderByList :: CompatMode -> [WindowOrderExpr] -> Doc
renderWindowOrderByList compat wos =
    kw "ORDER BY" <+> renderGenericOrderByList renderWindowOrderExpr compat wos

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

renderFromPart compat (FPAlias expr alias optCols)      =
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
renderSelectColumn compat (SCAlias expr name) = renderExtendedExpr compat expr
                                                <+> kw "AS"
                                                <+> text name

renderExtendedExpr :: CompatMode -> ExtendedExpr -> Doc
renderExtendedExpr compat (EEBase v)                  = renderExtendedExprBase compat v
renderExtendedExpr compat (EERowNum partColumn order) =
    kw "ROW_NUMBER() OVER"
    <+> parens ( partitionByDoc 
                 <>
                 case order of
                     [] -> empty
                     _  -> renderWindowOrderByList compat order
               )
  where
    partitionByDoc = maybe empty
                           ( \c -> kw "PARTITION BY"
                                   </> renderAggrExpr compat c
                                   <> linebreak
                           ) 
                           partColumn

renderExtendedExpr compat (EEDenseRank order)         =
    renderRank compat "DENSE_RANK() OVER" order
renderExtendedExpr compat (EERank order)              =
    renderRank compat "RANK() OVER" order

renderExtendedExpr compat (EEAggrExpr ae)             =
    renderAggrExpr compat ae

renderAggrExpr :: CompatMode -> AggrExpr -> Doc
renderAggrExpr compat e = case e of
    AEBase ve              ->
        renderValueExprTemplate renderAggrExpr compat ve

    AEAggregate optVE aggr -> 
        renderAggregateFunction compat aggr
        <> parens (maybe (char '*') (renderColumnExpr compat) optVE)

-- | Generic 'ValueExprTemplate' renderer.
renderValueExprTemplate :: (CompatMode -> a -> Doc)
                        -> CompatMode
                        -> ValueExprTemplate a
                        -> Doc
renderValueExprTemplate renderRec compat ve = case ve of
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
    VECase c t e         ->
        text "CASE WHEN" <+> renderRec compat c
                         <+> text "THEN"
                         <+> renderRec compat t
                         <+> text "ELSE"
                         <+> renderRec compat e
                         <+> text "END"

-- | Render a 'ExtendedExprBase' with the generic renderer.
renderExtendedExprBase :: CompatMode -> ExtendedExprBase -> Doc
renderExtendedExprBase = renderValueExprTemplate renderExtendedExpr
    

-- | Render a 'ColumnExprBase' with the generic renderer.
renderColumnExprBase :: CompatMode -> ColumnExprBase -> Doc
renderColumnExprBase = renderValueExprTemplate renderColumnExpr


-- | Render the postfix part of a rank operator.
renderRank :: CompatMode -> String -> [WindowOrderExpr] -> Doc
renderRank compat prefix order =
    kw prefix <+> parens (renderWindowOrderByList compat order)

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

