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
    QValueQuery q      -> renderValueQuery q
    QDefinitionQuery q -> renderDefinitionQuery c q

renderDefinitionQuery :: CompatMode -> DefinitionQuery -> Doc
renderDefinitionQuery _ (DQMatView query name)        =
    kw "CREATE MATERIALIZED VIEW"
    <+> text name
    <+> kw "AS"
    </> renderValueQuery query

renderDefinitionQuery c (DQTemporaryTable query name) =
    createStmt
    <+>
    case c of
        SQL99      ->
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
    indentedQuery = indent 4 $ renderValueQuery query

renderValueQuery :: ValueQuery -> Doc
renderValueQuery (VQSelect stmt)                         = renderSelectStmt stmt
renderValueQuery (VQLiteral vals)                        =
    kw "VALUES" <+> align (sep . punctuate comma $ map renderRow vals)
  where renderRow row = parens . enlistOnLine $ map renderValueExpr row

renderValueQuery (VQWith bindings body) =
    hang 4 (kw "WITH" </> enlist (map renderBinding bindings))
    <$> renderValueQuery body
  where renderBinding :: (String, Maybe [String], ValueQuery) -> Doc
        renderBinding (name, optCols, query) =
            text name
            <> renderOptColDefs optCols
            <+> kw "AS"
            <+> lparen
            <$> indent 4 (renderValueQuery query)
            <$> rparen

renderValueQuery (VQBinarySetOperation left right o)    =
    renderValueQuery left
    <> linebreak
    <$> renderSetOperation o
    <> linebreak
    <$> renderValueQuery right

renderSetOperation :: SetOperation -> Doc
renderSetOperation SOUnionAll  = kw "UNION ALL"
renderSetOperation SOExceptAll = kw "EXCEPT ALL"

renderSelectStmt :: SelectStmt -> Doc
renderSelectStmt stmt =
    kw "SELECT"
    <+> align ( let sC = enlist $ map renderSelectColumn $ selectClause stmt
                in if distinct stmt
                   then kw "DISTINCT" <+> sC
                   else sC
              )
               
    <> case fromClause stmt of
           []        -> empty
           fromParts ->
               linebreak
               <> kw "FROM"
               <+> align (enlist $ map renderFromPart fromParts)
    <> case whereClause stmt of
           Just valExpr -> linebreak
                           <> kw "WHERE"
                           <+> align (renderValueExpr valExpr)
           Nothing      -> empty
    <> case groupByClause stmt of
           []      -> empty
           valExpr -> linebreak
                      <> kw "GROUP BY"
                      <+> align (enlist $ map renderValueExpr valExpr)
    <> case orderByClause stmt of
           []    -> empty
           order -> linebreak
                    <> kw "ORDER BY"
                    <+> align (renderOrderExprList order)


renderOrderExpr :: OrderExpr -> Doc
renderOrderExpr (OE expr dir) = renderValueExpr expr <+> renderSortDirection dir

-- | Render a list of order expressions.
renderOrderExprList :: [OrderExpr] -> Doc
renderOrderExprList = enlistOnLine . map renderOrderExpr

renderSortDirection :: SortDirection -> Doc
renderSortDirection Ascending  = kw "ASC"
renderSortDirection Descending = kw "DESC"

-- | Render a list of columns as definition within a from clause or within
-- a common table expression.
renderOptColDefs :: Maybe [String] -> Doc
renderOptColDefs = maybe empty colDoc
  where colDoc = parens . enlistOnLine . map text

renderFromPart :: FromPart -> Doc
renderFromPart (FPAlias (FETableReference n) alias _) =
    -- Don't use positional mapping on table references, since they are mapped
    -- by their name.
    text n
    <+> kw "AS"
    <+> text alias

renderFromPart (FPAlias expr alias optCols)           =
    renderFromExpr expr
    <+> kw "AS"
    <+> text alias
    <> renderOptColDefs optCols

renderFromPart (FPInnerJoin left right cond) = renderFromPart left
                                               </> kw "INNER JOIN"
                                               </> renderFromPart right
                                               </> kw "ON"
                                               </> renderValueExpr cond

renderSubQuery :: ValueQuery -> Doc
renderSubQuery q = lparen <+> align (renderValueQuery q) <$> rparen

renderFromExpr :: FromExpr -> Doc
renderFromExpr (FESubQuery q)       = renderSubQuery q
renderFromExpr (FEVariable v)       = ondullblue $ int v
renderFromExpr (FETableReference n) = text n

-- | Renders an optional prefix.
renderOptPrefix :: Maybe String -> Doc
renderOptPrefix = maybe empty $ (<> char '.') . text


renderSelectColumn :: SelectColumn -> Doc
renderSelectColumn (SCAlias expr name)      = renderSelectExpr expr
                                              <+> kw "AS"
                                              <+> text name

renderSelectExpr :: SelectExpr -> Doc
renderSelectExpr (SEValueExpr v)             = renderValueExpr v
renderSelectExpr (SERowNum partColumn order) =
    kw "ROW_NUMBER() OVER"
    <+> parens (partitionByDoc 
                <>
                case order of
                    [] -> empty
                    _  -> kw "ORDER BY" <+> renderOrderExprList order)
  where partitionByDoc = maybe empty
                               (\c -> kw "PARTITION BY"
                                      </> renderValueExpr c
                                      <> linebreak)
                               partColumn

renderSelectExpr (SEDenseRank order)         = renderRank "DENSE_RANK() OVER"
                                                          order
renderSelectExpr (SERank order)              = renderRank "RANK() OVER" order

renderSelectExpr (SEAggregate optVE aggr)    =
    renderAggregateFunction aggr
    <> parens (maybe (char '*') renderValueExpr optVE)

-- | Render the postfix part of a rank operator.
renderRank :: String -> [OrderExpr] -> Doc
renderRank prefix order =
    kw prefix <+> parens (kw "ORDER BY" <+> renderOrderExprList order)

renderAggregateFunction :: AggregateFunction -> Doc
renderAggregateFunction AFAvg   = kw "AVG"
renderAggregateFunction AFMax   = kw "MAX"
renderAggregateFunction AFMin   = kw "MIN"
renderAggregateFunction AFSum   = kw "SUM"
renderAggregateFunction AFCount = kw "COUNT"
renderAggregateFunction AFAll   = kw "ALL" -- TODO
renderAggregateFunction AFProd  = kw "PROD" -- TODO does not exist in postgres
renderAggregateFunction AFDist  = kw "DIST" -- TODO - " -

renderValueExpr :: ValueExpr -> Doc
renderValueExpr (VEValue v)            = renderValue v
renderValueExpr (VEColumn n optPrefix) = renderOptPrefix optPrefix
                                         <> text n
renderValueExpr (VECast v ty)          = kw "CAST" <> parens castExpr
  where castExpr = renderValueExpr v <+> kw "AS" <+> renderDataType ty

renderValueExpr (VEBinApp f a b)       =
    parens $ renderValueExpr a
             <+> renderBinaryFunction f
             <+> renderValueExpr b

renderValueExpr (VENot a)              =
    parens $ kw "NOT" <+> renderValueExpr a
renderValueExpr (VEExists q)           = kw "EXISTS" <+> renderSubQuery q

renderValueExpr (VEIn v q)             =
    parens $ renderValueExpr v
             <+> kw "IN"
             <+> renderSubQuery q

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

