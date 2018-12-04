{
-- |
-- Module      : $Header$
-- Description : Parser for DFLang
-- Copyright   : (c) Justus Adam 2018. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : dev@justus.science
-- Stability   : experimental

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE OverloadedStrings, LambdaCase, TupleSections, TypeFamilies, FlexibleContexts #-}
module Ohua.DFLang.Parser
    ( parseExp
    ) where

import Ohua.Prelude

import qualified Data.Sequence as Seq

import Ohua.DFLang.Lexer
import Ohua.DFLang.Lang

import Prelude ((!!))

}


%name parseExpRaw Exp
%tokentype { Lexeme }
%error { parseError }
%lexer { lexer } { EOF }
%monad { PM }

%token

    id              { UnqualId $$ }
    qualid          { QualId $$ }
    nsid            { ModuleId $$ }
    int             { Int_ $$ }
    env_ref         { EnvRef $$ }

    let             { KWLet }
    in              { KWIn }
    dataflow        { KWDataflow }
    '('             { LParen }
    ')'             { RParen }
    '['             { LBracket }
    ']'             { RBracket }
    '<'             { LAngle }
    '>'             { RAngle }
    '='             { OPEq }
    ','             { OPComma }

%%
unit : '('')' {}

many1 (p)
    : p many1(p) { $1 : $2 }
    | p          { [$1] }

many (p)
    : many1(p)  { $1 }
    |           { [] }

many_sep1(p, sep)
    : p sep many_sep1(p, sep) { $1 : $3 }
    | p                       { [$1] }

many_sep(p, sep)
    : many_sep1(p, sep) { $1 }
    |                   { [] }

tuple(p)
    : '(' many_sep(p, ',') ')' { $2 }

opt(p)
    : p { Just $1 }
    |   { Nothing }

or(a, b)
    : a { Left $1 }
    | b { Right $1 }

ModId :: { NSRef }
ModId
    : id    { makeThrow [$1] :: NSRef }
    | nsid  { $1 }

Exp :: { DFExpr }
Exp : many(LetExpr) id { DFExpr (Seq.fromList $1) $2 }

FnId :: { FnId }
FnId : int { makeThrow $ fromInteger $1 }

LetExpr :: { LetExpr }
LetExpr : let Pat '=' FnRef '<' FnId '>' tuple(DFVar) opt(CtxRef) in { LetExpr $6 $2 $4 $8 $9 }

DFVar :: { DFVar }
DFVar
    : Lit { DFEnvVar $1 }
    | id      { DFVar $1 }

Lit :: { Lit }
    : int { NumericLit $1 }
    | env_ref { EnvRefLit $1 }
    | unit { UnitLit }

FnRef :: { DFFnRef }
FnRef : opt(dataflow) qualid { maybe EmbedSf (const DFFunction) $1 $2 }

CtxRef :: { Binding }
CtxRef : '[' id ']' { $2 }

Pat :: { Pat }
Pat : tuple(id) { $1 }

{

type Pat = [Binding]
type PM = Alex

nextToken :: PM Lexeme
nextToken = alexMonadScan

lexer :: (Lexeme -> PM a) -> PM a
lexer cont = nextToken >>= cont

runPM :: PM a -> Input -> a
runPM ac bs = either (error . toText) identity $ runAlex bs ac

parseError :: Lexeme -> PM a
parseError token = do
  (line, col) <- getLexerPos
  alexError $ ("Parse error at line " <> show line <> ", column " <> show col <> ", on token " <> show token :: String)

parseExp :: Input -> DFExpr
parseExp = runPM parseExpRaw

}
