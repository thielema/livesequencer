module Program where

import IO
import Term ( Identifier (..) )
import Rule
import Module

import Text.Parsec
import Text.Parsec.Token
import Text.PrettyPrint.HughesPJ

import qualified Data.Map as M
import Control.Monad ( foldM )

data Program = Program { modules :: M.Map Identifier Module }

rules :: Program -> [ Rule ]
rules p = concat $ map Module.rules $ M.elems $ modules p

-- | load from disk, with import chasing
chase :: Identifier -> IO Program
chase n = chaser ( Program { modules = M.empty } ) n

chaser :: Program -> Identifier -> IO Program
chaser p n = 
    case M.lookup n ( modules p ) of
        Just m -> do
            -- already loaded
            return p
        Nothing -> do    
            let f = map ( \ c -> if c == '.' then '/' else c ) ( Term.name n )
                  ++ ".hs"  
            s <- readFile f    
            case parse input f s of 
                Left err -> error $ show err
                Right m  -> do
                    foldM chaser ( p { modules = M.insert n m $ modules p } ) 
                          $ map source $ imports m
