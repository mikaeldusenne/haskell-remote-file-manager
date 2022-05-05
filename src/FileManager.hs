{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module FileManager where

import Hunix hiding (listFiles)
import qualified Hunix as H
import Control.Monad.IO.Class (liftIO)
import Types as T
import System.FilePath.Posix
import System.FilePath
import List
import Params
import Misc as MS
import Trees
import Control.Lens
import Data.List

-- replace with normalize
securePath :: String -> String
securePath ee
  | beginWith "/" ee || (any (beginWith "..") $ splitPath ee) = ""
  | otherwise = ee



listFiles :: String -> IO [FileDetail]
listFiles path = f $ securePath path
  where f p = do
          print $ "listing directory <" <> (directory </> p) <> ">" 
          l <- (H.listDir' $ directory </> p) >>= mapM
            (\e -> do
                sz <- fsize (directory </> p </> H.path e)
                -- return $ (size .~ sz) e
                return $ FileDetail{ _path = H.path e, _filetype = H.filetype e,
                                     _size = sz}
                )
          return $ if p /= ""
                   then FileDetail{_path="..", _filetype=Dir, _size=0} : l
                   else l

-- parentDir e (Leaf l) = Nothing
-- parentDir e n@(Node _ l) = if any (==e) l
--                            then Just n
--                            else ( (>0) . length (!∫) ) . filter (/=Nothing) .
--                                 traverse parentDir $ l
--   where f l = 

-- listDirWithPrev pp = (if pp /= ""
--                       then (Path ".." Dir :)
--                       else id ) <$> H.listDir True pp


listTree :: Path -> IO (Tree Path)
listTree p@(Path{H.path=path, H.filetype=t}) = case t of
  Dir -> do
    l <- sort <$> listDir True path >>= mapM listTree
    
    return $ Node p' l
  File -> return $ Leaf p'
  where p' = Path{H.path= path, H.filetype=t}



computeState :: Path -> IO FileDetail
computeState p@(Path{H.path=path, H.filetype=t}) = do
  sz <- fsize path
  return $ FileDetail { _size = sz, _path = path, _filetype = t }


findTreeByPath :: String -> Tree FileDetail -> Maybe (Tree FileDetail)
findTreeByPath s = f s'
  where s' = scanl (</>) (directory) . map (filter (/='/')) . splitPath $ s
        test :: String -> Tree FileDetail -> Maybe (Tree FileDetail)
        test a b = (a MS.?? _path (treeValue b)) b
        f :: [String] -> Tree FileDetail -> Maybe (Tree FileDetail)
        f [] _ = Nothing -- error $ "path not found <" ++ s ++ ">"
        f (x:xs) l@(Leaf e) = test x l
        f [x] n@(Node e l) = test x n
        f (x:xs) (Node e l) =  if _path e == x then (g $ map (f xs) l) else Nothing
          where g l = case filter (/=Nothing) l of
                  [] -> Nothing
                  [x] -> x
                  ll -> error $ "multiple corresponding paths " ++ show l 



-- folderChildren :: String -> Tree FileDetail -> [FileDetail]
-- folderChildren s =  just_or_default [] . f s'
--   where s' = splitPath s
--         test :: FileDetail -> String -> a -> Maybe a
--         test a b ans = if T.path a == b then Just ans else Nothing
--         f :: [String] -> Tree FileDetail -> Maybe [FileDetail]
--         f [] _ = Nothing -- error $ "path not found <" ++ s ++ ">"
--         f (x:xs) (Leaf e) = test e x []
--         f [x] (Node e l) = test e x (map treeValue l)
--         f (x:xs) (Node e l) =  if T.path e == x then (g $ map (f xs) l) else Nothing
--           where g l = case filter (/=Nothing) l of
--                   [] -> Nothing
--                   [x] -> x
--                   ll -> error $ "multiple corresponding paths " ++ show l 


-- listTreeSta :: IO (Tree FileDetail)
-- a = do
--   t <- listTree (Path directory Dir)
--   traverse computeState t

-- computeState :: IO StateApp
-- computeState = f (Path {H.path=directory, H.filetype=Dir})
--   where f :: Path -> IO StateApp
--         f (Path{H.path=p, H.filetype=t}) = do
--           spc <- freeSpace directory
--           sz  <- fsize p
--           let fl :: IO [FileType]
--               fl = do
--                 l' <- H.listDir p
--                 (allfiles <$> f) <*> l'
--           l   <-
--             if t == Dir
--             then fl
--             else return []
--           let fd = FileDetail {
--                 T.path = p,
--                 size = sz,
--                 T.filetype = t
--                 }
--           return $ if l == [] then Leaf fd else Node l fd
          


pcent :: Space -> Double
pcent (Space{used=u, avail=a}) = 100 * a' / (u'+a')
  where u' = fromInteger u
        a' = fromInteger a

free :: IO Space
free = freeSpace directory
